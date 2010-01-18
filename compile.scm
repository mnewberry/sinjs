;;; Convert CPS code into compiled code.

(use (srfi 1))

(define (compile-form form)
  (define (compile-formals formals)
    (cond
     ((null? formals) "")
     ((null? (cdr formals)) (compile-identifier (car formals)))
     (else (string-append (compile-identifier (car formals)) "," 
			  (compile-formals (cdr formals))))))

  (define (compile-arguments arguments)
    (cond
     ((null? arguments) "")
     ((null? (cdr arguments)) (compile-1 (car arguments)))
     (else (string-append (compile-1 (car arguments)) ","
			  (compile-arguments (cdr arguments))))))

  (define (escaping-char->string c)
    (case c
      ((#\backspace) "\\b")
      ((#\page) "\\f")
      ((#\newline) "\\n")
      ((#\nul) "\\0")
      ((#\return) "\\r")
      ((#\tab) "\\t")
      ((#\vtab) "\\v")
      ((#\') "\\'")
      ((#\") "\\\"")
      ((#\\) "\\\\")
      (else (string c))))

  (define (escape-string s)
    (apply string-append (map escaping-char->string (string->list s))))

  (define (compile-literal datum)
    (cond
     ((number? datum) (number->string datum))
     ((eq? datum #t) "true")
     ((eq? datum #f) "false")
     ((null? datum) "theNil")
     ((char? datum)
      (string-append "intern_char('" (escaping-char->string datum) "')"))
     ((pair? datum)
      (string-append "new Pair(" (compile-literal (car datum)) ","
		     (compile-literal (cdr datum)) ")"))
     ((symbol? datum) (string-append "\"" 
				     (escape-string (symbol->string datum))
				     "\""))
     ((string? datum) 
      (string-append "new SchemeString (\"" (escape-string datum) "\")"))
     (else (error (format "unsupported literal ~s\n" datum)))))

  ;; if it's not already a legitimate variable name, make it one.
  (define (compile-identifier sym)
    (define (legit? c) 
      (or (char-alphabetic? c)
	  (char-numeric? c)
	  (char=? c #\_)))
    (let ((name (symbol->string sym)))
      (if (string-skip name legit?)
	  (string-append "x_" (string-filter legit? (symbol->string sym)))
	  name)))

  (define (compile-temp-decls vars)
    (if (null? vars)
	""
	(string-append "var " 
		       (symbol->string (car vars))
		       (apply string-append 
			      (map (lambda (var)
				     (string-append ", " var))
				   (cdr vars)))
		       ";\n")))

  (define (compile-1 form)
    (cond
   ;;; variables become JS variables [note, these are all locals]
     ((symbol? form) (compile-identifier form))

     ((pair? form)
      (case (car form)
	((set!)
	 (let ((variable (cadr form))
	       (value (caddr form)))
	   (string-append (compile-identifier variable) "=(" 
			  (compile-1 value) ")")))

	((quote)
	 (compile-literal (cadr form)))

	((foreign-inline)
	 (let ((code (cadr form))
	       (args (cddr form)))
	   (apply format code 
		  (map (lambda (arg)
			 (string-append "(" (compile-1 arg) ")"))
		       args))))

	((top-level-set!)
	 (let ((variable (cadr form))
	       (value (caddr form)))
	   (string-append "top_level_binding['" (symbol->string variable)
			  "']=(" (compile-1 value) ")")))

	((top-level-ref)
	 (let ((variable (cadr form)))
	   (string-append "top_level_binding['" 
			  (symbol->string variable) "']")))
	
	((if)
	 (let ((test (cadr form))
	       (true-branch (caddr form))
	       (false-branch (cadddr form)))
	   (string-append "(" (compile-1 test) ")!==false?(" 
			  (compile-1 true-branch) "):(" 
			  (compile-1 false-branch) ")")))

	((lambda)
	 (let ((formals (cadr form))
	       (expression (caddr form)))
	   (unless (null? (cdddr form))
	     (error "implicit begin not supported in code generation\n"))
	   (if (list? formals)
	       (string-append "function (" (compile-formals formals) 
			      "){return function () {"
			      "return " 
			      (compile-1 expression) ";};}")
	       (let ((rest-var (take-right formals 0))
		     (main-vars (drop-right formals 0)))
		 (string-append "function (" (compile-formals main-vars) "){"
				(compile-identifier rest-var)
				"=sinjs_restify(arguments,"
				(number->string (length main-vars))
				"); return function () {"
				"return " 
				(compile-1 expression)
				"};}")))))

	(else
	 (string-append "(" (compile-1 (car form)) ")(" 
			(compile-arguments (cdr form)) ")"))))
     (else (error (format "unknown expression ~s\n" form)))))
  
  (compile-1 form))

