;;; Convert CPS code into compiled code.

(use (srfi 1))

;;; an inlinable is a procedure which codegen can compile into direct
;;; JS syntax.
;;; Simplification is responsible for inserting these:
;;;
;;; ((top-level-ref +) k arg1 arg2)
;;;   => (k ((INLINE +) arg1 arg2))
;;;
(define inlinables '(+ * - zero? cons car cdr))

(define (compile-form form)

  (define (compile-inlining procedure args)
    (case procedure
      ((+) (string-append "(" (compile-1 (car args)) 
			  ")+(" (compile-1 (cadr args)) ")"))
      ((*) (string-append "(" (compile-1 (car args)) 
			  ")*(" (compile-1 (cadr args)) ")"))
      ((-) (string-append "(" (compile-1 (car args))
			  ")-(" (compile-1 (cadr args)) ")"))
      ((zero?) (string-append "((" (compile-1 (car args)) ")===0)"))
      ((cons) (string-append "(new Pair(("
			     (compile-1 (car args))
			     "),("
			     (compile-1 (cadr args))
			     ")))"))
      ((car) (string-append "((" (compile-1 (car args)) ").car)"))
      ((cdr) (string-append "((" (compile-1 (car args)) ").cdr)"))
      (else (error compile-inlining "bad inline procedure spec"))))

  (define (compile-formals formals)
    (cond
     ((null? formals) "")
     ((null? (cdr formals)) (symbol->string (car formals)))
     (else (string-append (symbol->string (car formals)) "," 
			  (compile-formals (cdr formals))))))

  (define (compile-arguments arguments)
    (cond
     ((null? arguments) "")
     ((null? (cdr arguments)) (compile-1 (car arguments)))
     (else (string-append (compile-1 (car arguments)) ","
			  (compile-arguments (cdr arguments))))))

  (define (compile-literal datum)
    (cond
     ((number? datum) (number->string datum))
     ((eq? datum #t) "true")
     ((eq? datum #f) "false")
     ((null? datum) "theNil")
     ((pair? datum)
      (string-append "new Pair(" (compile-literal (car datum)) ","
		     (compile-literal (cdr datum)) ")"))
     ((symbol? datum) (string-append "\"" (symbol->string datum) "\""))
     (else (error (format "unsupported literal ~s\n" datum)))))


  (define (compile-1 form)
    (cond
   ;;; variables become JS variables [note, these are all locals]
     ((symbol? form) (symbol->string form))

     ((pair? form)
      (case (car form)
	((set!)
	 (let ((variable (cadr form))
	       (value (caddr form)))
	   (string-append (symbol->string variable) "=(" 
			  (compile-1 value) ")")))

	((quote)
	 (compile-literal (cadr form)))

	((top-level-set!)
	 (let ((variable (cadr form))
	       (value (caddr form)))
	   (string-append "top_level_binding['" (symbol->string variable)
			  "']=(" (compile-1 value) ")")))

	((top-level-ref)
	 (let ((variable (cadr form)))
	   (string-append "top_level_binding['" (symbol->string variable) "']")))
	
	((if)
	 (let ((test (cadr form))
	       (true-branch (caddr form))
	       (false-branch (cadddr form)))
	   (string-append "(" (compile-1 test) ")!=false?(" 
			  (compile-1 true-branch) "):(" 
			  (compile-1 false-branch) ")")))

	((lambda)
	 (let ((formals (cadr form))
	       (expression (caddr form)))
	   (unless (null? (cdddr form))
	     (error "implicit begin not supported in code generation\n"))
	   (if (list? formals)
	       (string-append "function (" (compile-formals formals) 
			      "){return " (compile-1 expression) ";}")
	       (let ((rest-var (take-right formals 0))
		     (main-vars (drop-right formals 0)))
		 (string-append "function (" (compile-formals main-vars) "){"
				(symbol->string rest-var)
				"=sinjs_restify(arguments,"
				(number->string (length main-vars))
				"); return " (compile-1 expression) ";}")))))

	(else
	 (cond
	  ;; inline tags are only allowed as CAR of combinations, and they
	  ;; are caught below for that case directly.
	  ((eq? (car form) inline-tag)
	   (error compile-form "inline procedure tag escaped"))

	  ((and (pair? (car form))
		(eq? (caar form) inline-tag))
	   (compile-inlining (cadar form) (cdr form)))

	  (else
	   (string-append "(" (compile-1 (car form)) ")(" 
			  (compile-arguments (cdr form)) ")"))))))
     (else (error (format "unknown expression ~s\n" form)))))

  (compile-1 form))


