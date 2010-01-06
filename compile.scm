;;; Convert CPS code into compiled code.

(use (srfi 1))


;;; an inlinable is a procedure which codegen can compile into direct
;;; JS syntax.
;;; Simplification is responsible for inserting these:
;;;
;;; ((top-level-ref +) k arg1 arg2)
;;;   => (k ((INLINE +) arg1 arg2))
;;;
;;; Each inlinable is specified in this table:
;;; NAME COMPLEX? NARGS MAXARGS FORMAT
;;;   NAME is a symbol naming the procedure
;;;   NARGS is the minimum number of args expected
;;;   FORMAT is a format string to compile the inline with ~a for the
;;;     compiled arguments, or a lambda
(define inlinables
  (let ()
    ;; OP must have a single ~a in it
    (define (unnumop op)
      (lambda (z) (format (string-append "check_number(~a) && " op) z z)))
    (define (unintop op)
      (lambda (z) (format (string-append "check_integer(~a) && " op) z z)))
    ;; must have two ~a's.
    (define (binumop op)
      (lambda (z1 z2)
	(format (string-append "check_number(~a) && check_number(~a)&&" op)
		z1 z2 z1 z2)))
    (define (binintop op)
      (lambda (z1 z2)
	(format (string-append "check_integer(~a) && check_integer(~a)&&" op)
		z1 z2 z1 z2)))

    (define (char-tester op)
      (lambda (c1 c2)
	(format (string-append 
		 "check_char(~a)&&check_char(~a)&&(~a).val.charCodeAt(0)" 
		 op "(~a).val.charCodeAt(0)")
		c1 c2 c1 c2)))

    (define (binumop-simple op)
      (binumop (string-append "((~a)" op "(~a))")))

    '((eq? 2 "((~a)===(~a))")
      (eqv? 2 "((~a)===(~a))")

      (number? 1 "typeof(~a) === 'number'")
      (complex? 1 "typeof(~a) === 'number'")
      (real? 1 "typeof(~a) === 'number'")
      (rational? 1 "typeof(~a) === 'number'")
      (integer? 1 ,(lambda (arg) (format "typeof(~a)==='number' && (Math.floor(~a)===(~a))"
					 arg arg)))
      (exact? 1 "false")
      (inexact? 1 "true")
      (= 2 ,(binumop-simple "==="))
      (< 2 ,(binumop-simple "<"))
      (> 2 ,(binumop-simple ">"))
      (<= 2 ,(binumop-simple "<="))
      (>= 2 ,(binumop-simple ">="))
      (zero? 1 ,(unnumop "(~a)===0"))
      (positive? 1 ,(unnumop "(~a)>0"))
      (negative? 1 ,(unnumop "(~a)<0"))
      (odd? 1 ,(unintop "(~a)%2!==0"))
      (even? 1 ,(unintop "(~a)%2===0"))
      (+ 2 ,(binumop-simple "+"))
      (* 2 ,(binumop-simple "*"))
      (- 1 ,(unnumop "-(~a)"))
      (- 2 ,(binumop-simple "-"))
      (/ 1 ,(unnumop "1/(~a)"))
      (/ 2 ,(binumop-simple "/"))
      (abs 1 ,(unnumop "Math.abs(~a)")))
    (remainder 2 ,(binintop "(~a)%(~a)"))
    (floor 1 ,(unnumop "Math.floor(~a)"))
    (ceiling 1 ,(unnumop "Math.ceil(~a)"))
    (exp 1 ,(unnumop "Math.exp(~a)"))
    (log 1 ,(unnumop "Math.log(~a)"))
    (sin 1 ,(unnumop "Math.sin(~a)"))
    (cos 1 ,(unnumop "Math.cos(~a)"))
    (tan 1 ,(unnumop "Math.tan(~a)"))
    (asin 1 ,(unnumop "Math.asin(~a)"))
    (acos 1 ,(unnumop "Math.acos(~a)"))
    (atan 1 ,(unnumop "Math.atan(~a)"))
    (sqrt 1 ,(unnumop "Math.sqrt(~a)"))
    (expt 2 ,(binumop "Math.pow(~a,~a)"))
    (exact->inexact 1 ,(unnumop "~a"))
    (inexact->exact 1 ,(unnumop "~a"))

    (not 1 "(~a)===false")
    (boolean? 1 "typeof(~a)==='boolean'")

    (pair? 1 "(~a).constructor===Pair")
    (cons 2 "new Pair(~a,~a)")
    (car 1 ,(lambda (p) (format "check_pair(~a)&&(~a).car" p p))
	 (cdr 1 ,(lambda (p) (format "check_pair(~a)&&(~a).cdr" p p)))
	 (caar 1 
	       ,(lambda (p) 
		  (format "check_pair(~a)&&check_pair(~a.car)&&(~a).car.car" p p p)))
	 (cadr 1
	       ,(lambda (p) 
		  (format "check_pair(~a)&&check_pair(~a.cdr)&&(~a).cdr.car" p p p)))
	 (cdar 1
	       ,(lambda (p) 
		  (format "check_pair(~a)&&check_pair(~a.car)&&(~a).car.cdr" p p p)))
	 (cddr 1
	       ,(lambda (p) 
		  (format "check_pair(~a)&&check_pair(~a.cdr)&&(~a).cdr.cdr" p p p)))
	 (null? 1 "(~a)===theNIL")
	 
	 (symbol? 1 "typeof(~a)==='string'")
	 (symbol->string 1 ,(lambda (s)
			      (format "check_symbol(~a)&&new SchemeString(~a)" s s)))
	 (string->symbol 1 ,(lambda (s)
			      (format "check_string(~a)&&(~a).val" s s)))
	 
	 (char? 1 "(~a).constructor===SchemeChar")
	 (char=? 2 ,(char-tester "==="))
	 (char<? 2 ,(char-tester "<"))
	 (char>? 2 ,(char-tester ">"))
	 (char<=? 2 ,(char-tester "<="))
	 (char>=? 2 ,(char-tester ">="))
	 (char->integer 1 ,(lambda (c)
			     (format "check_char(~a)&&(~a).val.charCodeAt(0)" c c)))
	 (integer->char 1 
			,(lambda (n)
			   (format
			    "check_integer(~a)&&intern_char(String.fromCharCode(~a))"
			    n n)))
	 
	 (string? 1 "(~a).constructor===SchemeString")
	 (string-length 1 ,(lambda (s)
			     (format "check_string(~a)&&(~a).val.length" s s)))
	 (string-ref 2 ,(lambda (s n)
			  (format "check_string_and_len((~a),(~a))&&intern_char((~a).val.charAt(~a))" s n s n)))
	 (string-set! 2 ,(lambda (s n c)
			   (format "check_string_and_len((~a),(~a))&&check_char(~a)&&(~a).val=(~a).val.slice(0,(~a))+(~a).val+(~a).val.slice((~a)+1)&&\"string-set! undefined-value\"")
			   s n c s s n c s n))
	 (string=? 2 ,(lambda (s1 s2)
			(format "check_string(~a)&&check_string(~a)&&(~a).val===(~a).val"
				s1 s2 s1 s2)))
	 (substring 3 ,(lambda (s start end)
			 (format "check_integer(~a)&&check_string_and_len((~a),(~a))&&new SchemeString((~a).val.substring((~a),(~a)))"
				 start s end s start end)))
	 (string-copy 1 ,(lambda (s)
			   (format "check_string(~a)&&new SchemeString((~a).val)"
				   s s)))

	 (vector? 1 "(~a).constructor===Array")
	 (vector-length 1 ,(lambda (v)
			     (format "check_vector(~a)&&(~a).length")))
	 (vector-ref 2 ,(lambda (v n)
			  (format "check_vector_and_len((~a),(~a))&&(~a)[~a]"
				  v n v n)))
	 (vector-set! 3 ,(lambda (v n obj)
			   (format "check_vector_and_len((~a),(~a))&&(~a)[~a]=(~a)&&\"vector-set! undefined value"
				   v n v n obj)))
	 
	 (procedure? 1 "typeof(~a)==='function'")

	 (input-port? 1 "(~a).constructor===SchemeInputPort")
	 (output-port? 1 "(~a).constructor===SchemeOutputPort")
	 (current-input-port 0 "sinjs_current_input_port")
	 (current-output-port 0 "sinjs_current_output_port")
	 (eof-object? 1 "(~a)===theEOF"))))

(define (compile-form form)

  (define (compile-inlining procedure args)
    (case procedure
       
      (else (error compile-inlining "bad inline procedure spec"))))

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
     ((symbol? datum) (string-append "\"" (symbol->string datum) "\""))
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
			      "){return function () {return " (compile-1 expression) ";};}")
	       (let ((rest-var (take-right formals 0))
		     (main-vars (drop-right formals 0)))
		 (string-append "function (" (compile-formals main-vars) "){"
				(compile-identifier rest-var)
				"=sinjs_restify(arguments,"
				(number->string (length main-vars))
				"); return function () {return " (compile-1 expression) "};}")))))

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

