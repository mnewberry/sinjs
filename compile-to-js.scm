;;; Compile Scheme to Javascript.
;;; With much thanks to Alex Yakovlev and Chris Double.

;;; Compile FORM and return the corresponding string.
(define (compile form env)
  (cond
   ;; syntactic literals
   ((number? form) (compile-numeric-literal form env))
   ((string? form) (compile-string-literal form env))
   ((char? form) (compile-character-literal form env))
   ((boolean? form) (compile-boolean-literal form env))
   
   ;; variable references
   ((symbol? form) (compile-variable-reference form env))

   ((pair? form)
    (case (car form)
      ((quote) (compile-literal form env))
      ((lambda) (compile-procedure form env))
      ((if) (compile-conditional form env))
      ((set!) (compile-assignment form env))
      (else (compile-call form env))))))

;;; for now we cheat with numbers; assume the scheme syntax
;;; is valid javascript and punt the rest.
(define (compile-numeric-literal form env)
  (number->string form))

;;; " -> \"
;;; \ -> \\
;;; ' -> \'
;;; and wrap in quotes.
(define (compile-string-literal form env)
  (string-append
   "\""
   (string-concatenate
    (map (lambda (c)
	   (cond
	    ((eqv? c #\") "\\\"")
	    ((eqv? c #\\) "\\\\")
	    ((eqv? c #\') "\\'")
	    (else (string c))))
	 (string->list form)))
   "\""))

(define (compile-character-literal form env)
  (string-append "(getChar(" (char->integer form) "))"))

(define (compile-boolean-literal form env)
  (if form "true" "false"))

(define (compile-variable-reference form env)
  (let ((binding (assq form env)))
    (if binding
	(cdr binding)
	(string-append "(TopEnv.get(" (symbol->string form) "))"))))

