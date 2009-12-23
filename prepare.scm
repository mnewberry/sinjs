;;; Prepare forms for CPS conversion.

;;; All macro expansions must have occurred, all internal definitions
;;; have been converted into letrec (and then macro expanded), and the
;;; syntax has been checked for correctness.  Only the following
;;; special forms are allowed: QUOTE SET! IF LAMBDA BEGIN

;;; In theory, BEGIN could be implemented as a macro thus:
;;; (define-syntax begin
;;;   (syntax-rules () 
;;;     ((_ form forms ...)
;;;      ((lambda () form forms ...)))))

;;; But in practice, this does not work:
;;;   1) internal definitions are allowed in lambda, but not begin;
;;;   2) top-level begin is a top-level binding context.
;;; So instead, we match begin here separately, once the top-level
;;; code has already happened.  
		
(define unique-id 0)
(define (uniquify name)
  (set! unique-id (+ unique-id 1))
  (string->symbol (string-append (symbol->string name)
				 (number->string unique-id))))

(define (prepare form env)
  (cond
   ((number? form) `(quote ,form))
   ((boolean? form) `(quote ,form))
   ((string? form) `(quote ,form))
   ((char? form) `(quote ,form))

   ((symbol? form) 
    (cond
     ((assq form env) => cdr)
     (else `(top-level-ref ,form))))
   
   ;; short circuit keyword match if keyword is bound locally
   ((and (pair? form)
	 (symbol? (car form))
	 (assq (car form) env))
    (map (cut prepare <> env) form))

   ((pair? form)
    (case (car form)
      ((quote) form)

      ((set!)
       (cond
	((assq (cadr form) env) => cdr)
	(else `(top-level-set! ,(cadr form) ,(caddr form)))))

      ((if)
       (if (= (length form) 3)
	   `(if ,(prepare (cadr form) env)
		,(prepare (caddr form) env)
		(quote oh-mickey-youre-so-fine-you-blow-my-mind-hey-mickey))
	   `(if ,(prepare (cadr form) env)
		,(prepare (caddr form) env)
		,(prepare (cadddr form) env))))
      
      ((begin)
       `((lambda () ,@(cdr form))))

      ((lambda)
       (let ((bindings (map cons (cadr form) (map (cut uniquify <>) 
						  (cadr form)))))
	 `(lambda ,(map-formals cdr bindings) 
	    ,@(map (cute prepare <> (append bindings env)) (cddr form)))))

      (else
       (map (cut prepare <> env) form))))))

(define (map-formals proc formals)
  (cond
   ((symbol? formals) (proc formals))
   ((null? formals) '())
   (else (cons (proc (car formals)) 
	       (map-formals proc (cdr formals))))))
