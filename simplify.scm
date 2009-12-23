;;; Simplify post-CPS code

(use (srfi 1))

;;; We can beta reduce these argument types
(define (reducable? form)
  (or (symbol? form)
      (and (pair? form)
	   (or (eq? (car form) 'quote)
	       (eq? (car form) 'top-level-ref)
	       (eq? (car form) 'lambda)))))

(define (simplify-1 form)
  (cond
   ((symbol? form) form)
   ((pair? form)
    (case (car form)
      ((quote) form)
      ((set!) form)
      ((top-level-set!) form)

      ((top-level-ref) form)

      ((if)
       (let ((test (cadr form))
	     (true-branch (caddr form))
	     (false-branch (cadddr form)))
	 `(if ,(simplify-1 test) 
	      ,(simplify-1 true-branch) 
	      ,(simplify-1 false-branch))))

      ((lambda) 
       (let ((args (cadr form))
	     (value (caddr form)))
	 `(lambda ,args ,(simplify-1 value))))

      (else
       (let ((procedure (car form))
	     (args (cdr form)))
	 (cond
	  ;; If all the arguments to the procedure are reducable,
	  ;; then do a beta reduction
	  ((and (list? procedure)
		(eq? (car procedure) 'lambda)
		(list? (cadr procedure)) ;no rest parameter
		(= (length (cadr procedure)) (length args))
		(every reducable? args))
	   (beta-reduce (caddr procedure) 
			(map cons (cadr procedure) args)))

	  (else (map simplify-1 form)))))))))

(define (simplify form)
  (let ((f (simplify-1 form)))
    (if (equal? f form)
	form
	(simplify f))))

(define (beta-reduce form mappings)
  (cond
   ((assq form mappings) => cdr)
   ((symbol? form) form)
   ((pair? form)
    (case (car form)
      ((quote) form)
      ((set!) (if (assq (cadr form) mappings)
		  (error "set! in beta reduction")
		  `(set! ,(cadr form) 
			 (beta-reduce (caddr form) mappings))))
      ((if) `(if ,(beta-reduce (cadr form) mappings)
		 ,(beta-reduce (caddr form) mappings)
		 ,(beta-reduce (cadddr form) mappings)))
      ((lambda)
       (let ((formals (cadr form))
	     (value (caddr form)))
	 ;; note that this depends on the uniquification of variables
	 `(lambda ,formals ,(beta-reduce value mappings))))

      (else
       (map (cut beta-reduce <> mappings) form))))))
