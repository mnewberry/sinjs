;;; Simplify post-CPS code

(use (srfi 1))

;;; Codegen has a list of inlinable procedures, and here we create
;;; a new expression type which codegen uses to emit those calls.
(define inline-tag (cons 'inlinable 'tag))

;;; We can beta reduce these argument types
(define (reducable? form)
  (or (symbol? form)
      (and (pair? form)
	   (let ((head (car form)))
	     (or (memq head '(quote top-level-ref lambda))
		 (and (pair? head)
		      (eq? (car head) inline-tag)))))))

(define (simplify form unsafes)
  (define (inline-ok? name)
    (and (memq name inlinables)
	 (not (memq name unsafes))))

  (define (simplify-1 form)
    (cond
     ((symbol? form) form)
     ((pair? form)
      (case (car form)
	((quote) form)
	((top-level-ref) form)

	((set!) 
	 (let ((var (cadr form))
	       (val (caddr form)))
	   `(set! ,var ,(simplify-1 val))))

	((top-level-set!)
	 (let ((var (cadr form))
	       (val (caddr form)))
	   `(top-level-set! ,var ,(simplify-1 val))))

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
	    ;; Not really a procedure, but an (INLINE foo) special form
	    ((and (eq? procedure inline-tag))
	     form)

	    ;; If all the arguments to the procedure are reducable,
	    ;; then do a beta reduction
	    ((and (list? procedure)
		  (eq? (car procedure) 'lambda)
		  (list? (cadr procedure)) ;no rest parameter
		  (= (length (cadr procedure)) (length args))
		  (every reducable? args))
	     #;(display (format "\nreducing ~s\n with ~s\n"
			      (caddr procedure)
			      (map cons (cadr procedure) args)))
	     (beta-reduce (caddr procedure) 
			  (map cons (cadr procedure) args)))
	    
	    ;; If the procedure is inlinable mark that for code-gen
	    ((and (pair? procedure)
		  (eq? (car procedure) 'top-level-ref)
		  (inline-ok? (cadr procedure)))
	     ;;; ((top-level-ref +) k a b) => (k ((INLINE +) a b))
	     `(,(car args)
	       ((,inline-tag ,(cadr procedure)) ,@(cdr args))))

	    (else (map simplify-1 form)))))))))

  (let ((f (simplify-1 form)))
    (if (equal? f form)
	form
	(simplify f unsafes))))


(define (beta-reduce form mappings)
  (cond
   ((eq? form inline-tag) form)
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
