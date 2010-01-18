;;; Integrate into FORM if possible.

;;; Input form has been through macro expansion and canonicalization,
;;; but neither it nor the values of any integrations have been
;;; through CPS transformation.
;;;
;;; Two basic rules necessary for correctness:
;;;   Rename all local variables when integrating a procedure;
;;;   Don't ever do integrations which would produce an infinite
;;;    recursive loop.
;;; The first rule is observed with the rename-integration procedure
;;; which generates a new lambda in place of the one in the integration
;;; for inclusion into the source.  The second is observed by carefully
;;; removing items from integrables when looping.
(define (perform-integrations form integrables)
  (cond
   ((symbol? form) form)
   ((pair? form)
    (case (car form)
      ((quote top-level-ref) form)

      ((foreign-inline)
       (let ((code (cadr form))
	     (args (cddr form)))
	 `(foreign-inline ,code
			  ,@(map (cut perform-integrations <> integrables) 
				 args))))
      
      ((set!)
       (let ((var (cadr form))
	     (val (caddr form)))
	 `(set! ,var ,(perform-integrations val integrables))))

      ;;; don't allow integration inside the variable's own
      ;;; definition
      ((top-level-set!)
       (let ((var (cadr form))
	     (val (caddr form)))
	 `(top-level-set! 
	   ,var
	   ,(perform-integrations val (drop-integrable var integrables)))))

      ((if) 
       `(if ,@(map (cut perform-integrations <> integrables) (cdr form))))

      ((begin)
       `(begin ,@(map (cut perform-integrations <> integrables) (cdr form))))

      ((lambda) 
       (let ((formals (cadr form))
	     (forms (cddr form)))
	 `(lambda ,formals ,@(map (cut perform-integrations <> integrables)
				  forms))))

      (else
       (let ((proc (car form))
	     (args (cdr form)))
	 (if (and (list? proc)
		  (eq? (car proc) 'top-level-ref))
	     (let ((integration (assq (cadr proc) integrables)))
	       (if integration
		   (let ((pruned-integrables (drop-integrable (car integration)
							      integrables)))
		     ;; don't allow integration of this name again inside
		     ;; the integration itself, to avoid infinite loops.
		     `(,(perform-integrations (rename-integration 
					       (cdr integration))
					      pruned-integrables)
		       ,@(map (cut perform-integrations <> integrables) args)))
		   (map (cut perform-integrations <> integrables) form)))
	     (map (cut perform-integrations <> integrables) form))))))

   (else (error perform-integrations "bad form"))))

;;; prune NAME from integrables
(define (drop-integrable name integrables)
  (remove (lambda (int)
	    (eq? (car int) name))
	  integrables))

;;; rename all local variables consistently inside FORM with new
;;; uniquified names.
(define (rename-integration form)
  (let ((renames '()))
    (let rename ((form form))
      (cond
       ((null? form) '())
       ((symbol? form) 
	(let ((a (assq form renames)))
	  (if a
	      (cdr a)
	      (let ((new-name (uniquify form)))
		(set! renames (cons (cons form new-name) renames))
		new-name))))
       ((pair? form)
	(case (car form)
	  ((quote top-level-ref) form)
	  ((set! if begin)
	   `(,(car form) ,@(map rename (cdr form))))
	  ((lambda)
	   (let ((formals (cadr form))
		 (body (cddr form)))
	     `(lambda ,(map-formals rename formals) ,@(map rename body))))
	  ((foreign-inline top-level-set!)
	   `(,(car form) ,(cadr form) ,@(map rename (cddr form))))
	  (else (map rename form))))
       (else (error rename-integration (format "bad form: ~s" form)))))))
