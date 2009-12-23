;;; Toplevel compilation handling

;;; Toplevel compilation requires special magic.  We use the special
;;; TOP-LEVEL-PREPARE function, which handles compilation of the
;;; various special top level forms.

;;; This is like map, but the values from each invocation are spliced
;;; together, and we promise to do the mapping in order.
(define (map-values proc lyst)
  (if (null? lyst)
      '()
      (call-with-values
	  (proc (car lyst))
	(lambda vals (append vals (map-values proc (cdr lyst)))))))

;;; compile top-level forms into a Javascript program.
(define (top-level-compile-forms forms)
  (define syntactic-env '())
  (define (extend-syntactic-env name transformer)
    (set! syntactic-env (cons (cons name transformer) syntactic-env)))

  (let* ((prepared-forms (map-values (cut top-level-prepare <> 
					  top-level-vals 
					  syntactic-env
					  extend-syntactic-env)
				     forms))
	 (unsafes (find-modifications prepared-forms)))
    (string-append sinjs-prologue
		   (apply string-append 
			  (map (cut top-level-compile <> unsafes) 
			       prepared-forms))
		   sinjs-epilogue)))

(define (top-level-prepare form syntactic-env extend-syntax)
  (let ((form (expand form top-level-vars syntactic-env)))
    ;; match special top-level thingies, but only if we aren't
    ;; creating a binding for them.  note carefully that we can't
    ;; go astray here on correct programs, because of the rules
    ;; at the end of R5RS section 5.3.  NB: R6RS is different.
    (if (and (pair? form))
	(case (car form)
	  ((begin)
	   (apply values
		  (map-in-order top-level-prepare (cdr form))))

	  ((define)
	   ;; turn into a set! as per R5RS 5.2.1.
	   (prepare `(set! ,(cadr form) ,(caddr form)) '()))

	  ((define-syntax)
	   (unless (and (list? form)
			(= 3 (length form))
			(symbol? (cadr form)))
	     (error define-syntax "bad define-syntax syntax"))
	   (extend-syntax (cadr form) (caddr form))
	   (values))
	  
	  (else
	   (prepare form '())))
	(prepare form '()))))

(define (top-level-compile form unsafes)
  (let ((continuation 'sinjs_bounce))
    (string-append 
     (compile (simplify (cps-transform form continuation)) unsafes)
     ";\n")))

;;; return a list of top-level variables bound by the specified forms
#;(define (find-bindings forms)
  (cond
   ((null? forms) '())
   ((not (pair? (car forms))) (find-bindings (cdr forms)))
   ((eq? (caar forms) 'begin) (append (find-bindings (cdar forms))
				      (find-bindings (cdr forms))))
   ((eq? (caar forms) 'define) (cons (cadar forms)
				     (find-bindings (cdr forms))))
   (else (find-bindings (cdr forms)))))

;;; works on a list of forms, or a single form, because
;;; of the rule for combinations.
(define (find-modifications form)
  (cond
   ((null? form) '())
   ((symbol? form) '())
   ((pair? form)
    (case (car form)
      ((quote top-level-ref) '())
      ((set!) (find-modifications (caddr form)))
      ((begin) (find-modifications (cdr form)))
      ((lambda) (find-modifications (cddr form)))
      ((top-level-set!) (cons (cadr form) ;the important one!
			      (find-modifications caddr form)))
      ((if) (append (find-modifications (cadr form))
		    (find-modifications (caddr form))
		    (find-modifications (cadddr form))))
      (else (append (find-modifications (car form))
		    (find-modifications (cdr form))))))))
					
(define sinjs-prologue
  (string-append
   "function sinjs_bounce (value){return value;};\n"
   "var scheme_top_level = new Object();\n"
   "scheme_top_level['+'] = function(k,a,b){return k(a+b);}\n"
   "scheme_top_level['*'] = function(k,a,b){return k(a*b);}\n"))

(define sinjs-epilogue "")

