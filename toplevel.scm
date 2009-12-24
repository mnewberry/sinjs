;;; Toplevel compilation handling

;;; This is like map, but the values from each invocation are spliced
;;; together, and we promise to do the mapping in order.
(define (map-values proc lyst)
  (if (null? lyst)
      '()
      (call-with-values
	  (lambda () (proc (car lyst)))
	(lambda vals (append vals (map-values proc (cdr lyst)))))))

;;; Note that we never stick variable bindings in the top-level-environment.
;;; This is used only to hold syntax definitions.
(define top-level-environment '())

;;; compile top-level forms into a Javascript program.
(define (top-level-compile-forms forms)
  (let* ((prepared-forms (map-values prepare forms))
	 (unsafes (find-modifications prepared-forms)))
    (string-append sinjs-prologue
		   (apply string-append 
			  (map (cut top-level-compile <> unsafes) 
			       prepared-forms))
		   sinjs-epilogue)))

;;; expand syntax first, check for top-level keywords, and then
;;; do a full expansion.
(define (prepare form)
  (let ((form (expand-first-syntax form top-level-environment)))
    (if (and (pair? form)
	     (identifier? (car form)))
	(case (identifier->name (car form))
	  ((begin)
	   (apply values (map-in-order prepare (cdr form))))

	  ((define)
	   ;; turn into a set! as per R5RS 5.2.1.
	   (let ((d (clean-define form)))
	     `(top-level-set! ,(cadr d) 
			      ,(expand (caddr d) top-level-environment))))

	  ((define-syntax)
	   (unless (and (list? form)
			(= 3 (length form))
			(symbol? (cadr form)))
	     (error prepare "bad define-syntax syntax"))
	   (set! top-level-environment
		 (cons (cons (cadr form) (cons (caddr form) #f))
		       top-level-environment))
	   (values))
	  
	  (else
	   (expand form top-level-environment)))
	(expand form top-level-environment))))

(define (top-level-compile form unsafes)
  (let ((continuation 'sinjs_bounce))
    (string-append 
     (compile-form (simplify (cps-transform form continuation) unsafes))
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
			      (find-modifications (caddr form))))
      ((if) (append (find-modifications (cadr form))
		    (find-modifications (caddr form))
		    (find-modifications (cadddr form))))
      (else (append (find-modifications (car form))
		    (find-modifications (cdr form))))))))
					
(define sinjs-prologue
  (string-append
   "function sinjs_bounce (value){return value;};\n"
   "var scheme_top_level = new Object();\n"
;   "scheme_top_level['+'] = function(k,a,b){return k(a+b);}\n"
;   "scheme_top_level['*'] = function(k,a,b){return k(a*b);}\n"
   ))

(define sinjs-epilogue "")

