;;; Toplevel compilation handling
;;;
;;; Top level forms are allowed to capture continuations just
;;; like any other forms.  In principle, this means we could 
;;; compile correctly with a giant letrec.  The consequence is that
;;; the user's program would be encapsulated into one giant JS
;;; expression.  That's perhaps not such a good idea.  So we handle
;;; top level forms specially.

;;; Each top level form gets compiled into a regular CPS procedure
;;; and then we generate a table "scheme_top_level_table" which lists each
;;; top level form.  Then the "scheme_top_level" JS function walks
;;; that table, calling each form with a continuation that proceeds to
;;; the next form in the table.  

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
  (let ((prepared-forms (map-values prepare forms)))
    (let-values (((global-set!s local-set!s)
		  (find-modifications prepared-forms)))
      (string-append (sinjs-prologue)
		     (make-top-level-table
		      (map (cut top-level-compile <> global-set!s local-set!s) 
			   prepared-forms))
		     (sinjs-epilogue)))))

;;; expand syntax first, check for top-level keywords, wrap
;;; in a procedure, and do a full expansion.
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
	     `(lambda () 
		(top-level-set! ,(identifier->name (cadr d))
				,(expand (caddr d) top-level-environment)))))

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
	   (expand `(lambda () ,form) top-level-environment)))
	(expand `(lambda () ,form) top-level-environment))))

;;; top_level_return is a JS function which just returns its
;;; argument.  Since FORM is a lamba expression, this means the
;;; return of TOP-LEVEL-COMPILE is a JS code fragment which 
;;; evaluates to a procedure.
#;(define (top-level-compile form unsafes)
  (let ((continuation 'top_level_return))
    (compile-form (simplify (cps-transform form continuation) unsafes))))

;;; while simplification does introduce variables, it never renames
;;; or introduces set! or top-level-set!, so the list of assignments
;;; from the preparation step is ok.
(define (top-level-compile form global-set!s local-set!s)
  (let* ((continuation 'top_level_return)
	 (transformed (cps-transform form continuation))
	 (simplified (simplify transformed global-set!s local-set!s))
	 (compiled (compile-form simplified)))
    #;(display (format "start form ~a\n" form))
    #;(display (format "after CPS ~a\n" transformed))
    #;(display (format "after simp ~a\n" simplified))
    compiled))

;;; Take a series of strings, each which evaluates to a JS function,
;;; and generate JS code that puts them in a table.
(define (make-top-level-table frags)
  (let ((tablevars (map (lambda (ignored) (uniquify 'toplev)) frags)))
    (string-append
     (apply string-append 
	    (map (lambda (var frag)
		   (string-append (symbol->string var) " = " frag ";\n"))
		 tablevars frags))
     "scheme_top_level_table = ["
     (apply string-append
	    (map (lambda (var) (string-append (symbol->string var) ",\n")) 
		 tablevars))
     
     "scheme_top_level_done"
     "];")))

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
;;; of the rule for combinations.  return two values:
;;; first, list of targets of top-level-set!, second, list
;;; of targets of set!.
(define (find-modifications form)
  (define (empty) (values '() '()))
  (cond
   ((null? form) (empty))
   ((symbol? form) (empty))
   ((pair? form)
    (case (car form)
      ((quote top-level-ref) (empty))
      ((set!) (let-values (((global local)
			    (find-modifications (caddr form))))
		(values global (cons (cadr form) local))))
      ((begin) (find-modifications (cdr form)))
      ((lambda) (find-modifications (cddr form)))
      ((top-level-set!) (let-values (((global local)
				      (find-modifications (caddr form))))
			  (values (cons (cadr form) global) local)))
      ((if) (let-values (((global1 local1) (find-modifications (cadr form)))
			 ((global2 local2) (find-modifications (caddr form)))
			 ((global3 local3) (find-modifications (cadddr form))))
	      (values (append global1 global2 global3)
		      (append local1 local2 local3))))
      (else (let-values (((global1 local1) (find-modifications (car form)))
			 ((global2 local2) (find-modifications (cdr form))))
	      (values (append global1 global2) (append local1 local2))))))
   (else (error find-modifications (format "bad form ~a\n" form)))))
					
(define (sinjs-prologue)
  (read-all "runtime.js"))

(define (sinjs-epilogue) "")
