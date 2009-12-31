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
  (let* ((prepared-forms (map-values prepare forms))
	 (unsafes (find-modifications prepared-forms)))
    (string-append (sinjs-prologue)
		   (make-top-level-table
		    (map (cut top-level-compile <> unsafes) 
			 prepared-forms))
		   (sinjs-epilogue))))

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
(define (top-level-compile form unsafes)
  (let ((continuation 'top_level_return))
    (string-append 
     (compile-form (simplify (cps-transform form continuation) unsafes)))))

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
     
     "scheme_top_level_done()"
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
		    (find-modifications (cdr form))))))
   (else (error find-modifications (format "bad form ~a\n" form)))))
					
(define (sinjs-prologue)
  (read-all "runtime.js"))

(define (sinjs-epilogue) "")

