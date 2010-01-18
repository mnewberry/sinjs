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
  (string-append (sinjs-prologue)
		 (make-top-level-table (compile-many forms))
		 (sinjs-epilogue)))

;;; Maintain a list of integrable procedures
(define *integrable-procedures* '())
(define (mark-integrable name lambda-expr)
  (set! *integrable-procedures*
	(cons (cons name lambda-expr) *integrable-procedures*)))

;;; expand syntax first, check for top-level keywords, wrap
;;; in a procedure, and do a full expansion.
(define (prepare form)
  (let ((form (expand-first-syntax form top-level-environment)))
    (if (and (pair? form)
	     (identifier? (car form)))
	(case (identifier->name (car form))
	  ((begin)
	   (apply values (map-values prepare (cdr form))))

	  ((define)
	   ;; turn into set! as per R5RS
	   (let* ((d (clean-define form))
		  (name (identifier->name (cadr d)))
		  (val (expand (caddr d) top-level-environment)))
	     (if (and (list? val)
		      (identifier? (car val))
		      (eq? (identifier->name (car val)) 'lambda))
		 (mark-integrable name val))
	     `(top-level-set! ,name ,(expand (caddr d) top-level-environment))))

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

(define (prune-integrables integrables global-set!s)
  (remove (lambda (integrable) 
	    (memq (car integrable) global-set!s))
	  integrables))

(define (simp a b c)
  (write a) (newline)
  (simplify a b c))
;;; compile a bunch of top-level forms into a list of JavaScript
;;; expressions.  
(define (compile-many forms)
  ;; First do everything but code-gen
  (let* ((orig-forms forms)
	 (forms (map-values prepare forms))
	 (foo (display "done preparing\n"))
	 (global-set!s (find-global-modifications forms))
	 (integrables (prune-integrables *integrable-procedures* global-set!s))
	 ;(forms (map (cut perform-integrations <> integrables) forms))
	 (foo (display "done integrating\n"))
	 (local-set!s (find-local-modifications forms))
	 (forms (map (cut cps-transform <> 'top_k) forms))
	 (foo (display "done transforming\n"))
	 (forms (map (cut simp <> global-set!s local-set!s) forms)))
    (display "done simplifying\n")
    ;; Find the references inside these forms
    (let-values (((mandatory-refs conditional-refs)
		  (find-references forms)))
      (let ((forms (prune-top-level forms orig-forms 
				    mandatory-refs conditional-refs)))
	(map compile-form forms)))))

;;; Take a series of strings of JS expressions, and generate JS code
;;; that puts them in a table of functions.
(define (make-top-level-table frags)
  (let ((tablevars (map (lambda (ignored) (uniquify 'toplev)) frags)))
    (string-append
     (apply string-append 
	    (map (lambda (var frag)
		   (string-append (symbol->string var) " = function(top_k){return function () {return " frag ";};};\n"))
		 tablevars frags))
     "scheme_top_level_table = ["
     (apply string-append
	    (map (lambda (var) (string-append (symbol->string var) ",\n")) 
		 tablevars))
     
     "scheme_top_level_done"
     "];\n")))


;;; find global modifications in a list of top-level forms.  if it's a
;;; a top-level-set!, it doesn't count as a modification if it
;;; occurs only once.
(define (find-global-modifications forms)
  (let ((globals-defined '()))
    (let find-all-modifications* ((forms forms))
      (if (null? forms)
	  '()
	  (let ((form (car forms)))
	    (if (and (list? form)
		     (eq? 'top-level-set! (car form))
		     (not (memq (cadr form) globals-defined)))
		(begin
		  (set! globals-defined (cons (cadr form) globals-defined))
		  (append (find-modifications (caddr form) 'top-level-set!)
			  (find-global-modifications (cdr forms))))
		(append (find-modifications form 'top-level-set!)
			(find-global-modifications (cdr forms)))))))))

(define (find-local-modifications forms)
  (find-modifications forms 'set!))

;;; works on a list of forms, or a single form, because
;;; of the rule for combinations.  setter is either set! or
;;; top-level-set!, depending on what kind of modifications we're
;;; looking for.
(define (find-modifications form setter)
  (cond
   ((null? form) '())
   ((symbol? form) '())
   ((pair? form)
    (case (car form)
      ((quote top-level-ref) '())
      ((set!) (if (eq? setter 'set!)
		  (cons (cadr form) (find-modifications (caddr form) setter))
		  (find-modifications (caddr form) setter)))
      ((begin) (find-modifications (cdr form) setter))
      ((lambda) (find-modifications (cddr form) setter))
      ((foreign-inline) (find-modifications (cddr form) setter))
      ((top-level-set!) (if (eq? setter 'top-level-set!)
			    (cons (cadr form) (find-modifications (caddr form)
								  setter))
			    (find-modifications (caddr form) setter)))
      ((if) (append (find-modifications (cadr form) setter)
		    (find-modifications (caddr form) setter)
		    (find-modifications (cadddr form) setter)))
      (else (append (find-modifications (car form) setter)
		    (find-modifications (cdr form) setter)))))
   (else (error find-modifications (format "bad form ~a\n" form)))))

;;; Find references inside a form or list of forms.  
;;; Return two values. First, a list of mandatory top-level variables.
;;; Second, an alist mapping top-level names to the top-level variables
;;; they conditionally require.
;;; A mandatory top-level reference is one which we must emit, because
;;; it is used by a top-level form directly.  A conditional reference
;;; is one for a definition we need to emit only if some other form
;;; is needed.  Thus, (define (x) ...) at top level doesn't need to be
;;; emitted unless some code somewhere has a reference to X.  If that 
;;; code is inside (define (y) (x ...)) then the reference is conditional
;;; upon Y.  Later walking this graph in prune-top-level will eliminate
;;; all the top-level-definitions we determine are unnecessary.
(define (find-references form)
  (cond
   ((null? form) (values '() '()))
   ((symbol? form) (values '() '()))
   ((pair? form)
    (case (car form)
      ((set!) (find-references (caddr form)))
      ((quote) (values '() '()))
      ((foreign-inline) (find-references (cddr form)))
      ((lambda) (find-references (cddr form)))
      ((if) (find-references (cdr form)))
      ((top-level-ref) (values (list (cadr form)) '())) ;mandatory
      ((top-level-set!)
       (let ((name (cadr form))
	     (val (caddr form)))
	 (if (and (list? val)
		  (eq? (car val) 'lambda))
	     (let-values (((mand opt) (find-references val)))
	       ;; all "mandatory" references inside this lambda
	       ;; are conditional on NAME
	       (values '() (merge-alists (list (cons name mand)) opt)))
	     (find-references val))))
      (else (let-values (((mand1 opt1) (find-references (car form)))
			 ((mand2 opt2) (find-references (cdr form))))
	      (values (append mand1 mand2) (merge-alists opt1 opt2))))))
   (else (error find-references "bad form"))))

;;; not very efficient, but clear
(define (merge-alists a1 a2)
  (define (ref sym a)
    (let ((p (assq sym a)))
      (if p (cdr p) '())))
  (if (null? a1)
      a2
      (let ((first-key (caar a1)))
	(cons (append (cons first-key (cdar a1)) (ref first-key a2))
	      (merge-alists (cdr a1) (remove (lambda (p) (eq? (car p) first-key))
					     a2))))))

;;; identify and remove forms which we don't need to emit.
(define (prune-top-level forms orig-forms mandatory conditional)
  (let ((mandatory (propogate-mandatory mandatory conditional)))
    (filter-map
     (lambda (form orig-form)
       ;; if it's (top-level-set! xxx yyy) then maybe prune
       (if (and (list? orig-form)
		(eq? (car orig-form) 'top-level-set!))
	   (and (memq (cadr orig-form) mandatory) form)
	   form))
     forms orig-forms)))

(define (propogate-mandatory mandatory conditional)
  ;; for each thing in MANDATORY, check and see if anything
  ;; is conditional on it, and add those things to mandatory,
  ;; and recurse.
  (let propogate ((mandatory mandatory))
    (let ((new-mands (lset-difference eq?
				      (apply append
					     (map (lambda (sym) 
						    (let ((p (assq sym conditional)))
						      (if p (cdr p) '())))
						  mandatory))
				      mandatory)))
      (if (null? new-mands)
	  mandatory
	  (propogate (append new-mands mandatory))))))

(define (sinjs-prologue)
  (string-append
   (read-all "runtime.js")
   (read-all "rhino.js")
   "rhino_initialize();\n"))

(define (sinjs-epilogue) 
  "scheme_top_level ();\n")
