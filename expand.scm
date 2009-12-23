;;; Syntax expander for sinjs.  

;;; We canonicalize lots of things at the same time since we're walking
;;; the tree and keeping track of bindings.

;;; All literals get embedded in QUOTE forms.
;;; All variable names become globally uniquified.
;;; IF expressions are forced to be double-branched.
;;; Top-level variable references and sets are turned into
;;;   TOP-LEVEL-REF and TOP-LEVEL-SET! forms.
;;; Internal definitions are eliminated.
;;;
;;; Only the following special forms remain at the end:
;;;   QUOTE SET! IF LAMBDA TOP-LEVEL-REF TOP-LEVEL-SET! BEGIN
;;; In addition, DEFINE remains if it is in a context not matched
;;; by the internal definition handler.  Those which are top-level
;;; will be handled specially by the top-level code, and those which
;;; are not will generate errors in CPS transformation.
;;; 
;;; Macros expand sometimes into binding forms, which are equivalent to
;;; variable references or set! except that they mandate a particular
;;; uniquification (perhaps to something already around) in the event
;;; they show up free.  If they get bound, they will get renamed again.
;;; These look like:
;;; (***binding name boundname)
;;; (***top-lev-binding name)
;;; and can occur anywhere an identifier is allowed.

(define unique-id 0)
(define (uniquify name)
  (set! unique-id (+ unique-id 1))
  (string->symbol (string-append (symbol->string name)
				 (number->string unique-id))))

;;; unique objects that cannot occur in the user's code.  these
;;; are treated like identifiers, but specify a special binding
;;; treatment.  they are inserted in macro expansion for identifiers
;;; which are introduced by the macro.
;;; (***local-binding name rename) [#f rename means top level]
(define ***special-binding (cons #f #f))

;;; tell whether a form should be treated as an identifier
(define (identifier? form)
  (or (symbol? form)
      (and (pair? form)
	   (eq? (car form) ***special-binding))))

;;; turn an identifier into a name. it must pass identifier?.
(define (identifier->name form)
  (if (symbol? form)
      form
      (cadr form)))

;;; report the renaming (or #f for top level) for an identifier
(define (identifier-rename form env)
  (if (symbol? form)
      

;;; An environment ENV is an alist; each symbol maps to either
;;; a symbol (for a variable) giving its canonical name, or to
;;; a pair (for a macro) thus 
;;; ((syntax-rules ...) . env) where ENV
;;; is the environment at the macro definition's location.

(define (expand form env)
  ((number? form) `(quote ,form))
  ((boolean? form) `(quote ,form))
  ((string? form) `(quote ,form))
  ((char? form) `(quote ,form))
  ((symbol? form)
   (cond
    ((alist-ref form env) => (lambda (bound)
			       (unless (symbol? bound)
				 (error expand "illegal use of syntax keyword"))
			       bound))
    (else `(top-level-ref ,form))))

  ((pair? form)
   (let ((binding (and (symbol? (car form))
		       (assq (car form) env))))
     (cond
      ((and (pair? binding) (eq? (caddr binding) 'syntax-rules))
       (expand (expand-syntax-rules form (cadr binding) (cddr binding) env)))

      ;;; mandated binding from macro expander.
      ((eq? (car form) ***binding)
       (caddr form))

      ;;; mandated top-level reference from macro expander.
      ((eq? (car form) ***top-lev-binding)
       `(top-level-ref ,(cadr form)))

      ((and (symbol? (car form)) (not binding))
       (case (car form)
	 ((quote) form)
	 ((set!) 
	  (let ((var (cadr form))
		(value (expand (caddr form) env)))
	    (cond
	     ((assq var env) => (lambda (p)
				  `(set! ,(cdr p) ,value)))
	     ((and (pair? var)
		   (eq? (car var) ***binding))
	      `(set! ,(caddr var) ,value))
	     ((and (pair? var)
		   (eq? (car var) ***top-lev-binding))
	      `(top-level-set! ,(cadr var) ,value))
	     (else `(top-level-set! ,var ,value)))))

	 ((if) 
	  (if (= (length form) 3)
	      `(if (expand (cadr form) env)
		   (expand (caddr form) env)
		   '(quote oh-mickey-youre-so-fine-you-blow-my-mind-hey-mickey))
	      `(if (expand (cadr form) env)
		   (expand (caddr form) env)
		   (expand (cadddr form) env))))

	 ((begin)
	  `(begin ,@(map (cut expand <> env) (cdr form))))

	 ((lambda)
	  (let ((bindings (map cons (cadr form) (map (cut uniquify <>)
						     (cadr form)))))
	    `(lambda ,(map-formals cdr bindings)
	       ,@(map (cute expand <> (append bindings env))
		      (cddr form)))))

	 ((let-syntax)
	  (let ((binding-list (cadr form))
		(body (cddr form)))
	    (expand `(begin ,@body)
		    (append 
		     (map (lambda (name transformer)
			    (unless (symbol? name)
			      (error expand "bad let-syntax syntax"))
			    (unless (and (pair? transformer)
					 (eq? (car transformer)
					      'syntax-rules))
			      (error expand "bad let-syntax transformer"))
			    `(,name ,transformer . ,env))
			  binding-list)
		     env))))

	 ((letrec-syntax)
	  (let* ((binding-list (cadr form))
		 (body (cddr form))
		 (env-add (map (lambda (name transformer)
				 (unless (symbol? name)
				   (error expand "bad letrec-syntax syntax"))
				 (unless (and (pair? transformer)
					      (eq? (car transformer)
						   'syntax-rules))
				   (error expand 
					  "bad letrec-syntax transformer"))
				 `(,name ,transformer . #f))))
		 (new-env (append env-add env)))
	    (for-each
	     (lambda (env-element)
	       (set-cdr! (cdr env-element) new-env)))
	    (expand `(begin ,@body) new-env)))

	 (else
	  (map (cut expand <> env) form))))

      (else
       (map (cut expand <> env) form))))))

(define (map-formals proc formals)
  (cond
   ((symbol? formals) (proc formals))
   ((null? formals) '())
   (else (cons (proc (car formals)) 
	       (map-formals proc (cdr formals))))))

;;; Expand FORM according to the specified syntax-rules TRANSFORMER.
;;; DEF-ENV is the environment in which the transformer was specified;
;;; and USE-ENV is the environment in which FORM was found.
(define (expand-syntax-rules form transformer def-env use-env)
  (if (symbol? (cadr transformer))
      (expand-syntax-rules1 form (cadr transformer) (caddr transformer)
			    (cdddr transformer) def-env use-env)
      (expand-syntax-rules1 form '... (cadr transformer)
			    (cddr transformer) def-env use-env)))

(define (expand-syntax-rules1 form ellipsis literals rules def-env use-env)

  ;; return a match list if FORM matches PATTERN, and #f otherwise.
  ;; A match list is an alist.
  ;; If the key is a symbol, the value is the matching form.
  ;; If the key is a pair (foo) it's an ellipsed foo with one set of 
  ;; parens for each ellipsis after foo in the pattern.
  (define (match-syntax form pattern)
    (cond
     ((symbol? pattern)
      (if (memq pattern literals)
	  (and (eq (assq pattern def-env)
		   (assq form use-env))
	       '())
	  (cons pattern form)))
   
     ((null? pattern)
      (and (null? form)
	   '()))

     ((pair? pattern)
      (if (and (pair? (cdr pattern))
	       (eq? ellipsis (cadr pattern)))
	  (begin
	    (unless (null? (caddr pattern))
	      (error match-syntax "non-final ellipsis"))
	    (and (list? form)
		 (assemble-ellipsed (map (cut match-syntax <> (car pattern)) 
					 form))))
	  (let ((match-car (match-syntax (car form) (car pattern))))
	    (and match-car
		 (append match-car
			 (match-syntax (cdr form) (cdr pattern)))))))
     (else
      (error match-syntax "unsupported pattern"))))

  (define renamings '())

  ;; expand the syntax template TEMPLATE in accord with the matched
  ;; PAIRING match alist.
  (define (expand-syntax template pairing)
    (cond
     ((symbol? template)
      (cond
       ((assq template pairing) => cdr)
       ;; A literal identifier is being inserted into the expansion.
       ;; Here we must be very careful with hygiene.  If the identifier
       ;; exists in the definition environment of the syntax, then
       ;; we insert that binding.  See above that BINDING is a special
       ;; form we tolerate from the macro expander.
       ((assq template def-env) =>
	(lambda (binding) `(,***binding ,(cdr binding))))

       ;; It's a top-level identifier at macro-definition place
       (else `(,***top-lev-binding ,template))))

       


  (unless (symbol? ellipsis)
    (error expand-syntax-rules "bad ellipsis in syntax-rules"))
  (unless (list? literals)
    (error expand-syntax-rules "bad literals in syntax-rules"))
  (unless (list? rules)
    (error expand-syntax-rules "bad ruleset in syntax-rules"))
  
  (let next ((rules rules))
    (when (null? rules)
      (error expand-syntax-rules "syntax match failure"))
    (unless (and (list? (car rules))
		 (= (length (car rules)) 2))
      (error expand-syntax-rules "bad rule in syntax-rules"))
    (let ((pairing (match-syntax (cdr form) (cdaar rules))))
      (if pairing
	  (expand-syntax (cadar rules) pairing)
	  (next (cdr rules))))))

;;; matches is a list of match lists.  Splice them together.
(define (assemble-ellipsed matches)
  (map
   (lambda (key)
     (cons (list key) (map (cut alist-ref var <> equal?) matches)))
   (car matches)))



  