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
(define unique-id 0)
(define (uniquify name)
  (set! unique-id (+ unique-id 1))
  (string->symbol (string-append (symbol->string name)
				 (number->string unique-id))))

;;; An environment ENV is an alist; each identifier maps to either
;;; a symbol (for a variable) giving its canonical name, or to
;;; a pair (for a macro) thus 
;;; ((syntax-rules ...) . env) where ENV
;;; is the environment at the macro definition's location.

;;; unique objects that cannot occur in the user's code.  these
;;; are treated like identifiers, but specify a special binding
;;; treatment.  they are inserted in macro expansion for identifiers
;;; which are introduced by the macro.
;;; (***special-binding name rename) [#f rename means top level]
;;; If FOO is free in some macro template, then occurrences of FOO
;;; become ***special-binding forms with RENAME copied from the
;;; environment in effect at macro definition time.  Moreover,
;;; all these ***special-binding forms must be eq? to eachother.
;;;
;;; RENAME is allowed to be either a symbol or a ((syntax-rules ...) . env) 
;;; structure.

;; any old unique value will do for this; we never look inside it.
(define ***special-binding (cons 'special 'binding)) 

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
  (cond
   ((assq form env) => cdr)
   ((and (pair? form) (eq? (car form) ***special-binding)) (cadr form))
   (else #f)))

;;; here is the basic syntax walker
(define (expand form env)
  ;;; this is what to do if we recognize a form as a combination.
  (define (combination) (map (cut expand <> env) form))

  ((number? form) `(quote ,form))
  ((boolean? form) `(quote ,form))
  ((string? form) `(quote ,form))
  ((char? form) `(quote ,form))

  ((identifier? form)
   (let ((val (identifier-rename form env)))
     (cond
      ((symbol? val) val)
      ((pair? val) (error expand "illegal use of syntax keyword"))
      ((not val) `(top-level-ref ,(identifier->name form))))))

  ;;; combination with nothing special
  ((and (pair? form)
	(not (identifier? (car form))))
   (combination))

  (else
   (let ((starter (identifier-rename (car form) env)))
     (cond
      ;; if starter is a pair, then this is a macro invocation.
      ;; expand it and recurse.
      ((pair? starter)
       (if (and (pair? (car starter))
		(eq? (caar starter) 'syntax-rules))
	   (expand (expand-syntax-rules form (car starter) (cdr starter) env) env)
	   (error expand "internal bad syntax transformer spec")))

      ;; if starter is not a pair, and not #f, then it is locally bound,
      ;; and therefore not a syntactic keyword.
      (starter (combination))

      ;; so we have a top-level binding. if it is a synactic keyword,
      ;; dtrt, otherwise, it's a combination.
      (case (identifier->name (car form))
	((quote) form)			;nothing to do [don't expand inside!]

	((set!)
	 (let ((var (cadr form))
	       (value (expand (caddr form) env)))
	   (unless (identifier? var)
	     (error expand "bad identifier in set!"))
	   (let ((r (identifier-rename var env)))
	     (cond
	      ((symbol? r) `(set! ,r ,value))
	      ((pair? r) (error expand "illegal use of syntax keyword"))
	      ((not val) `(top-level-set! ,(identifier->name var) ,value))))))

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

	;; each binding is a mapping from an identifier to
	;; new renamed thing.  We leave the identifiers alone,
	;; which guarantees that if we are here binding a
	;; variable inserted by a macro template, the renaming
	;; we stick in the environment refers only to the ID that
	;; was in the macro, and not other uses of the same name
	;; from the macro call's environment.
	((lambda)
	 (let ((bindings (map-formals (lambda (id)
					(cons id 
					      (uniquify (identifier->name id))))
				      (cadr form))))
	   `(lambda ,(map-formals cdr bindings)
	      ,@(map (cute expand <> (append bindings env))
		     (cddr form)))))

	((let-syntax)
	 (let ((binding-list (cadr form))
	       (body (cddr form)))
	   (expand `(begin ,@body)
		   (append 
		    (map (lambda (name transformer)
			   (unless (identifier? name)
			     (error expand "bad let-syntax syntax"))
			   (unless (and (pair? transformer)
					(eq? (car transformer)
					     'syntax-rules))
			     (error expand "bad let-syntax transformer"))
			   `(,name . (,transformer . ,env)))
			 binding-list)
		    env))))

	((letrec-syntax)
	 (let* ((binding-list (cadr form))
		(body (cddr form))
		(env-add (map (lambda (name transformer)
				(unless (identifier? name)
				  (error expand "bad letrec-syntax syntax"))
				(unless (and (pair? transformer)
					     (eq? (car transformer)
						  'syntax-rules))
				  (error expand 
					 "bad letrec-syntax transformer"))
				`(,name . (,transformer . #f)))))
		(new-env (append env-add env)))
	   (for-each
	    (lambda (env-element)
	      (set-cdr! (cdr env-element) new-env)))
	   (expand `(begin ,@body) new-env)))

	;; top level binding, but not to a syntactic keyword, so it's
	;; just a combination
	(else (combination)))))))

;;; map across a lambda list.
(define (map-formals proc formals)
  (cond
   ((identifier? formals) (proc formals))
   ((null? formals) '())
   ((pair? formals) (cons (proc (car formals)) 
			  (map-formals proc (cdr formals))))
   (else (error map-formals "bad lambda list"))))

;;; Note that everything in syntax transformers could be
;;; ***special-binding objects, if a macro expanded into a syntax binding
;;; construct.

;;; Expand FORM according to the specified syntax-rules TRANSFORMER.
;;; DEF-ENV is the environment in which the transformer was specified;
;;; and USE-ENV is the environment in which FORM was found.
(define (expand-syntax-rules form transformer def-env use-env)
  (if (identifier? (cadr transformer))
      (expand-syntax-rules1 form (cadr transformer) (caddr transformer)
			    (cdddr transformer) def-env use-env)
      (expand-syntax-rules1 form '... (cadr transformer)
			    (cddr transformer) def-env use-env)))

;;; used below to tag illegal pattern variables in ellipsis contexts
(define bad-object (cons 'bad 'object))

(define (expand-syntax-rules1 form ellipsis literals rules def-env use-env)

  ;; return a match list if FORM matches PATTERN, and #f otherwise.
  ;; A match list is an alist.
  ;; If the key is an identifier, the value is the matching form.
  ;; If the key is a pair (id) it's an ellipsed id with one set of 
  ;; parens for each ellipsis after foo in the pattern.
  (define (match-syntax form pattern)
    (cond
     ((identifier? pattern)
      (if (memq pattern literals)
	  (and (eq? (identifier-rename pattern def-env)
		    (identifier-rename form use-env))
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
	      (error match-syntax "non-final ellipsis in pattern"))
	    (and (list? form)
		 (assemble-ellipsed (map (cut match-syntax <> (car pattern)) 
					 form))))
	  (let ((match-car (match-syntax (car form) (car pattern))))
	    (and match-car
		 (append match-car
			 (match-syntax (cdr form) (cdr pattern)))))))
     (else
      (error match-syntax "unsupported pattern"))))

  ;; this works because toplevel EXPAND-SYNTAX is called at most once
  ;; inside an invocation of EXPAND-SYNTAX-RULES1.
  (define renamings '())

  ;; expand the syntax template TEMPLATE in accord with the matched
  ;; PAIRING match alist.
  (define (expand-syntax template pairing)
    (cond
     ((identifier? template)
      (cond
       ((assq template pairing) 
	=> (lambda (p)
	     (when (eq? (cdr p) bad-object)
	       (error expand-syntax "pattern variable with too few ellipses"))
	     (cdr p)))
       ;; A literal identifier is being inserted into the expansion.
       ;; Stick in an appropriate ***special-binding.
       ((assq template renamings) => cdr)
       (else
	;; the expansion is the value this has inside the 
	;; macro definition environment.
	(let ((renaming (list ***special-binding template
			      (identifier-rename template def-env))))
	  (set! renamings (cons (cons template renaming) renamings))
	  renaming))))

     ((pair? pattern)
      (if (and (pair? (cdr pattern))
	       (eq? ellipsis (cadr pattern)))
	  (begin
	    (unless (null? (caddr pattern))
	      (error match-syntax "non-final ellipsis in template"))
	    (let 

     
       


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

;;; destruct an ellipsed pairings assoc list; return two values
;;; first the base list which will be the same for all the
;;; things to do, and second, a list of pairings to append on, one
;;; per invocation.
(define (destruct-ellipsed pairings)
  (if (null? pairings)
      (values '() '())
      (let-values (((base per) (destruct-ellipsed (cdr pairings))))
	(cond
	 ((identifier? (car pairings))
	  (values (cons (cons (car pairings) bad-object) base)
		  per))
	 ((identifier? (caar pairings))
	  (values base 
		  (cons (car pairings) per)))
	 (else
	  (values (cons (caar pairings) base)
		  per))))))

;;; splices is an alists.  Each value in it should be the same length.
;;; Splice these into what we need to add to template assembly
;;; pairings.
;;; that is
;;; ((a . (1 2)) (b . (3 4))) => (((a . 1) (b . 3)) ((a . 2) (b . 4)))
(define (assemble-ellipsed splices)
  (if (null? splices)
      '()
      (begin
	(let ((n (length (cdar splices))))
	  (unless (every (lambda (splice)
			   (= n (length splice)))
			 splices)
	    (error assemble-ellipsed "ellipsis length mismatch"))
	  
    
    
  