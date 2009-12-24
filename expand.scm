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
   ((pair? form) (caddr form))
   (else #f)))

;;; here is the basic syntax walker
(define (expand form env)
  (display (format "e ~s\n" (pform form)))
  ;;; this is what to do if we recognize a form as a combination.
  (define (combination) (map (cut expand <> env) form))

  (cond
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

   ;; combination with nothing special
   ((and (pair? form)
	 (not (identifier? (car form))))
    (combination))

   ((pair? form)
    (let ((starter (identifier-rename (car form) env)))
      (cond
       ;; if starter is a pair, then this is a macro invocation.
       ;; expand it and recurse.
       ((pair? starter)
	(if (and (pair? (car starter))
		 (eq? (caar starter) 'syntax-rules))
	    (expand (expand-syntax-rules form (car starter) (cdr starter) env) 
		    env)
	    (error expand "internal bad syntax transformer spec")))

       ;; if starter is not a pair, and not #f, then it is locally bound,
       ;; and therefore not a syntactic keyword.
       (starter (combination))

       ;; so we have a top-level binding. if it is a synactic keyword,
       ;; dtrt, otherwise, it's a combination.
       (else
	(case (identifier->name (car form))
	  ((quote) form)		;nothing to do [don't expand inside!]

	  ((set!)
	   (let ((var (cadr form))
		 (value (expand (caddr form) env)))
	     (unless (identifier? var)
	       (error expand "bad identifier in set!"))
	     (let ((r (identifier-rename var env)))
	       (cond
		((symbol? r) `(set! ,r ,value))
		((pair? r) (error expand "illegal use of syntax keyword"))
		((not r) `(top-level-set! ,(identifier->name var) ,value))))))

	  ((if) 
	   (if (= (length form) 3)
	       `(if ,(expand (cadr form) env)
		    ,(expand (caddr form) env)
		    (quote oh-mickey-youre-so-fine-you-blow-my-mind-hey-mickey))
	       `(if ,(expand (cadr form) env)
		    ,(expand (caddr form) env)
		    ,(expand (cadddr form) env))))

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
					  (cons id (uniquify
						    (identifier->name id))))
					(cadr form))))
	     `(lambda ,(map-formals (lambda (id) (cdr (assq id bindings)))
				    (cadr form))
		,@(expand-body (cddr form) (append bindings env)))))

	  ;; note that let-syntax and letrec-syntax create a
	  ;; LAMBDA and not a BEGIN.  this makes sure that
	  ;; they don't get spliced the way BEGIN does, so that
	  ;; internal definitions inside them are treated as
	  ;; properly internal.  That is correct for R5RS,
	  ;; but R6RS changes it (see R6RS 11.2).
	  ((let-syntax)
	   (let ((binding-list (cadr form))
		 (body (cddr form)))
	     `((lambda () 
		 ,@(expand-body body
				(append 
				 (map (lambda (binding)
					(let ((name (car binding))
					      (transformer (cadr binding)))
					  (unless (identifier? name)
					    (error expand 
						   "bad let-syntax syntax"))
					  (unless (and (pair? transformer)
						       (eq? (car transformer)
							    'syntax-rules))
					    (error expand 
						   "bad let-syntax transformer"))
					  `(,name . (,transformer . ,env))))
				      binding-list)
				 env))))))

	  ((letrec-syntax)
	   (let* ((binding-list (cadr form))
		  (body (cddr form))
		  (env-add (map (lambda (binding)
				  (let ((name (car binding))
					(transformer (cadr binding)))
				    (unless (identifier? name)
				      (error expand "bad letrec-syntax syntax"))
				    (unless (and (pair? transformer)
						 (eq? (car transformer)
						      'syntax-rules))
				      (error expand 
					     "bad letrec-syntax transformer"))
				    `(,name . (,transformer . #f))))
				binding-list))
		  (new-env (append env-add env)))
	     (for-each
	      (lambda (env-element)
		(set-cdr! (cdr env-element) new-env))
	      env-add)
	     `((lambda () ,@(expand-body body new-env)))))

	  ;; top level binding, but not to a syntactic keyword, so it's
	  ;; just a combination
	  (else (combination)))))))
   (else (error expand "improper expression"))))

;;; map across a lambda list.
(define (map-formals proc formals)
  (cond
   ((identifier? formals) (proc formals))
   ((null? formals) '())
   ((pair? formals) (cons (proc (car formals)) 
			  (map-formals proc (cdr formals))))
   (else (error map-formals "bad lambda list"))))

;;; expand all first-level syntax in a form, but leave the rest alone.
(define (expand-first-syntax form env)
  (display (format "efs ~a\n" (pform form)))
  (if (or (identifier? form) 
	  (not (pair? form))
	  (not (identifier? (car form))))
      form
      (let ((starter (identifier-rename (car form) env)))
	(cond
	 ((or (symbol? starter) (not starter)) form)
	 ((and (pair? (car starter))
	       (eq? (caar starter) 'syntax-rules))
	  (expand-first-syntax (expand-syntax-rules form (car starter)
						    (cdr starter) env)
			       env))
	 (else
	  (error expand-first-syntax "internal bad syntax transform spec"))))))

;;; turn an implicit LAMBDA define (if this is one) into 
;;; a correct version.  make sure we observe hygiene when inserting
;;; the keyword LAMBDA into the output.
(define (clean-define defn)
  (if (identifier? (cadr defn))
      ;; normal definition
      (if (= (length defn) 3)
	  defn
	  (error clean-define "definition too long"))
      ;; implicit lambda
      (if (>= (length defn) 3)
	  (let ((name (caadr defn))
		(formals (cdadr defn))
		(body (cddr defn))
		(magic-lambda (list ***special-binding 'lambda #f)))
	    `(,(car defn) ,name (,magic-lambda ,formals ,@body))))))

;;; expand a body in the specified environment.
;;; we need expand-first-syntax because we must allow syntax
;;; that produces (define ...) forms, without then going inside them and
;;; doing the identifier renaming (since we don't yet know what the 
;;; defined identifiers are!).  Thank God R5RS doesn't allow internal
;;; syntax definitions, or this would be nearly impossible.
(define (expand-body forms env)

  ;;; Peel off internal definitions from the front of FORMS
  ;;; and when they're all taken care of, use FINISH-BODY to
  ;;; construct the result.
  (define (expand-body1 forms defns)
    (if (null? forms) 
	(finish-body defns)
	(let ((primo (expand-first-syntax (car forms) env)))
	  (if (and (pair? primo)	;list syntax
		   (identifier? (car primo)) ;begins with identifier
		   (not (identifier-rename (car primo) env)) ;not locally bound
		   (or (eq? (identifier->name (car primo))
			    'define) ;and is a definition
		       (eq? (identifier->name (car primo))
			    'begin))) ;or might be
	      (if (eq? (identifier->name (car primo)) 'begin)
		  ;; splice
		  (expand-body1 (append (cdr primo) forms) defns)
		  ;; a definition
		  (expand-body1 (cdr forms) (cons primo defns)))
	      (finish-body defns forms)))))

  ;; if there are definitions, turn them into an implicit letrec-type
  ;; construct and expand that. note that we are careful to make sure
  ;; the LAMBDA and SET! we insert have their top-level bindings since
  ;; they are passed to EXPAND.
  (define (finish-body defns exprs)
    (if (null? defns)
	(map (cut expand <> env) exprs)
	(let* ((defns (map clean-define defns))
	       (vars (map cadr defns))
	       (vals (map caddr defns))
	       (magic-lambda (list ***special-binding 'lambda #f))
	       (magic-set! (list ***special-binding 'set! #f)))
	  (expand
	   `((,magic-lambda ,vars
		,@(map (lambda (var val) `(,magic-set! ,var ,val))
		       vars vals)
		,@exprs)
	     ,@(list (length vars) 
		     'when-i-get-older-losing-my-hair-many-years-form-now))
	   env))))

  (expand-body1 forms '()))
  

;;; Note that everything in syntax transformers could be
;;; ***special-binding objects, if a macro expanded into a syntax binding
;;; construct.

;;; Expand FORM according to the specified syntax-rules TRANSFORMER.
;;; DEF-ENV is the environment in which the transformer was specified;
;;; and USE-ENV is the environment in which FORM was found.  If the
;;; transformer is specified in a top-level DEFINE-SYNTAX, then DEF-ENV
;;; will be #f, and the definition environment is the *current* top-level
;;; environment.  This way top-level DEFINE-SYNTAX definitions can use
;;; each other recursively.  R6RS will require a change to this, since
;;; it allows internal DEFINE-SYNTAX.
(define (expand-syntax-rules form transformer def-env use-env)
  (let ((def-env (or def-env top-level-environment)))
    (if (identifier? (cadr transformer))
	(expand-syntax-rules1 form (cadr transformer) (caddr transformer)
			      (cdddr transformer) def-env use-env)
	(expand-syntax-rules1 form '... (cadr transformer)
			      (cddr transformer) def-env use-env))))

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
	  (and (identifier? form)
	       (eq? (identifier-rename pattern def-env)
		    (identifier-rename form use-env))
	       '())
	  (list (cons pattern form))))

     ((null? pattern)
      (and (null? form)
	   '()))

     ((pair? pattern)
      (if (and (pair? (cdr pattern))
	       (eq? ellipsis (cadr pattern)))
	  (begin
	    (unless (null? (cddr pattern))
	      (error match-syntax "non-final ellipsis in pattern"))
	    (and (list? form)
		 (let ((matches (map (cut match-syntax <> (car pattern))
				     form)))
		   (and (every identity matches)
			(assemble-ellipsed matches)))))
	  (and (pair? form)
	       (not (identifier? form))
	       (let ((match-car (match-syntax (car form) (car pattern)))
		     (match-cdr (match-syntax (cdr form) (cdr pattern))))
		 (and match-car
		      match-cdr
		      (append match-car match-cdr))))))
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
       ;; Stick in an appropriate ***special-binding or use the one
       ;; we already have if this is a repeat.
       ((assq template renamings) => cdr)
       (else
	;; the expansion is the value this has inside the 
	;; macro definition environment.
	(let ((renaming (list ***special-binding template
			      (identifier-rename template def-env))))
	  (set! renamings (cons (cons template renaming) renamings))
	  renaming))))

     ((pair? template)
      (if (and (pair? (cdr template))
	       (eq? ellipsis (cadr template)))
	  (begin
	    (unless (null? (cddr template))
	      (error expand-syntax "non-final ellipsis in template"))
	    (let-values (((base splices) (destruct-ellipsed pairing)))
	      (display (format "template ~a\n pairing ~a\n base ~a\n splices ~a\n" template pairing base splices))
	      (map (lambda (newbies)
		     (expand-syntax (car template) (append newbies base)))
		   (splice-ellipsed splices))))
	  (cons (expand-syntax (car template) pairing)
		(expand-syntax (cdr template) pairing))))

     (else template)))

  (unless (identifier? ellipsis)
    (error expand-syntax-rules "bad ellipsis in syntax-rules"))
  (unless (list? literals)
    (error expand-syntax-rules "bad literals in syntax-rules"))
  (unless (list? rules)
    (error expand-syntax-rules "bad ruleset in syntax-rules"))
  
  (display (format "expanding ~a\n" (pform form)))
  (let next ((rules rules))
    (when (null? rules)
      (error expand-syntax-rules "syntax match failure"))
    (unless (and (list? (car rules))
		 (= (length (car rules)) 2))
      (error expand-syntax-rules "bad rule in syntax-rules"))
    (display (format "trying ~a\n" (caar rules)))
    (let ((pairing (match-syntax (cdr form) (cdaar rules))))
      (if pairing
	  (begin
	    (display (format "template ~a\n" (pform (cadar rules))))
	    (let ((x (expand-syntax (cadar rules) pairing)))
	      (display (format "result ~a\n" (pform x)))
	      x))
	  (next (cdr rules))))))

;;; matches is a list of match lists.  Splice them together.
(define (assemble-ellipsed matches)
  (map
   (lambda (match-pair)
     (let ((key (car match-pair)))
       (cons (list key) (map (cut alist-ref key <> equal?) matches))))
   (car matches)))

;;; destruct an ellipsed pairings assoc list; return two values
;;; first the base list which will be the same for all the
;;; things to do, and second, a list of pairings to append on, one
;;; per invocation: a suitable argument for splice-ellipsed.
(define (destruct-ellipsed pairings)
  (if (null? pairings)
      (values '() '())
      (let-values (((base per) (destruct-ellipsed (cdr pairings))))
	(let* ((this (car pairings))
	       (key (car this))
	       (val (cdr this)))
	  (cond
	   ((identifier? key)
	    (values (cons (cons key bad-object) base)
		    per))
	   ((identifier? (car key))
	    (values base 
		    (cons (cons (car key) val) per)))
	   (else
	    (values (cons (cons (car key) val) base)
		    per)))))))

;;; splices is an alists.  Each value in it should be the same length.
;;; Splice these into what we need to add to template assembly
;;; pairings.
;;; that is
;;; example:
;;; ((a . (1 2)) (b . (3 4)) (c . (5 6)) 
;;;   => (((a . 1) (b . 3) (c . 5)) ((a . 2) (b . 4) (c . 6)))
(define (splice-ellipsed splices)
  (let ((names (map car splices))
	(val-lists (map cdr splices)))
    ;; names (a b c)
    ;; val-lists ((1 2) (3 4) (5 6))
    (apply map 
	   ;; vals (1 3 5) and then (2 4 6)
	   (lambda vals
	     (map cons names vals))
	   val-lists)))

;;; like write, but with special magic for identifiers (the problem is
;;; that if an id is special-bound to a syntax transformer, the
;;; transformer includes a reference to its own environment, which in
;;; the case of letrec-syntax, is a circular structure) so we don't
;;; print the renaming of specially bound identifiers.
(define (pform form)
  (cond
   ((symbol? form) (symbol->string form))
   ((pair? form) 
    (if (eq? (car form) ***special-binding)
	(string-append "SPECIAL-" (symbol->string (cadr form)))
	(string-append "(" (pform (car form)) (prest (cdr form)))))
   (else (format "~s" form))))

(define (prest form)
  (cond
   ((null? form) ")")
   ((pair? form) (string-append " " (pform (car form)) (prest (cdr form))))
   (else (string-append " . " (pform form) ")"))))

  