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

;;; tell whether OBJ is the toplevel id (in ENV) matching symbol SYM
(define (top-level-id obj env sym)
  (and (identifier? obj)
       (not (identifier-rename obj env))
       (eq? (identifier->name obj) sym)))

;;; return a copy of FORM with all special identifiers cleaned out
;;; (used for returning literals)
(define (clean-ids form)
  (cond
   ((identifier? form) (identifier->name form))
   ((pair? form) (cons (clean-ids (car form)) (clean-ids (cdr form))))
   (else form)))

;;; here is the basic syntax walker
(define (expand form env)
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
	  ((quote) (clean-ids form))		;nothing to do

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
	   #;(display (format "lambda ~s\n" (pform form)))
	   (let ((bindings (map-formals-flat (lambda (id)
					       (cons id 
						     (uniquify
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
						       (top-level-id (car transformer) env
								     'syntax-rules))
					    (error expand 
						   (format "bad let-syntax transformer: ~a"
							   (pform transformer))))
					  `(,name . ((syntax-rules ,@(cdr transformer)) . ,env))))
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
						 (top-level-id (car transformer) env 'syntax-rules))
				      (error expand 
					     "bad letrec-syntax transformer"))
				    `(,name . ((syntax-rules ,@(cdr transformer)) . #f))))
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

;;; map across a lambda list preserving dotted list structure
(define (map-formals proc formals)
  (cond
   ((identifier? formals) (proc formals))
   ((null? formals) '())
   ((pair? formals) 
    (if (not (identifier? (car formals)))
	(error map-formals "bad lambda list")
	(cons (proc (car formals)) 
	      (map-formals proc (cdr formals)))))
   (else (error map-formals "bad lambda list"))))

;;; map across a lambda list, turning dotted list into proper list structure
(define (map-formals-flat proc formals)
  (cond
   ((identifier? formals) (list (proc formals)))
   ((null? formals) '())
   ((pair? formals) 
    (if (not (identifier? (car formals)))
	(error map-formals-flat "bad lambda list")
	(cons (proc (car formals))
	      (map-formals-flat proc (cdr formals)))))
   (else (error map-formals-flat "bad lambda list"))))


;;; expand all first-level syntax in a form, but leave the rest alone.
(define (expand-first-syntax form env)
  #;(display (format "efs ~s\n" (pform form)))
  (if (or (identifier? form) 
	  (not (pair? form))
	  (not (identifier? (car form))))
      form
      (let ((starter (identifier-rename (car form) env)))
	#;(display (format "starter ~s\n" (pform starter)))
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
	(finish-body defns forms)
	(let ((primo (expand-first-syntax (car forms) env)))
	  (if (pair? primo)
	      (cond
	       ((top-level-id (car primo) env 'define)
		(expand-body1 (cdr forms) (cons primo defns)))
	       ((top-level-id (car primo) env 'begin)
		(expand-body1 (append (cdr primo) (cdr forms)) defns))
	       (else (finish-body defns forms)))
	      (finish-body defns forms)))))

  ;; if there are definitions, turn them into an implicit letrec-type
  ;; construct and expand that. note that we are careful to make sure
  ;; the LAMBDA and SET! we insert have their top-level bindings since
  ;; they are passed to EXPAND.
  ;; xxx
  ;; note that this has a bug: the variables are not assigned all
  ;; at once, after the values have been determined, as they should be.
  ;; xxx
  (define (finish-body defns exprs)
    (when (null? exprs)
      (error finish-body "lambda expression without body is prohibited"))
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

(define (expand-syntax-rules1 form ellipsis literals rules def-env use-env)

  ;; return a match list if FORM matches PATTERN, and #f otherwise.
  ;; A match list is an alist, whose cdr is a pair (depth . val).
  ;; Depth indicates nesting level within ellipses, and val is the form
  ;; matching this pattern variable.
  (define (match-syntax form pattern)
    (cond
     ((identifier? pattern)
      (if (memq pattern literals)
	  (and (identifier? form)
	       (eq? (identifier-rename pattern def-env)
		    (identifier-rename form use-env))
	       '())
	  (list `(,pattern 0 . ,form))))

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
		 (if (null? form)
		     (null-ellipses (car pattern))
		     (let ((matches  (map (cut match-syntax <> (car pattern))
					  form)))
		       (and (every identity matches)
			    (assemble-ellipsed matches))))))
	  (and (pair? form)
	       (not (identifier? form))
	       (let ((match-car (match-syntax (car form) (car pattern)))
		     (match-cdr (match-syntax (cdr form) (cdr pattern))))
		 (and match-car
		      match-cdr
		      (append match-car match-cdr))))))
     (else
      (error match-syntax "unsupported pattern"))))

  (define (inc-depth element)
    `(,(car element) ,(+ (cadr element) 1) ,@(cddr element)))

  (define (dec-depth element)
    `(,(car element) ,(- (cadr element) 1) ,@(cddr element)))

  ;; PATTERN is piece of a syntax pattern, which is being "matched"
  ;; with ellipses against an empty list.  Generate a suitable
  ;; empty spliced ellipses entry for a match list by scanning
  ;; through it looking for the relevant identifiers.
  (define (null-ellipses pattern)
    #;(display (format "null ellipses on ~s\n" (pform pattern)))
    (cond
     ((identifier? pattern)
      (if (memq pattern literals)
	  '()
	  (list `(,pattern 1 . ()))))
     ((null? pattern) '())
     ((pair? pattern)
      (if (and (pair? (cdr pattern))
	       (eq? ellipsis (cadr pattern)))
	  (begin
	    (unless (null? (cddr pattern))
	      (error null-ellipses "non-final ellipsis in pattern"))
	    ;; inner ellipses get the same treatment, but one more
	    ;; level of parens around each key.
	    (map inc-depth (null-ellipses (car pattern))))
	  (append (null-ellipses (car pattern))
		  (null-ellipses (cdr pattern)))))
     (else
      (error null-ellipses "unsupported pattern"))))
	
  ;; this works because toplevel EXPAND-SYNTAX is called at most once
  ;; inside an invocation of EXPAND-SYNTAX-RULES1.
  (define renamings '())

  ;; expand the syntax template TEMPLATE in accord with the matched
  ;; PAIRING match alist.
  (define (expand-syntax template pairing)
    (cond
     ((identifier? template)
      (cond
       ((assq template pairing) => 
	(lambda (element)
	  (if (zero? (cadr element))
	      (cddr element)
	      (error expand-syntax
		     (string-append
		      "pattern variable occurs in template with too "
		      (if (negative? (cadr element)) "many" "few")
		      " ellipses")))))
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
	    (map (lambda (p) (expand-syntax (car template) p))
		 (let ((g (gather-ellipsed (find-ellipsed template) pairing)))
		   #;(display (format "  as ~s\n" g))
		   g)))
	  (cons (expand-syntax (car template) pairing)
		(expand-syntax (cdr template) pairing))))

     (else template)))
  
  ;; find the variables in TEMPLATE which ellipsis expansion should
  ;; deal with and return an alist mapping them to their depth.
  (define (find-ellipsed template)
    (cond
     ((identifier? template)
      (if (eq? ellipsis template)
	  (error find-ellipsed "misplaced ellipsis in template")
	  (list (cons template 0))))
     ((pair? template)
      (if (and (pair? (cdr template))
	       (eq? ellipsis (cadr template)))
	  (map (lambda (a) 
		 (cons (car a) (+ (cdr a) 1)))
	       (find-ellipsed (car template)))
	  (append (find-ellipsed (car template))
		  (find-ellipsed (cdr template)))))
     (else '())))

  ;; VARS is an alist of pattern variables to the depth they occur in
  ;; a piece of template. 
  ;; return a list of PAIRINGS for iteration across one ellipsis depth.
  (define (gather-ellipsed vars pairing)
    ;; make sure each pattern variable needed occurs
    ;; at the right depth in PAIRING.
    (for-each 
     (lambda (varmap)
       (let ((element (assq (car varmap) pairing)))
	 (when (and element
		    (not (= (cdr varmap) (cadr element))))
	   (error gather-ellipsed "incorrect number of ellipses in pattern"))))
     vars)

    #;(display (format "  gathering ~s with ~s\n" vars pairing))
    ;; return a new pairing with the depth reduced a level, and only
    ;; including variables in VARS.
    (let ((new-pairing (filter-map (lambda (elt) 
				     (and (assq (car elt) vars)
					  (dec-depth elt)))
				   pairing)))
      #;(display (format "   new pairing ~s\n" new-pairing))
      (let ((vars (map car new-pairing))
	    (depths (map cadr new-pairing))
	    (vals (map cddr new-pairing)))
	#;(display (format "   vars ~s depths ~s vals ~s\n" vars depths vals))
	(unless (apply = (map length vals))
	  (error gather-ellipsed
		 "lists not of equal length in ellipsis expansion"))
	(if (null? vals)
	    '()
	    (apply map 
		   (lambda val-set (map (lambda (var depth val) 
					  `(,var ,depth . ,val))
					vars depths val-set))
		   vals)))))
  
  (unless (identifier? ellipsis)
    (error expand-syntax-rules "bad ellipsis in syntax-rules"))
  (unless (list? literals)
    (error expand-syntax-rules "bad literals in syntax-rules"))
  (unless (list? rules)
    (error expand-syntax-rules "bad ruleset in syntax-rules"))
  
  (let next ((rules rules))
    (when (null? rules)
      (error expand-syntax-rules (format "syntax match failure: ~a" 
					 (pform form))))
    (unless (and (list? (car rules))
		 (= (length (car rules)) 2))
      (error expand-syntax-rules "bad rule in syntax-rules"))
    (let ((pairing (match-syntax (cdr form) (cdaar rules))))
      (if pairing
	  (begin
	    #;(display (format "expanding ~s\nwith pattern~s\n"
			     (pform form) (pform (caar rules))))
	    #;(display (format "inside template ~s\n" (pform (cadar rules))))
	    (let ((x (expand-syntax (cadar rules) pairing)))
	      #;(display (format "producing ~s\n" (pform x)))
	      x))
	  (next (cdr rules))))))

;;; matches is a list of match lists.  Splice them together.
(define (assemble-ellipsed matches)
  #;(display (format "assembling ~s\n" matches))
  (map
   (lambda (elt)
     `(,(car elt) ,(+ 1 (cadr elt))
       . ,(map (lambda (m) (cddr (assq (car elt) m))) matches)))
   (car matches)))

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

  