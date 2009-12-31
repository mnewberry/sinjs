;;; SINJS Scheme library

;;;
;;; SYNTAX
;;;
;;; Syntax bindings declared here are present for all
;;; programs, and are thus specified at top level with define-syntax.
;;;
;;; The compiler handles all definitions, as well as
;;; IF, LAMBDA, SET!, QUOTE, BEGIN, LET-SYNTAX, and LETREC-SYNTAX.

;;; R5RS 4.2.1
(define-syntax cond
  (syntax-rules (=> else)
    ;; one clause forms
    ((_ (else expr exprs ...)) (begin expr exprs ...))
    ((_ (test)) test)
    ((_ (test => proc)) (let ((tmp test)) (if tmp (proc tmp))))
    ((_ (test exprs ...)) (if test (begin exprs ...)))

    ;; multiple clause forms [except else]
    ((_ (test) more ...) (let ((tmp test)) (if tmp tmp (cond more ...))))
    ((_ (test => proc) more ...)
     (let ((tmp test)) (if tmp (proc tmp) (cond more ...))))
    ((_ (test exprs ...) more ...)
     (let ((tmp test)) (if tmp (begin exprs ...) (cond more ...))))))

(define-syntax case
  (syntax-rules (else)
    ;; this matches any KEY which cannot bear multiple evaluation.
    ((_ (key ...) clauses ...)
     (let ((tmp (key ...))) (case tmp clauses ...)))

    ;; one clause forms
    ((_ key (else exprs ...))
     (begin exprs ...))
    ((_ key ((datums ...) exprs ...))
     (if (memv key 'datums) (begin exprs ...)))

    ;; multiple clause form
    ((_ key ((datums ...) exprs ...) more ...)
     (if (memv key 'datums) (begin exprs ...) (case key more ...)))))

(define-syntax and
  (syntax-rules ()
    ((_) #t)
    ((_ x) x)
    ((_ x y ...) (if x (and y ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((_) #f)
    ((_ x) x)
    ((_ x y ...) (let ((tmp x)) (if tmp tmp (or y ...))))))

;;; R5RS 4.2.2 and 4.2.4

(define-syntax let
  (syntax-rules ()
    ((_ ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))
    ((_ name ((var val) ...) body ...)
     ((letrec ((name (lambda 
			 (var ...)
		       body ...)))
	name)
      val ...))))

(define-syntax let*
  (syntax-rules ()
    ((_ () body ...) (let () body ...))
    ((_ ((var val) more ...) body ...)
     (let ((var val)) (let* (more ...) body ...)))))

;; As Al Petrofsky points out, this is fine since DEFINE
;; must be handled by the compiler anyhow.
(define-syntax letrec
  (syntax-rules ()
    ((_ ((var val) ...) body ...)
     (let ()
       (begin (define var val) ...)
       (let () body ...)))))

;;; 4.2.4 [named LET is above]
(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
	 (test expr ...)
       command ...)
     (let-syntax ((last (syntax-rules ()
			  ((_ x) x)
			  ((_ x y) y))))
       (let loop ((var init) ...)
	 (if test
	     (begin 'undefined-do-result expr ...)
	     (loop (last var step ...) ...)))))))

;;; 4.2.5
;;; The R5RS sample implementation requires an auxiliary procedure
;;; which we'd like to avoid; if you put the procedure into the
;;; syntax-rules then users of promise get forced inlining of a big
;;; procedure.  So instead, we capture the expression here in a cons
;;; (FORCED . proc/val), and all the rest of the work is in FORCE.
(define-syntax delay
  (syntax-rules ()
    ((_ expr) (cons #f (lambda () expr)))))

;;; 4.2.6
;;; xxx tracks depth with secret extra arguments that users could screw with
(define-syntax quasiquote
  (syntax-rules (unquote unquote-splicing quasiquote)
    ;; basic top-level unquoting rules
    ((_ (unquote item)) item)
    ((_ ((unquote-splicing item) items ...)
	(append item (quasiquote (items ...)))))

    ;; increment depth if we are nesting
    ((_ (quasiquote item) tail ...
     (list 'quasiquote (quasiquote item #f tail ...))))
    
    ;; undo depth if unquoting and we are nested
    ((_ (unquote item) tail more-tail ...)
     (list 'unquote (quasiquote item more-tail ...)))
    ((_ (unquote-splicing item) tail more-tail ...)
     (list 'unquote-splicing (quasiquote item more-tail ...)))

    ;; deconstruct and reconstruct list and vector structure
    ((_ (item . items) tail ...)
     (cons (quasiquote item tail ...) (quasiquote items tail ...)))
    ((_ #(items ...) tail ...)
     (list->vector (quasiquote (items ...) tail ...)))

    ;; anything else is just quoted, whatever the depth
    ((_ anything tail ...) (quote anything))))


;;;
;;; Procedure definitions
;;;
;;; To avoid violating the inviolability rules which require
;;; standard procedures to always do the standard thing, even if other
;;; procedure name bindings have been assigned, we embed all of 
;;; them in a big let, and then return an alist for the procedures.
;;; The system then does the right thing to turn this alist into
;;; top level bindings.
;;;


;;;
;;; PROCEDURES (equivalence)
;;;
;;; EQ? is builtin.

(define (eqv? a b)
  (or (eq? a b)
      (and (char? a)
	   (char? b)
	   (char=? a b))
      (and (number? a)
	   (number? b)
	   (= a b))))

(define (equal? a b)
  (or (eqv? a b)
      (and (pair? a)
	   (pair? b)
	   (equal? (car a) (car b))
	   (equal? (cdr a) (cdr b)))
      (and (vector? a)
	   (vector? b)
	   (= (vector-length a) (vector-length b))
	   (let next-elt ((i 0))
	     (or (= i (vector-length a))
		 (and (equal? (vector-ref a i) (vector-ref b i))
		      (next-elt (+ i 1))))))
      (and (string? a)
	   (string? b)
	   (string=? a b))))

;;;
;;; PROCEDURES (numbers)
;;;
;;; Javascript only provides floating point numbers.  We cheat as follows,
;;; skating the edge of R5RS.  Scheme numbers will be Javascript numbers,
;;; and we will take advantage of permission to inexactify numbers by
;;; inexactifying *all* numbers, at every operation.  That is, no exact
;;; numbers actually exist, and even inexact->exact will produce an 
;;; inexact result.  This seems to actually be ok, with the following 
;;; curious reasoning.  We are permitted to reduce the supported range of
;;; exact numbers as we wish.  What of 6.2.3's admonishment that we must
;;; support them for valid data structure indexes and lengths?  Ah, well,
;;; we'll just extend those procedures to accept inexact arguments, and the
;;; result is that no correctly written Scheme program should be able to
;;; tell quite what we're up to.  Except, that is, for the curious behavior
;;; of the exact? and inexact->exact procedures.

;;;
;;; Builtin are the following procedures:
;;;   number? < > <= >= + * - / abs remainder
;;;   floor ceiling
;;;   exp log sin cos tan asin acos atan sqrt expt
;;;   number->string string->number

;;; The following are not provided, in accord with the permission
;;; of R5RS to omit them if we don't support general complex numbers:
;;;   make-rectangular make-polar real-part imag-part magnitude angle

(define complex? number?)
(define real? number?)
(define rational? number?)
(define (integer? obj)
  (and (number? obj)
       (= obj (floor obj))))

(define (exact? z) #f)
(define (inexact? z) #t)

(define (zero? z)
  (= n 0))
(define (positive? x)
  (> n 0))
(define (negative? x)
  (< n 0))
(define (odd? n)
  (= 1 (remainder n 2)))
(define (even? n)
  (= 0 (remainder n 2)))

(define (max z1 . zs)
  (if (null? zs)
      z1
      (if (> z1 (car zs))
	  (apply max z1 (cdr zs))
	  (apply max zs))))

(define (min z1 . zs)
  (if (null? zs)
      z1
      (if (< z1 (car zs))
	  (apply min z1 (cdr zs))
	  (apply min zs))))

;;; JS has no builtin integer division!
(define (quotient n1 n2)
  (/ (- n1 (remainder n1 n2)) n2))

(define (modulo n1 n2)
  (let ((r (remainder n1 n2)))
    (cond
     ((and (negative? n2) (positive? r))
      (- r n2))
     ((and (positive? n2) (negative? r))
      (+ r n2))
     (else r))))

(define (gcd . ns)
  (define (gcd* a b)
    (if (zero? b)
	(abs a)
	(gcd* b (remainder a b))))
  (let next ((ns ns) (g 0))
    (if (null? ns)
	g
	(next (cdr ns) (gcd* g (car ns))))))

(define (lcm . ns)
  (define (lcm* a b)
    (* (/ (abs a) (gcd a b)) (abs b)))
  (let next ((ns ns) (m 1))
    (if (null? ns)
	m
	(next (cdr ns) (lcm* g (car ns))))))

(define (denominator q)
  ;; assume we're dealing with binary floating point
  ;; and search for the right denominator.
  (if (integer? q)
      1
      (let next-ub-guess ((ub 1))
	(if (integer? (* q ub))
	    (let next-search ((lb 1)
			      (ub ub))
	      ;; binary search for the minimal denominator
	      ;; invariant: LB is too small; UB is (maybe) too big
	      (if (= ub (+ lb 1))
		  ub
		  (let ((midpoint (if (even? (- ub lb))
				      (+ lb (/ (- ub lb) 2))
				      (+ lb (/ (- ub lb) 2) -0.15))))
		    (if (integer? (* q midpoint))
			(next-search lb midpoint)
			(next-search midpoint ub)))))
	    (next-ub-guess (* ub 2))))))

(define (numerator q)
  (* (denominator q) q))

(define (truncate x)
  (cond
   ((positive? x) (floor x))
   ((negative? x) (ceiling x))
   (else x)))

;;; note that it is not clear if JS's Math.round is guaranteed
;;; to round even on midpoints.
(define (round x)
  (let ((diff (- x (floor x))))
    (cond
     ((< diff 0.5) (floor x))
     ((> diff 0.5) (ceiling x))
     ((even? (floor x)) (floor x))
     (else (ceiling x)))))

;;; Thanks to Alan Bawden.  This is snarfed from the MIT/GNU Scheme source.
(define (rationalize x e)
  (define (loop x y)
    (let ((fx (floor x))
	  (fy (floor y)))
      (cond ((not (< fx x)) fx)
	    ((= fx fy)
	     (+ fx
		(/ 1
		   (loop (/ 1 (- y fy))
			 (/ 1 (- x fx))))))
	    (else (+ fx 1)))))

  (define (find-it x y)
    (cond ((positive? x) (loop x y))
	  ((negative? y) (- (loop (- y) (- x))))
	  (else 0)))
  (cond
   ((positive? e) (find-it (- x e) (+ x e)))
   ((negative? e) (find-it (+ x e) (- x e)))
   (else x)))

(define (exact->inexact z) z)
(define (inexact->exact z) z)

;;;
;;; PROCEDURES (other data types)  
;;;
;;; Builtin are:
;;;   pair? cons car cdr set-car! set-cdr!
;;;   symbol? symbol->string string->symbol 
;;;   char? char->integer integer->char
;;;   string? make-string string-length string-ref string-set!
;;;   vector? make-vector vector-length vector-ref vector-set!

(define (boolean? obj)
  (or (eq? obj #t)
      (eq? obj #f)))

(define (not obj)
  (if obj #f #t))

(define (caar obj) (car (car obj)))
(define (cadr obj) (car (cdr obj)))
(define (cdar obj) (cdr (car obj)))
(define (cddr obj) (cdr (cdr obj)))

(define (caaar obj) (car (caar obj)))
(define (caadr obj) (car (cadr obj)))
(define (cadar obj) (car (cdar obj)))
(define (caddr obj) (car (cddr obj)))
(define (cdaar obj) (cdr (caar obj)))
(define (cdadr obj) (cdr (cadr obj)))
(define (cddar obj) (cdr (cdar obj)))
(define (cdddr obj) (cdr (cddr obj)))

(define (caaaar obj) (car (caaar obj)))
(define (caaadr obj) (car (caadr obj)))
(define (caadar obj) (car (cadar obj)))
(define (caaddr obj) (car (caddr obj)))
(define (cadaar obj) (car (cdaar obj)))
(define (cadadr obj) (car (cdadr obj)))
(define (caddar obj) (car (cddar obj)))
(define (cadddr obj) (car (cdddr obj)))
(define (cdaaar obj) (cdr (caaar obj)))
(define (cdaadr obj) (cdr (caadr obj)))
(define (cdadar obj) (cdr (cadar obj)))
(define (cdaddr obj) (cdr (caddr obj)))
(define (cddaar obj) (cdr (cdaar obj)))
(define (cddadr obj) (cdr (cdadr obj)))
(define (cdddar obj) (cdr (cddar obj)))
(define (cddddr obj) (cdr (cdddr obj)))

(define (null? obj)
  (eq? obj '()))

(define (list? obj)
  (or (null? obj)
      (and (pair? obj)
	   (list? (cdr obj)))))

(define (list . objs) objs)

(define (length list)
  (if (null? list)
      0
      (+ 1 (length (cdr list)))))

(define (append . lists)
  (cond
   ((null? lists) '())
   ((null? (car lists)) (apply append (cdr lists)))
   (else (cons (caar lists) (apply append (cdar lists) (cdr lists))))))

(define (reverse lyst)
  (if (null? lyst)
      '()
      (append (reverse lyst) (list (car lyst)))))

(define (list-tail list k)
  (if (zero? k)
      list
      (list-tail (cdr list) (- k 1))))

(define (list-ref list k)
  (if (zero? k)
      (car list)
      (list-ref (cdr list) (- k 1))))

(define (memq obj list)
  (and (pair? list)
       (if (eq? obj (car list))
	   list
	   (memq obj (cdr list)))))

(define (memv obj list)
  (and (pair? list)
       (if (eqv? obj (car list))
	   list
	   (memv obj (cdr list)))))

(define (member obj list)
  (and (pair? list)
       (if (equal? obj (car list))
	   list
	   (member obj (cdr list)))))

(define (assq obj alist)
  (and (pair? alist)
       (if (eq? obj (caar alist))
	   (car alist)
	   (assq obj (cdr alist)))))

(define (assv obj alist)
  (and (pair? alist)
       (if (eqv? obj (caar alist))
	   (car alist)
	   (assv obj (cdr alist)))))

(define (assoc obj alist)
  (and (pair? alist)
       (if (equal? obj (caar alist))
	   (car alist)
	   (assoc obj (cdr alist)))))

(define-syntax char-compare
  (syntax-rules ()
    ((_ name test? cvt)
     (define (name char1 char2)
       (test? (cvt char1) (cvt char2))))))

(char-compare char=? = char->integer)
(char-compare char<? < char->integer)
(char-compare char>? > char->integer)
(char-compare char<=? <= char->integer)
(char-compare char>=? >= char->integer)
(char-compare char-ci=? char=? char-upcase)
(char-compare char-ci<? char<? char-upcase)
(char-compare char-ci>? char>? char-upcase)
(char-compare char-ci<=? char<=? char-upcase)
(char-compare char-ci>=? char>=? char-upcase)

(define (char-alphabetic? c)
  (or (char-upper-case? c)
      (char-lower-case? c)))

(define (char-numeric? c)
  (let ((n (char->integer c)))
    (and (>= n 48) (<= n 57))))

(define (char-whitespace? c)
  (let ((n (char->integer c)))
    (or (= n 32)			;space
	(= n 9)				;tab
	(= n 10)			;linefeed
	(= n 12)			;formfeed
	(= n 13))))			;carriage return

(define (char-upper-case? c)
  (let ((n (char->integer c)))
    (and (>= n 65) (<= n 90))))

(define (char-lower-case? c)
  (let ((n (char->integer c)))
    (and (>= n 97) (<= n 122))))
  
(define (char-upcase c)
  (if (char-lower-case? c)
      (integer->char (- (char->integer c) 32))
      c))

(define (char-downcase c)
  (if (char-upper-case? c)
      (integer->char (+ (char->integer c) 32))
      c))

;; this is bad, given that string-set! is so slow given
;; the immutability of JS strings.
(define (string . chars)
  (let ((s (make-string (length chars))))
    (let next ((i 0)
	       (l chars))
      (if (null? l)
	  s
	  (begin (string-set! s 0 (car l))
		 (next (+ i 1) (cdr l)))))))

(define-syntax string-equality
  (syntax-rules ()
    ((_ name chartest?)
     (define (name string1 string2)
       (let ((len (string-len string1)))
	 (and (= len (string-len string2))
	      (let next ((i 0))
		(or (= i len)
		    (and (chartest? (string-ref string1 i)
				    (string-ref string2 i))
			 (next (+ i 1)))))))))))
       
(string-equality string=? char=?)
(string-equality string-ci=? char-ci=?)

(define-syntax string-compare
  (syntax-rules ()
    ((_ name lentest? chartest? chareqtest?)
     (define (name string1 string2)
       (let ((len1 (string-len string1))
	     (len2 (string-len string2)))
	 (let ((minlen (min len1 len2)))
	   (let next ((i 0))
	     (if (= i minlen)
		 (lentest? len1 len2)
		 (or (chartest? (string-ref string1 i)
				(string-ref string2 i))
		     (and (chareqtest? (string-ref string1 i))
			  (chareqtest? (string-ref string2 i))
			  (next (+ i 1))))))))))))

(string-compare string<? < char<? char=?)
(string-compare string>? > char>? char=?)
(string-compare string<=? <= char<=? char=?)
(string-compare string>=? >= char>=? char=?)
(string-compare string-ci<? < char-ci<? char-ci=?)
(string-compare string-ci>? > char-ci>? char-ci=?)
(string-compare string-ci<=? <= char-ci<=? char-ci=?)
(string-compare string-ci>=? >= char-ci>=? char-ci=?)

(define (substring s start end)
  (let ((news (make-string (- end start))))
    (do ((i start (+ i 1))
	 (j 0 (+ j 1)))
	((= i end) news)
      (string-set! news j (string-ref s i)))))

(define (string-append . strings)
  (let ((s (make-string (apply + (map string-len strings)))))
    (let next-string ((i 0)
		      (strings strings))
      (if (null? strings)
	  s
	  (next-string
	   (+ i (let* ((this-string (car strings))
		       (this-len (string-len this-string)))
		  (do ((j 0 (+ j 1)))
		      ((= j len) len)
		    (string-set! s i (string-ref this-string j)))))
	   (cdr strings))))))

(define (string->list s)
  (let ((len (string-len s)))
    (let next-char ((i len)
		    (lis '()))
      (if (= len 0)
	  lis
	  (next-char (- i 1) (cons (string-ref s (- i 1)) lis))))))

(define (list->string lis)
  (let* ((len (length lis))
	 (s (make-string len)))
    (let next-char ((i 0)
		    (lis lis))
      (if (= i len)
	  s
	  (begin (string-set! s i (car lis))
		 (next-char (+ i 1) (cdr lis)))))))

(define (string-copy s)
  (substring s 0 (string-len s)))

(define (string-fill! s c)
  (let ((len (string-len s)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (string-set! s i c))))

(define (vector . objs)
  (list->vector objs))

(define (vector->list v)
  (let ((len (vector-length v)))
    (let next-elt ((i len)
		   (lis '()))
      (if (= len 0)
	  lis
	  (next-elt (- i 1) (cons (vector-ref v (- i 1)) lis))))))

(define (list->vector lis)
  (let ((v (make-vector (length lis))))
    (do ((i 0 (+ i 1))
	 (lis lis (cdr lis)))
	((null? lis) v)
      (vector-set! v i (car lis)))))

(define (vector-fill! v elt)
  (let ((len (vector-len v)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (vector-set! v i c))))

;;;
;;; PROCEDURES (control features)
;;;
;;; Builtin are:
;;;   procedure? apply call-with-current-continuation call-with-values
;;;   dynamic-wind

(define (map proc list . lists)
  (if (null? lists)
      (let map1 ((inlist list)
		 (outlist '()))
	(if (null? inlist)
	    (reverse outlist)
	    (map1 (cdr inlist) (cons (proc (car inlist)) outlist))))
      (let map1 ((inlists (cons list lists))
		 (outlist '()))
	(if (null? (car inlists))
	    (reverse outlist)
	    (map1 (map cdr inlists)
		  (cons (apply proc (map car inlists)) outlist))))))

(define (for-each proc . lists)
  (if (not (null? (car lists)))
      (begin
	(apply proc (map car lists))
	(for-each proc (map cdr lists)))))

;; Recall that a promise is a cons (FORCED . PROC/VAL).  If FORCED, 
;; the cdr holds the value; if not forced, it holds a thunk.
(define (force promise)
  (if (car promise)
      (cdr promise)
      (let ((val ((cdr promise))))
	(if (car promise)
	    (cdr promise)
	    (begin (set-car! promise #t)
		   (set-cdr! promise val)
		   val)))))

(define (values . vals)
  (call-with-current-continuation
   (lambda (c)
     (apply c vals))))

;;;
;;; EVAL
;;;
;;; Not implemented:
;;; eval scheme-report-environment null-environment interaction-environment

;;; INPUT AND OUTPUT
;;;
;;; Builtin are
;;;   open-input-file open-output-file close-input-port close-output-port
;;;   input-port? output-port?
;;;   current-input-port current-output-port [with an arg, sets it, returns old]
;;;   read-char peek-char eof-object? char-ready? write-char
;;;
(define (call-with-input-file str proc)
  (let ((p (open-input-file str)))
    (proc p)
    (close-input-port p)))

(define (call-with-output-file str proc)
  (let ((p (open-output-file str)))
    (proc p)
    (close-output-port p)))

(define (port? p)
  (or (input-port? p)
      (output-port? p)))

(define (with-input-from-file str proc)
  (call-with-input-file str (lambda (p)
			      (let ((old (current-input-port p)))
				(proc)
				(current-input-port old)))))

(define (with-output-to-file str proc)
  (call-with-output-file str (lambda (p)
			       (let ((old (current-output-port p)))
				 (proc)
				 (current-output-port old)))))

(define (read . port*)
  (let ((port (if (null? port*) (current-input-port) (car port*))))
    (define gathered #f)
    (define delimiters '(#\( #\) #\" #\' #\` #\,
			 #\; #\space #\tab #\newline #\page #\return))
    (define (read*)
      (if gathered
	  (let ((c (peek-char port)))
	    (if (memv c delimiters)
		(let ((tok (parse-long-token gathered)))
		  (set! gathered #f)
		  tok)
		(begin
		  (set! gathered 
			(string-append gathered 
				       (string (char-downcase 
						(read-char port)))))
		  (read*))))
	  (let ((c (read-char port)))
	    (case c
	      ((#\() (read-list #t))
	      ((#\)) (error read "unexpected list terminator"))
	      ((#\") (read-string))
	      ((#\') (list 'quote (read*)))
	      ((#\`) (list 'quasiquote (read*)))
	      ((#\,) (if (eqv? (peek-char port) #\@)
			 (begin (read-char port)
				(list 'unquote-splicing (read*)))
			 (list 'unquote (read*))))
	      ((#\#)
	       (case (char-downcase (read-char port))
		 ((#\t) #t)
		 ((#\f) #f)
		 ((#\() (list->vector (read-list #f)))
		 ((#\\) (let ((c (read-char port)))
			  (if (char-alphabetic? c)
			      (begin (set! gathered  (string-append "#\\" c))
				     (read*))
			      (parse-long-token (string-append "#\\" c)))))
		 ((#\i) (set! gathered "#i") (read*))
		 ((#\e) (set! gathered "#e") (read*))
		 ((#\b) (set! gathered "#b") (read*))
		 ((#\o) (set! gathered "#o") (read*))
		 ((#\d) (set! gathered "#d") (read*))
		 ((#\x) (set! gathered "#x") (read*))
		 (else (error read "impromper # syntax"))))
	      ((#\space #\newline #\page #\tab #\return) (read*))
	      ((#\;) (skip-comment) (read*))
	      (else
	       (if (or (char-alphabetic? c)
		       (char-numeric? c)
		       (memv c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\=
				 #\> #\? #\^ #\_ #\~ #\+ #\- #\. #\@)))
		   (begin (set! gathered (string (char-downcase c)))
			  (read*))
		   (error read 
			  (string-append "unexpected character '" c "'"))))))))

    ;; peek char, but skip white space on the way
    (define (skip-white-peek)
      (let ((c (peek-char port)))
	(cond
	 ((memv c (#\space #\tab #\newline #\page #\return))
	  (read-char port) 
	  (skip-white-peek))
	 ((char=? c #\;)
	  (skip-comment)
	  (skip-white-peek))
	 (else c))))
    
    (define (skip-comment)
      (if (not (char=? (read-char port) #\newline))
	  (skip-comment)))

    (define (read-list allow-improper-list)
      (if (char=? (skip-white-peek) #\))
	  (begin (read-char port) '())
	  (let ((a (read*)))
	    ;; dotted lists are a pain!
	    (if (char=? (skip-white-peek) #\.)
		(begin (read-char port)
		       (if (memv (peek-char port) delimiters)
			   ;; legitimate dot
			   (begin (if (not allow-improper-list)
				      (error read "invalid vector syntax"))
				  (read-char port) ;consume delimiter
				  (let ((b (read*)))
				    (skip-white-peek)
				    (if (not (char=? (read-char port) #\)))
					(error read "missing list terminator"))
				    (cons a b)))
			   ;; not really a dot, keep trying
			   (begin (set! gathered ".")
				  (cons a (read-list allow-improper-list)))))
		(cons a (read-list allow-improper-list))))))

    ;; parse a long token, defined specifically as a number, identifier,
    ;; or character.
    (define (parse-long-token str)
      (if (and (char=? (string-ref str 0) #\#)
	       (char=? (string-ref str 1) #\\))
	  (cond
	   ((string-ci=? str "#\\newline") #\newline)
	   ((string-ci=? str "#\\space") #\space)
	   ((string-ci=? str "#\\return") #\return)
	   ((string-ci=? str "#\\page") #\page)
	   ((string-ci=? str "#\\tab") #\tab)
	   (else (if (> (string-length str) 3)
		     (error read
			    (string-append "invalid character syntax " str))
		     (string-ref str 2))))
	  (or (string->number str)
	      (string->symbol str))))

    (define (read-string)
      (let ((str ""))
	(let more ((c (read-char port)))
	  (case c
	    ((#\\) 
	     (set! str (string-append str (string (read-char port))))
	     (more))
	    ((#\") str)
	    (else (set! str (string-append str c))
		  (more))))))

    (read*)))

(define (write obj . port*)
  (let ((port (if (null? port*) (current-output-port) (car port*))))
    (define (write-slashify str)
      (write-char #\" port)
      (let ((len (string-length str)))
	(do ((i 0 (+ i 1)))
	    ((>= i len))
	  (if (or (char=? (string-ref str i) #\")
		  (char=? (string-ref str i) #\\))
	      (write-char #\\ port))
	  (write-char (string-ref str i) port)))
      (write-char #\" port))

    (define (write-rest tail)
      (cond
       ((null? tail) (write-char #\) port))
       ((pair? tail)
	(write-char #\space port)
	(write (car tail) port)
	(write-rest (cdr tail)))
       (else (display " . " port)
	     (write (cdr tail) port)
	     (write-char #\) port))))

    (cond
     ((eq? obj #t) (display "#t" port))
     ((eq? obj #f) (display "#f" port))
     ((symbol? obj) (display (symbol->string obj) port))
     ((char? obj) (display (case obj
			     ((#\newline) "#\\newline")
			     ((#\return) "#\\return")
			     ((#\page) "#\\page")
			     ((#\tab) "#\\tab")
			     ((#\space) "#\\space")
			     (else (string #\# #\\ obj)))))
     ((vector? obj) (write-char #\# port) (write (vector->list obj) port))
     ((pair? obj) 
      (write-char #\( port)
      (write (car obj) port) 
      (write-rest (cdr obj)))
     ((null? obj) (display "()" port))
     ((number? obj) (display (number->string obj) port))
     ((string? obj) (slashify-string obj))
     ((procedure? obj) (display "#<procedure>" port))
     ((eof-object? obj) (display "#<eof>" port))
     ((port? obj) (display "#<port>" port)))))

(define (display obj . port*)
  (let ((port (if (null? port) (current-output-port) (car port))))
    (define (display-rest tail)
      (cond
       ((null? tail) (write-char #\) port))
       ((pair? tail)
	(write-char #\space port)
	(display (car tail) port)
	(display-rest (cdr tail)))
       (else (display " . " port)
	     (display (cdr tail) port)
	     (write-char #\) port))))

    (cond
     ((eq? obj #t) (display "#t" port))
     ((eq? obj #f) (display "#f" port))
     ((symbol? obj) (display (symbol->string obj) port))
     ((char? obj) (write-char obj port))
     ((vector? obj) (write-char #\# port) (display (vector->list obj) port))
     ((pair? obj) (write-char #\( port)
                  (display (car obj) port) 
                  (display-rest (cdr obj)))
     ((null? obj) (display "()" port))
     ((number? obj) (display (number->string obj) port))
     ((string? obj) (let ((len (string-length (obj))))
		      (do ((i 0 (+ i 1)))
			  ((>= i len))
			(write-char (string-ref obj i)))))
     ((procedure? obj) (display "#<procedure>" port))
     ((eof-object? obj) (display "#<eof>" port))
     ((port? obj) (display "#<port>" port)))))

(define (newline . port*)
  (write-char #\newline (if (null? port) (current-output-port) (car port))))

;;; SYSTEM INTERFACE
;;;
;;; Not implemented:
;;; load transcript-on transcript-off
