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
    ((_ (else expr exprs ...)) (begin 'not-defn expr exprs ...))
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
     (begin 'not-defn exprs ...))
    ((_ key ((datums ...) exprs ...))
     (if (memv key '(datums ...)) (begin exprs ...)))

    ;; multiple clause form
    ((_ key ((datums ...) exprs ...) more ...)
     (if (memv key '(datums ...)) (begin exprs ...) (case key more ...)))))

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
	     (begin (begin command ...) (loop (last var step ...) ...))))))))

;;; 4.2.5
;;; The R5RS sample implementation requires an auxiliary procedure
;;; which we'd like to avoid; if you put the procedure into the
;;; syntax-rules then users of promise get forced inlining of a big
;;; procedure.  So instead, we capture the expression here in a cons
;;; (FORCED . proc/val), and all the rest of the work is in FORCE.
;;;
;;; Requires CONS
(define-syntax delay
  (syntax-rules ()
    ((_ expr) (cons #f (lambda () expr)))))

;;; 4.2.6
;;; xxx tracks depth with secret extra arguments that users could screw with
;;; Requires APPEND LIST CONS LIST->VECTOR 
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
;;; R5RS guarantees that standard procedures continue to function correctly
;;; even if others have been assigned.  We guarantee this by requiring
;;; any procedure used by another standard procedure to be either inlined,
;;; or specially dealt with.  

;;; The strategy then is to inline something if it's easy, or if it
;;; is used a lot.

;;; All R5RS-specified procedures are inlined except these:
;;;   equal? max min quotient modulo gcd lcm numerator denominator
;;;   truncate round rationalize number->string string->number
;;;   list? list length append reverse list-tail list-ref
;;;   memq memv member assq assv assoc 
;;;   char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
;;;   char-alphabetic? char-numeric? char-whitespace? 
;;;   char-upper-case? char-lower-case? char-upcase char-downcase
;;;   make-string string string-append string->list list->string string-fill!
;;;   string-ci=? string<? string>? string<=? string>=? 
;;;   string-ci<? string-ci>? string-ci<=? string-ci>=?
;;;   make-vector vector vector->list list->vector vector-fill!
;;;   apply map for-each force call-with-current-continuation
;;;   values call-with-values dynamic-wind
;;;   call-with-input-file call-with-output-file
;;;   with-input-from-file with-output-to-file
;;;   open-input-file open-output-file close-input-port close-output-port
;;;   read read-char peek-char char-ready? write display newline write-char
;;;   = < > <= >=  [with more than one arg]
;;;   + * [with other than two args]
;;;   - / [with more than two args]
;;;   c*r [three or four d/a's]
;;;   current-input-port current-output-port [with one arg]

;;; PROCEDURES (equivalence)
;;;
;;; Builtin: eq?

(define eqv? eq?)	       ;ok b/c we know chars and numbers are eq

(define equal?
  (let ()
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
    equal?))

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
  (= z 0))
(define (positive? x)
  (> x 0))
(define (negative? x)
  (< x 0))
(define (odd? n)
  (= 1 (remainder n 2)))
(define (even? n)
  (= 0 (remainder n 2)))

(define (max z1 . zs)
  (let loop ((l zs)
	     (top z1))
    (cond
     ((null? l) top)
     ((> (car l) top) (loop (cdr l) (car l)))
     (else (loop (cdr l) top)))))

(define (min z1 . zs)
  (let loop ((l zs)
	     (bottom z1))
    (cond
     ((null? l) bottom)
     ((< (car l) bottom) (loop (cdr l) (car l)))
     (else (loop (cdr l) bottom)))))

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

(define lcm
  (let ((gcd gcd))
    (lambda ns
      (define (lcm* a b)
	(* (/ (abs a) (gcd a b)) (abs b)))
      (let next ((ns ns) (m 1))
	(if (null? ns)
	    m
	    (next (cdr ns) (lcm* m (car ns))))))

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
				      (+ lb (/ (- ub lb) 2) -0.5))))
		    (if (integer? (* q midpoint))
			(next-search lb midpoint)
			(next-search midpoint ub)))))
	    (next-ub-guess (* ub 2))))))

(define numerator
  (let ((denominator denominator))
    (lambda (q)
      (* (denominator q) q))))

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
;;;   string? make-string string string-length string-ref string-set! 
;;;   string=? string<? string>? string<=? string>=? list->string string-fill!
;;;   vector? make-vector vector vector-length vector-ref 
;;;   vector-set! list->vector
;;;
;;; For efficiency's sake:
;;;   substring string-append

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

(define (caaaar obj) (caar (caar obj)))
(define (caaadr obj) (caar (cadr obj)))
(define (caadar obj) (caar (cdar obj)))
(define (caaddr obj) (caar (cddr obj)))
(define (cadaar obj) (cadr (caar obj)))
(define (cadadr obj) (cadr (cadr obj)))
(define (caddar obj) (cadr (cdar obj)))
(define (cadddr obj) (cadr (cddr obj)))
(define (cdaaar obj) (cdar (caar obj)))
(define (cdaadr obj) (cdar (cadr obj)))
(define (cdadar obj) (cdar (cdar obj)))
(define (cdaddr obj) (cdar (cddr obj)))
(define (cddaar obj) (cddr (caar obj)))
(define (cddadr obj) (cddr (cadr obj)))
(define (cdddar obj) (cddr (cdar obj)))
(define (cddddr obj) (cddr (cddr obj)))

(define (null? obj)
  (eq? obj '()))

(define (list? obj)
  (or (null? obj)
      (and (pair? obj)
	   (list? (cdr obj)))))

(define (list . objs) objs)

(define length
  (let ()
    (define (length list)
      (if (null? list)
	  0
	  (+ 1 (length (cdr list)))))
    length))

(define (append . lists)
  (let append* ((lists lists))
    (cond
     ((null? lists) '())
     ((null? (car lists)) (append* (cdr lists)))
     (else (cons (caar lists) (append* (cons (cdar lists) (cdr lists))))))))

(define (reverse list)
  (let next ((list list) 
	     (so-far '()))
    (if (null? list)
	so-far
	(next (cdr list) (cons (car list) so-far)))))

(define list-tail
  (let ()
    (define (list-tail list k)
      (if (zero? k)
	  list
	  (list-tail (cdr list) (- k 1))))
    list-tail))

(define (list-ref list k)
  (if (zero? k)
      (car list)
      (list-ref (cdr list) (- k 1))))

(define memq
  (let ()
    (define (memq obj list)
      (and (pair? list)
	   (if (eq? obj (car list))
	       list
	       (memq obj (cdr list)))))
    memq))

(define memv
  (let ()
    (define (memv obj list)
      (and (pair? list)
	   (if (eqv? obj (car list))
	       list
	       (memv obj (cdr list)))))
    memv))

(define member
  (let ((equal? equal?))
    (define (member obj list)
      (and (pair? list)
	   (if (equal? obj (car list))
	       list
	       (member obj (cdr list)))))
    member))


(define assq
  (let ()
    (define (assq obj alist)
      (and (pair? alist)
       (if (eq? obj (caar alist))
	   (car alist)
	   (assq obj (cdr alist)))))
    assq))

(define assv
  (let ()
    (define (assv obj alist)
      (and (pair? alist)
	   (if (eqv? obj (caar alist))
	       (car alist)
	       (assv obj (cdr alist)))))
    assv))

(define assoc
  (let ((equal? equal?))
    (define (assoc obj alist)
      (and (pair? alist)
	   (if (equal? obj (caar alist))
	       (car alist)
	       (assoc obj (cdr alist)))))
    assoc))

(define-syntax char-compare
  (syntax-rules ()
    ((_ name test? cvt)
     (define (name char1 char2)
       (test? (cvt char1) (cvt char2))))))
(define-syntax char-compare-protect
  (syntax-rules ()
    ((_ name test? cvt)
     (define name
       (let ((cvt cvt)
	     (test? test?))
	 (lambda (char1 char2)
	   (test? (cvt char1) (cvt char2))))))))


;;; Each requires the specified two procedures
(char-compare char=? = char->integer)
(char-compare char<? < char->integer)
(char-compare char>? > char->integer)
(char-compare char<=? <= char->integer)
(char-compare char>=? >= char->integer)
(char-compare-protect char-ci=? char=? char-upcase)
(char-compare-protect char-ci<? char<? char-upcase)
(char-compare-protect char-ci>? char>? char-upcase)
(char-compare-protect char-ci<=? char<=? char-upcase)
(char-compare-protect char-ci>=? char>=? char-upcase)

(define char-alphabetic?
  (let ((char-upper-case char-upper-case)
	(char-lower-case char-lower-care))
    (lambda (c)
      (or (char-upper-case? c)
	  (char-lower-case? c)))))

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
  
(define char-upcase
  (let ((char-lower-case char-lower-case))
    (if (char-lower-case? c)
	(integer->char (- (char->integer c) 32))
	c)))

(define char-downcase
  (let ((char-upper-case char-upper-case))
    (lambda (c)
      (if (char-upper-case? c)
	  (integer->char (+ (char->integer c) 32))
	  c))))

(define string-ci=?
  (let ((char-ci=? char-ci=?))
    (lambda (string1 string2)
      (let ((len (string-length string1)))
	(and (= len (string-length string2))
	     (let next ((i 0))
	       (or (= i len)
		   (and (char-ci=? (string-ref string1 i)
				   (string-ref string2 i))
			(next (+ i 1))))))))))

(define-syntax string-compare-ci
  (syntax-rules ()
    ((_ name lentest? chartest?)
     (define name
       (let ((lentest? lentest?)
	     (chartest? chartest?)
	     (char-ci=? char-ci=?))
	 (define (name string1 string2)
	   (let ((len1 (string-length string1))
		 (len2 (string-length string2)))
	     (let ((minlen (min len1 len2)))
	       (let next ((i 0))
		 (if (= i minlen)
		     (lentest? len1 len2)
		     (or (chartest? (string-ref string1 i)
				    (string-ref string2 i))
			 (and (char-ci=? (string-ref string1 i)
					 (string-ref string2 i))
			      (next (+ i 1))))))))))))))

(string-compare-ci string-ci<? < char-ci<?)
(string-compare-ci string-ci>? > char-ci>?)
(string-compare-ci string-ci<=? <= char-ci<=?)
(string-compare-ci string-ci>=? >= char-ci>=?)

(define (string->list s)
  (let ((len (string-len s)))
    (let next-char ((i len)
		    (lis '()))
      (if (= len 0)
	  lis
	  (next-char (- i 1) (cons (string-ref s (- i 1)) lis))))))

(define (string-copy s)
  (substring s 0 (string-length s)))

(define (vector->list v)
  (let ((len (vector-length v)))
    (let next-elt ((i len)
		   (lis '()))
      (if (= len 0)
	  lis
	  (next-elt (- i 1) (cons (vector-ref v (- i 1)) lis))))))

(define (vector-fill! v elt)
  (let ((len (vector-length v)))
    (do ((i 0 (+ i 1)))
	((= i len))
      (vector-set! v i c))))

;;;
;;; PROCEDURES (control features)
;;;
;;; Builtin are:
;;;   procedure? apply call-with-current-continuation call-with-values
;;;   dynamic-wind

(define map
  (let ((reverse reverse)
	(apply apply))
    (define map (proc list . lists)
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
    map))

(define for-each
  (let ((apply apply)
	(map map))
    (define (for-each proc . lists)
      (if (not (null? (car lists)))
	  (begin
	    (apply proc (map car lists))
	    (for-each proc (map cdr lists)))))
    for-each))

(define (force promise)
  (if (car promise)
      (cdr promise)
      (let ((val ((cdr promise))))
	(if (car promise)
	    (cdr promise)
	    (begin (set-car! promise #t)
		   (set-cdr! promise val)
		   val)))))

(define values
  (let ((apply apply)
	(call-with-current-continuation call-with-current-continuation))
    (lambda vals
      (call-with-current-continuation
       (lambda (c)
	 (apply c vals))))))

;;;
;;; EVAL
;;;
;;; Builtin are:
;;;   eval scheme-report-environment null-environment interaction-environment

;;; INPUT AND OUTPUT
;;;
;;; Builtin are
;;;   open-input-file open-output-file close-input-port close-output-port
;;;   input-port? output-port?
;;;   current-input-port current-output-port [with an arg, sets it, returns old]
;;;   read-char peek-char eof-object? char-ready? write-char
;;;
(define call-with-input-file
  (let ((open-input-file open-input-file)
	(close-input-port close-input-port))
    (lambda (str proc)
      (let ((p (open-input-file str)))
	(proc p)
	(close-input-port p)))))

(define call-with-output-file
  (let ((open-output-file open-output-file)
	(close-output-port close-output-port))
    (lambda (str proc)
      (let ((p (open-output-file str)))
	(proc p)
	(close-output-port p)))))

(define (port? p)
  (or (input-port? p)
      (output-port? p)))

(define with-input-from-file
  (let ((call-with-input-file call-with-input-file)
	(current-input-port current-input-port))
    (lambda (str proc)
      (call-with-input-file str (lambda (p)
				  (let ((old (current-input-port p)))
				    (proc)
				    (current-input-port old)))))))

(define with-output-to-file
  (let ((call-with-output-file call-with-output-file)
	(current-output-port current-output-port))
    (lambda (str proc)
      (call-with-output-file str (lambda (p)
				   (let ((old (current-output-port p)))
				     (proc)
				     (current-output-port old)))))))

;;; Requires NULL? CURRENT-INPUT-PORT CAR PEEK-CHAR MEMV STRING-APPEND
;;; STRING CHAR-DOWNCASE READ-CHAR LIST ERROR EQV? LIST->VECTOR 
;;; CHAR-ALPHABETIC? CHAR-NUMERIC? CHAR=? NOT CONS STRING-REF STRING-CI=?
;;; STRING-LENGTH STRING->NUMBER STRING->SYMBOL 
#;(define (read . port*)
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

(define write
  (let ((current-output-port current-output-port)
	(write-char write-char)
	(display display)
	(vector->list vector->list)
	(number->string number->string)
	(port? port?))
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
		 (write tail port)
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
				 (else (string #\# #\\ obj))) port))
	 ((vector? obj) (write-char #\# port) (write (vector->list obj) port))
	 ((pair? obj) 
	  (write-char #\( port)
	  (write (car obj) port) 
	  (write-rest (cdr obj)))
	 ((null? obj) (display "()" port))
	 ((number? obj) (display (number->string obj) port))
	 ((string? obj) (write-slashify obj))
	 ((procedure? obj) (display "#<procedure>" port))
	 ((eof-object? obj) (display "#<eof>" port))
	 ((port? obj) (display "#<port>" port)))))
    write))

(define display
  (let ((current-output-port current-output-port)
	(write-char write-char)
	(vector->list vector->list)
	(port? port?))
    (define (display obj . port*)
      (let ((port (if (null? port*) (current-output-port) (car port*))))
	(define (display-rest tail)
	  (cond
	   ((null? tail) (write-char #\) port))
	   ((pair? tail)
	    (write-char #\space port)
	    (display (car tail) port)
	    (display-rest (cdr tail)))
	   (else (display " . " port)
		 (display tail port)
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
	 ((string? obj) (let ((len (string-length obj)))
			  (do ((i 0 (+ i 1)))
			      ((>= i len))
			    (write-char (string-ref obj i) port))))
	 ((procedure? obj) (display "#<procedure>" port))
	 ((eof-object? obj) (display "#<eof>" port))
	 ((port? obj) (display "#<port>" port)))))
    display))
    
(define newline
  (let ((write-char write-char)
	(current-output-port current-output-port))
    (lambda port*
      (write-char #\newline 
		  (if (null? port*) (current-output-port) (car port*))))))

;;; SYSTEM INTERFACE
;;;
;;; Not implemented:
;;; load transcript-on transcript-off
