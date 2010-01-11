;;; Numbers in sinjs
;;;
;;; The following numeric formats exist:
;;;    quatnum   q:
;;;    flonum    flo:
;;;    fracnum   frac:
;;;    bignum	 big:
;;;    fixnum    fix:


;;; The following numeric types exist.  Note that a number is often
;;; part of more than one type, though only of a single format.
;;;    quaternion	  quaternion:		T       
;;;    real		  real:			X, Y    
;;;    (exact) rational   rational:		Q
;;;    integer		  integer:		N

;;; For each format and type there is a prefix used on symbols which
;;; conventionally identifies the domain of arguments.

;;; Builtin primitives:
;;; Quatnum format: q:real-part q:imag-part q:jmag-part q:kmap-part
;;;                 quatnum? make-quatnum
;;;   number?

;;;
;;; Quaternion arithmetic
;;;
(define (quaternion:complex? obj)
  (if (quatnum? obj)
      (and (real:zero? (q:jmag-part obj))
	   (real:zero? (q:kmag-part obj)))
      #t))

(define (quaternion:real? obj)
  (if (quatnum? obj)
      (and (real:zero? (q:imag-part obj))
	   (real:zero? (q:jmag-part obj))
	   (real:zero? (q:kmag-part obj)))
      #t))

(define (quaternion:integer? obj)
  (if (quatnum? obj)
      (and (real:zero? (q:imag-part obj))
	   (real:zero? (q:jmag-part obj))
	   (real:zero? (q:jmag-part obj))
	   (real:integer? (q:real-part obj)))
      (real:integer? obj)))

(define (quaternion:exact? t)
  (if (quatnum? t)
      (and (real:exact? (q:real-part obj))
	   (real:exact? (q:imag-part obj))
	   (real:exact? (q:jmag-part obj))
	   (real:exact? (q:kmag-part obj)))
      (real:exact? t)))

(define (quaternion:= t1 t2)
  (if (quatnum? t1)
      (if (quatnum? t2)
	  (and (real:= (q:real-part t1) (q:real-part t2))
	       (real:= (q:imag-part t1) (q:imag-part t2))
	       (real:= (q:jmag-part t1) (q:jmag-part t2))
	       (real:= (q:kmag-part t1) (q:kmag-part t2)))
	  (and (zero? (q:imag-part t1))
	       (zero? (q:jmag-part t1))
	       (zero? (q:kmag-part t1))
	       (real:= (q:real-part t1) (complex:real-part t2))))
      (if (quatnum? t2)
	  (and (zero? q:imag-part t2)
	       (zero? (q:jmag-part t2))
	       (zero? (q:kmag-part t2))
	       (real:= (q:imag-part t2) (complex:imag-part t1)))
	  (real:= t1 t2))))

(define (quaternion:zero? t)
  (if (quatnum? t)
      (and (real:zero? q:real-part t)
	   (real:zero? q:imag-part t)
	   (real:zero? q:jmag-part t)
	   (real:zero? q:kmap-part t))
      (real:zero? t)))

(define (quaternion:positive? t)
  (real:positive? (quaternion:real-arg t 'positive?)))

(define (quaternion:negative? t)
  (real:negative? (quaternion:real-arg t 'negative?)))

(define (quaternion:even? t)
  (real:even? (quaternion:real-arg t 'even?)))

(define (quaternion:+ t1 t2)
  (if (quatnum? t1)
      (if (quatnum? t2)
	  (make-rectangular* (real:+ (q:real-part t1) (q:real-part t2))
			     (real:+ (q:imag-part t1) (q:imag-part t2))
			     (real:+ (q:jmag-part t1) (q:jmag-part t2))
			     (real:+ (q:kmag-part t1) (q:kmag-part t2)))
	  (make-rectangular* (real:+ (q:real-part t1) t2)
			     (q:imag-part t1)
			     (q:jmag-part t1)
			     (q:kmag-part t1)))
      (if (quatnum? t2)
	  (make-rectangular* (real:+ t1 (q:real-part t2))
			     (q:imag-part t2)
			     (q:jmag-part t2)
			     (q:kmag-part t2))
	  (real:+ t1 t2))))

(define (quaternion:- t1 t2)
  (if (quatnum? t1)
      (if (quatnum? t2)
	  (make-rectangular* (real:- (q:real-part t1) (q:real-part t2))
			     (real:- (q:imag-part t1) (q:imag-part t2))
			     (real:- (q:jmag-part t1) (q:jmag-part t2))
			     (real:- (q:kmag-part t1) (q:kmag-part t2)))
	  (make-rectangular* (real:- (q:real-part t1) t2)
			     (q:imag-part t1)
			     (q:jmag-part t1)
			     (q:kmag-part t1)))
      (if (quatnum? t2)
	  (make-rectangular* (real:- t1 (q:real-part t2))
			     (real:negate (q:imag-part t2))
			     (real:negate (q:jmag-part t2))
			     (real:negate (q:kmag-part t2)))
	  (real:- t1 t2))))

(define (quaternion:negate t)
  (if (quatnum? t)
      (make-rectangular* (real:negate (q:real-part t))
			 (real:negate (q:imag-part t))
			 (real:negate (q:jmag-part t))
			 (real:negate (q:kmag-part t)))
      (real:negate t)))

(define (quaternion:* t1 t2)
  (if (quatnum? t1)
      (if (quatnum? t2)
	  (make-rectangular* 
	   (real:- (real:- (real:* (q:real-part t1) (q:real-part t2))
			   (real:* (q:imag-part t1) (q:imag-part t2)))
		   (real:+ (real:* (q:jmag-part t1) (q:jmag-part t2))
			   (real:* (q:kmag-part t1) (q:kmag-part t2))))
	   (real:+ (real:+ (real:* (q:real-part t1) (q:imag-part t2))
			   (real:* (q:imag-part t1) (q:real-part t2)))
		   (real:- (real:* (q:jmag-part t1) (q:kmag-part t2))
			   (real:* (q:kmag-part t1) (q:jmag-part t2))))
	   (real:+ (real:+ (real:* (q:real-part t1) (q:jmag-part t2))
			   (real:* (q:jmag-part t1) (q:real-part t2)))
		   (real:- (real:* (q:kmag-part t1) (q:imag-part t2))
			   (real:* (q:imag-part t1) (q:kmag-part t2))))
	   (real:+ (real:+ (real:* (q:real-part t1) (q:kmag-part t2))
			   (real:* (q:kmag-part t1) (q:real-part t2)))
		   (real:- (real:* (q:imag-part t1) (q:jmag-part t2))
			   (real:* (q:jmag-part t1) (q:imag-part t2)))))
	  (make-rectangular* (real:* (q:real-part t1) t2)
			     (real:* (q:imag-part t1) t2)
			     (real:* (q:jmag-part t1) t2)
			     (real:* (q:kmag-part t1) t2)))
      (if (quatnum? t2)
	  (make-rectangular* (real:* t1 (q:real-part t2))
			     (real:* t1 (q:imag-part t2))
			     (real:* t1 (q:jmag-part t2))
			     (real:* t1 (q:kmag-part t2)))
	  (real:* t1 t2))))

(define (quaternion:reciprocal t)
  (if (quatnum? t)
      (let ((scale (real:+ (real:+ (real:* (q:real-part t) (q:real-part t))
				   (real:* (q:imag-part t) (q:imag-part t)))
			   (real:+ (real:* (q:jmag-part t) (q:jmag-part t))
				   (real:* (q:kmag-part t) (q:kmag-part t))))))
	(make-rectangular* (real:/ (q:real-part t) scale)
			   (real:/ (real:negate (q:imag-part t)) scale)
			   (real:/ (real:negate (q:jmag-part t)) scale)
			   (real:/ (real:negate (q:kmag-part t)) scale)))
      (real:reciprocal t)))

;;; This is right-ways division: (/ t1 t2) => (* t1 (reciprocal t2))
;;; which is easy to understand when combined with Scheme's left-associativity
;;; of many-argument /.
(define (quaternion:/ t1 t2)
  (if (quatnum? t2)
      (quaternion:* t1 (quaternion:reciprocal t2))
      (if (quatnum? t1)
	  (make-rectangular* (real:/ (q:real-part t1) t2)
			     (real:/ (q:imag-part t1) t2)
			     (real:/ (q:jmag-part t1) t2)
			     (real:/ (q:kmag-part t1) t2))
	  (real:/ t1 t2))))

(define (quaternion:abs t)
  (real:abs (quaternion:real-arg t 'abs)))

(define (quaternion:quotient t1 t2)
  (real:quotient (quaternion:real-arg t1 'quotient)
		 (quaternion:real-arg t2 'quotient)))

(define (quaternion:remainder t1 t2)
  (real:remainder (quaternion:real-arg t1 'remainder)
		  (quaternion:real-arg t2 'remainder)))

(define (quaternion:modulo t1 t2)
  (real:modulo (quaternion:real-arg t1 'modulo)
	       (quaternion:real-arg t2 'modulo)))

(define (quaternion:gcd t1 t2)
  (real:gcd (quaternion:real-arg t1 'gcd)
	    (quaternion:real-arg t2 'gcd)))

(define (quaternion:lcm t1 t2)
  (real:lcm (quaternion:real-arg t1 'lcm)
	    (quaternion:real-arg t2 'lcm)))

(define (quaternion:numerator t)
  (real:numerator (quaternion:real-arg t 'numerator)))

(define (quaternion:denominator t)
  (real:denominator (quaternion:real-arg t 'denominator)))

(define (quaternion:floor t)
  (real:floor (quaternion:real-arg t 'floor)))

(define (quaternion:ceiling t)
  (real:ceiling (quaternion:real-arg t 'ceiling)))

(define (quaternion:truncate t)
  (real:truncate (quaternion:real-arg t 'truncate)))

(define (quaternion:round t)
  (real:round (quaternion:real-arg t 'round)))

(define (quaternion:rationalize t1 t2)
  (real:rationalize (quaternion:real-arg t1 'rationalize)
		    (quaternion:real-arg t2 'rationalize)))

(define (quaternion:exp t)
  (if (quatnum? t)
      (let ((mag (quaternion:magnitude (quaternion:vector-part t))))
	(quaternion:* (real:exp (q:real-part t))
		      (quaternion:+ (real:cos mag)
				    (quaternion:* (quaternion:unit-vector t)
						  (real:sin mag)))))
      (real:exp t)))

(define (quaternion:log t)
  (if (quatnum? t)
      (quaternion:+ (quaternion:* 1/2 (real:log (quaternion:magnitude t)))
		    (quaternion:* (quaternion:unit-vector t)
				  (real:atan (quaternion:magnitude 
					      (quaternion:vector-part t))
					     (q:real-part t))))
      (real:log t)))
      
(define (quaternion:sin t)
  (if (quatnum? t)
      (let ((mag (quaternion:magnitude (quaternion:vector-part t))))
	(quaternion:+ (real:* (real:sin (q:real-part t))
			      (real:cosh mag))
		      (quaternion:* (quaternion:unit-vector t)
				    (real:* (real:cos (q:real-part t))
					    (real:sinh mag)))))
      (real:sin t)))

(define (quaternion:cos t)
  (if (quatnum? t)
      (let ((mag (quaternion:magnitude (quaternion:vector-part t))))
	(quaternion:+ (real:* (real:cos (q:real-part t))
			      (real:cosh mag))
		      (quaternion:* (quaternion:unit-vector t)
				    (real:* (real:sin (q:real-part t))
					    (reas:sinh mag)))))
      (real:cos t)))

(define (quaternion:tan t)
  (if (quatnum? t)
      (quaternion:/ (quaternion:sin t) (quaternion:cos t))
      (real:tan t)))

(define (quaternion:asin t)
  (if (quatnum? t)
      (let ((vec (unit-vector t)))
	(quaternion:negate
	 (quaternion:* vec
		       (quaternion:log
			(quaternion:+ (quaternion:* vec t)
				      (quaternion:sqrt
				       (quaternion:- 1 
						     (quaternion:* t t))))))))
      (real:asin t)))

(define (quaternion:acos t)
  (if (quatnum? t)
      (let ((vec (unit-vector t)))
	(quaternion:negate
	 (quaternion:* vec
		       (quaternion:log
			(quaternion:+ t
				      (quaternion:sqrt
				       (quaternion:- (quaternion:* t t)
						     1)))))))
      (real:acos t)))

(define (quaternion:atan t)
  (if (quatnum? t)
      (let ((vec (unit-vector t)))
	(quaternion:* (quaternion:* 1/2 vec)
		      (quaternion:log
		       (quaternion:* (quaternion:+ t vec)
				     (quaternion:reciprocal
				      (quaternion:- vec t))))))
      (real:atan t)))

(define (quaternion:sqrt t)
  (if (quatnum? t)
      (quaternion:expt t 1/2)
      (real:sqrt t)))

(define (quaternion:expt t1 t2)
  (if (or (quatnum? t1) (quatnum? t2))
      (quaternion:exp (quaternion:* (quaternion:log t1)
				    t2))
      (real:expt t1 t2)))

(define (make-rectangular* real imag jmag kmag)
  (if (and (real:zero? imag)
	   (real:zero? jmag)
	   (real:zero? kmag))
      (if (or (real:inexact? imag)
	      (real:inexact? jmag)
	      (real:inexact? kmag))
	  (real:exact->inexact real)
	  real)
      (make-quatnum real imag jmag kmag)))

(define (quaternion:real-part t)
  (if (quatnum? t)
      (q:real-part t)
      t))

(define (quaternion:imag-part t)
  (if (quatnum? t)
      (q:imag-part t)
      0))

(define (quaternion:jmag-part t)
  (if (quatnum? t)
      (q:jmag-part t)
      0))

(define (quaternion:kmag-part t)
  (if (quatnum? t)
      (q:kmag-part t)
      0))

(define (quaternion:magnitude t)
  (if (quatnum? t)
      (real:sqrt (real:+ (real:+ (real:* (q:real-part t) (q:real-part t))
				 (real:* (q:imag-part t) (q:imag-part t)))
			 (real:+ (real:* (q:jmag-part t) (q:jmag-part t))
				 (real:* (q:kmag-part t) (q:kmag-part t)))))
      (real:abs t)))

(define (quaternion:angle t)
  (if (quatnum? t)
      (real:atan (real:/ (real:sqrt (real:+ (real:* (q:imag-part t) 
						    (q:imag-part t))
					    (real:+ (real:* (q:jmag-part t)
							    (q:jmag-part t))
						    (real:* (q:kmag-part t)
							    (q:kmag-part t)))))
			 (q:real-part t)))
      0))

(define (quaternion:colatitude t)
  (if (quatnum? t)
      (real:atan (real:/ (real:sqrt (real:+ (real:* (q:jmag-part t)
						    (q:jmag-part t))
					    (real:* (q:kmag-part t)
						    (q:kmag-part t))))
			 (q:imag-part t)))
      0))

(define (quaternion:longitude t)
  (if (quatnum? t)
      (real:atan (real:/ (q:kmag-part t) (q:jmag-part t)))
      0))

(define (quaternion:vector-part t)
  (if (quatnum? t)
      (make-rectangular* 0
			 (q:imag-part t)
			 (q:jmag-part t)
			 (q:kmag-part t))
      0))

(define (quaternion:unit-vector t)
  (if (quatnum? t)
      (quaternion:* (quaternion:vector-part t)
		    (real:reciprocal (quaternion:magnitude t)))
      0))

(define (quaternion:exact->inexact t)
  (if (quatnum? t)
      (make-rectangular* (real:exact->inexact (q:real-part t))
			 (real:exact->inexact (q:imag-part t))
			 (real:exact->inexact (q:jmag-part t))
			 (real:exact->inexact (q:kmag-part t)))
      (real:exact->inexact t)))

(define (quaternion:inexact->exact t)
  (if (quatnum? t)
      (make-rectangular* (real:inexact->exact (q:real-part t))
			 (real:inexact->exact (q:imag-part t))
			 (real:inexact->exact (q:jmag-part t))
			 (real:inexact->exact (q:kmag-part t)))
      (real:inexact->exact t)))

(define (quaternion:real-arg t name)
  (if (quatnum? t)
      (if (and (real:zero? (q:imag-part t))
	       (real:zero? (q:jmag-part t))
	       (real:zero? (q:kmag-part t)))
	  (q:real-part t)
	  (num-error name "real number" t))
      t))

;;;
;;; Real arithmetic
;;;
(define (real:integer? x)
  (if (flonum? x)
      (real:= x (real:floor x))
      (rational:integer? x)))

(define (real:exact? x)
  (not (flonum? x)))

;;; comparisons are unusual: for mixed-mode arguments, they promote
;;; flonum into fractions.  this guarantees the required transitivity
;;; of the predicates.
(define (real:= x1 x2)
  (if (flonum? x1)
      (if (flonum? x2)
	  (flo:= x1 x2)
	  (rational:= (flo:->rational x1) x2))
      (if (flonum? x2)
	  (rational:= x1 (flo:->rational x2))
	  (rational:= x1 x2))))

(define (real:< x1 x2)
  (if (flonum? x1)
      (if (flonum? x2)
	  (flo:< x1 x2)
	  (rational:< (flo:->ratnum x1) x2))
      (if (flonum? x2)
	  (rational:< x1 (flo:->ratnum x2))
	  (rational:< x1 x2))))

(define (real:zero? x)
  (if (flonum? x)
      (flo:zero? x)
      (rational:zero? x)))

(define (real:positive? x)
  (if (flonum? x)
      (flo:positive? x)
      (rational:positive? x)))

(define (real:negative? x)
  (if (flonum? x)
      (flo:negative? x)
      (rational:negative? x)))

(define (real:even? x)
  (rational:even? (if (flonum? x)
		      (flo:->rational x)
		      x)))

(define (real:+ x1 x2)
  (if (flonum? x1)
      (if (flonum? x2)
	  (flo:+ x1 x2)
	  (flo:+ x1 (rational:->flonum x2)))
      (if (flonum? x2)
	  (flo:+ (rational:->flonum x1) x2)
	  (rational:+ x1 x2))))

(define (real:* x1 x2)
  (if (flonum? x1)
      (if (flonum? x2)
	  (flo:* x1 x2)
	  (if (rational:zero? x2)
	      x2			;exact value
	      (flo:* x1 (rational:->flonum x2))))
      (if (flonum? x2)
	  (if (rational:zero? x1)
	      x1			;exact value
	      (flo:* (rational:->flonum x1) x2))
	  (rational:* x1 x2))))

(define (real:- x1 x2)
  (if (flonum? x1)
      (if (flonum? x2)
	  (flo:- x1 x2)
	  (flo:- x1 (rational:->flonum x2)))
      (if (flonum? x2)
	  (flo:- (rational:->flonum x1) x2)
	  (rational:- x1 x2))))

(define (real:negate x)
  (if (flonum? x)
      (flo:negate x)
      (rational:negate x)))

(define (real:/ x1 x2)
  (if (flonum? x1)
      (if (flonum? x2)
	  (flo:/ x1 x2)
	  (flo:/ x1 (rational:->flonum x2)))
      (if (flonum? x2)
	  (if (rational:zero? x1)
	      x1			;exact value
	      (flo:/ (rational:->flonum x1) x2))
	  (rational:/ x1 x2))))

(define (real:reciprocal x)
  (if (flonum? x)
      (flo:reciprocal x)
      (rational:reciprocal x)))

(define (real:abs x)
  (if (flonum? x)
      (flo:abs x)
      (rational:abs x)))

(define (real:quotient x y)
  (rational:quotient (if (flonum? x)
			 (flo:->rational x)
			 x)
		     (if (flonum? y)
			 (flo:->rational y)
			 y)))

(define (real:remainder x y)
  (rational:remainder (if (flonum? x)
			  (flo:->rational x)
			  x)
		      (if (flonum? y)
			  (flo:->rational y)
			  y)))

(define (real:modulo x y)
  (rational:modulo (if (flonum? x)
		       (flo:->rational x)
		       x)
		   (if (flonum? y)
		       (flo:->rational y)
		       y)))

(define (real:gcd x y)
  (rational:gcd (if (flonum? x)
		    (flo:->rational x)
		    x)
		(if (flonum? y)
		    (flo:->rational y)
		    y)))


(define (real:lcm x y)
  (rational:lcm (if (flonum? x)
		    (flo:->rational x)
		    x)
		(if (flonum? y)
		    (flo:->rational y)
		    y)))

(define (real:numerator x)
  (if (flonum? x)
      (flo:numerator x)
      (rational:numerator x)))

(define (real:denominator x)
  (if (flonum? x)
      (flo:denominator x)
      (rational:denominator x)))

(define (real:floor x)
  (if (flonum? x)
      (flo:floor x)
      (rational:floor x)))

(define (real:ceiling x)
  (if (flonum? x)
      (flo:ceiling x)
      (rational:ceiling x)))

(define (real:truncate x)
  (if (flonum? x)
      (flo:truncate x)
      (rational:truncate x)))

(define (real:round x)
  (if (flonum? x)
      (flo:round x)
      (rational:round x)))

(define (real:exp x)
  (if (flonum? x)
      (flo:exp x)
      (if (rational:zero? x)
	  1				;exact value
	  (flo:exp (rational:->flonum x)))))

(define (real:log x)
  (if (flonum? x)
      (flo:log x)
      (if (rational:= x 1)
	  0				;exact value
	  (flo:log (rational:->flonum x)))))

(define (real:sin x)
  (if (flonum? x)
      (flo:sin x)
      (if (rational:zero? x)
	  x				;exact value
	  (flo:sin (rational:->flonum x)))))

(define (real:cos x)
  (if (flonum? x)
      (flo:cos x)
      (if (rational:zero? x)
	  1				;exact value
	  (flo:cos (rational:->flonum x)))))

(define (real:tan x)
  (if (flonum? x)
      (flo:tan x)
      (if (rational:zero? x)
	  x				;exact value
	  (flo:tan (rational:->flonum x)))))

(define (real:asin x)
  (if (flonum? x)
      (flo:asin x)
      (if (zero? x)
	  x				;exact value
	  (flo:asin (rational:->flonum x)))))

(define (real:acos x)
  (if (flonum? x)
      (flo:acos x)
      (if (rational:=? x 1)
	  0				;exact value
	  (flo:acos (rational:->flonum x)))))

(define (real:atan x)
  (if (flonum? x)
      (flo:atan x)
      (if (rational:zero? x)
	  x				;exact value
	  (flo:atan (rational:->flonum x)))))

(define (real:sqrt x)
  (if (flonum? x)
      (flo:sqrt x)
      (let* ((root (flo:sqrt (rational:->flonum x)))
	     (exactified (flo:->rational root)))
	(if (rational:= (rational:* exactified exactified)
			x)
	    exactified			;exact value
	    root))))

;;; you might think that if X1 is exactly zero, we know the answer
;;; is exactly zero, but if X2 is zero, then the result should be one.
;;; and as a result, if X2 is inexact, we cannot know the answer even if
;;; X1 is exactly zero.
(define (real:expt x1 x2)
  (if (flonum? x1)
      (if (flonum? x2)
	  (flo:expt x1 x2)
	  (if (rational:zero? x2)
	      1				;exact value
	      (flo:expt x1 (rational:->flonum x2))))
      (if (flonum? x2)
	  (flo:expt (rational:->flonum x1) x2)
	  (rational:expt x1 x2))))

(define (real:exact->inexact x)
  (if (flonum? x)
      x
      (rational:->flonum x)))

(define (real:inexact->exact x)
  (if (flonum? x)
      (flo:->rational x)
      x))

;;;
;;; Rational arithmetic
;;;
;;; Much here hinges upon the rule that numerators of zero
;;; and denominators of one are always normalized away.

(define (rational:integer? q)
  (not (fracnum? q)))

(define (rational:= q1 q2)
  (or (and (fracnum? q1)
	   (fracnum? q1)
	   (integer:= (frac:numerator q1) (frac:numerator q1))
	   (integer:= (frac:denominator q1) (frac:denominator q1)))
      (integer:= q1 q2)))

(define (rational:< q1 q2)
  (if (fracnum? q1)
      (if (fracnum? q2)
	  (integer:< (integer:* (frac:numerator q1) (frac:denominator q2))
		     (integer:* (frac:numerator q2) (frac:denominator q1)))
	  (integer:< (frac:numerator q1) (integer:* q2 (frac:denominator q1))))
      (if (fracnum? q2)
	  (integer:< (integer:* q1 (frac:denominator q2)) (frac:numerator q2))
	  (integer:< q1 q2))))

(define (rational:zero? q)
  (and (not (fracnum? q))
       (integer:zero? q)))

(define (rational:positive? q)
  (integer:positive? (if (fracnum? q) (frac:numerator q) q)))

(define (rational:negative? q)
  (integer:negative? (if (fracnum? q) (frac:numerator q) q)))

(define (rational:even? q)
  (if (fracnum? q)
      (num-error 'even? "integer" q)
      (integer:even? q)))

(define (rational:+ q1 q2)
  (if (fracnum? q1)
      (if (fracnum? q2)
	  (quotient* (integer:+ (integer:* (frac:numerator q1)
					   (frac:denominator q2))
				(integer:* (frac:numerator q2)
					   (frac:denominator q1)))
		     (integer:* (frac:denominator q1)
				(frac:denominator q2)))
	  (quotient* (integer:+ (frac:numerator q1)
				(integer:* q2 (frac:denominator q1)))
		     (frac:denominator q1)))
      (if (fracnum? q2)
	  (quotient* (integer:+ (integer:* q1 (frac:denominator q2))
				(frac:numorator q2))
		     (frac:denominator q2))
	  (integer:+ q1 q2))))

(define (rational:* q1 q2)
  (if (fracnum? q1)
      (if (fracnum? q2)
	  (quotient* (integer:* (frac:numerator q1) (frac:numerator q2))
		     (integer:* (frac:denominator q1) (frac:denominator q2)))
	  (quotient* (integer:* (frac:numerator q1) q2)
		     (frac:denominator q1)))
      (if (fracnum? q2)
	  (quotient* (integer:* q1 (frac:numerator q2))
		     (frac:denominator q2))
	  (integer:* q1 q2))))

(define (rational:negate q)
  (if (fracnum? q)
      (make-frac (integer:negate (frac:numerator q))
		 (frac:denominator q))
      (integer:negate q)))

(define (rational:- q1 q2)
  (if (fracnum? q1)
      (if (fracnum? q2)
	  (quotient* (integer:- (integer:* (frac:numerator q1)
					   (frac:denominator q2))
				(integer:* (frac:numerator q2)
					   (frac:denominator q1)))
		     (integer:* (frac:denominator q1)
				(frac:denominator q2)))
	  (quotient* (integer:- (frac:numerator q1)
				(integer:* q2 (frac:denominator q1)))
		     (frac:denominator q1)))
      (if (fracnum? q2)
	  (quotient* (integer:- (integer:* q1 (frac:denominator q2))
				(frac:numerator q2))
		     (frac:denominator q2))
	  (integer:- q1 q2))))

(define (rational:reciprocal q)
  (if (fracnum? q)
      (if (integer:= (frac:numerator q) 1)
	  (frac:denominator q)
	  (make-frac (frac:denominator q) (frac:numerator q)))
      (if (integer:zero? q)
	  (num-error '/ "non-zero number" q)
	  (make-frac 1 q))))

(define (rational:/ q1 q2)
  (if (fracnum? q1)
      (if (fracnum? q2)
	  (quotient* (integer:* (frac:numerator q1) (frac:denominator q2))
		     (integer:* (frac:denominator q1) (frac:numerator q2)))
	  (quotient* (frac:numerator q1)
		     (integer:* (frac:denominator q1) q2)))
      (if (fracnum? q2)
	  (quotient* (integer:* q1 (frac:denominator q2))
		     (frac:numerator q2))
	  (quotient* q1 q2))))

(define (rational:abs q)
  (if (fracnum? q)
      (make-frac (integer:abs (frac:numerator q))
		 (frac:denominator q))
      (integer:abs q)))

(define (rational:quotient q1 q2)
  (if (fracnum? q1)
      (num-error 'quotient "integer" q1)
      (if (fracnum? q2)
	  (num-error 'quotient "integer" q2)
	  (integer:quotient q1 q2))))

(define (rational:remainder q1 q2)
  (if (fracnum? q1)
      (num-error 'remainder "integer" q1)
      (if (fracnum? q2)
	  (num-error 'remainder "integer" q2)
	  (integer:remainder q1 q2))))

(define (rational:modulo q1 q2)
  (if (fracnum? q1)
      (num-error 'modulo "integer" q1)
      (if (fracnum? q2)
	  (num-error 'modulo "integer" q2)
	  (integer:modulo q1 q2))))

(define (rational:gcd q1 q2)
  (if (fracnum? q1)
      (num-error 'gcd "integer" q1)
      (if (fracnum? q2)
	  (num-error 'gcd "integer" q2)
	  (integer:gcd q1 q2))))

(define (rational:lcm q1 q2)
  (if (fracnum? q1)
      (num-error 'lcm "integer" q1)
      (if (fracnum? q2)
	  (num-error 'lcm "integer" q2)
	  (integer:lcm q1 q2))))

(define (rational:numerator q)
  (if (fracnum? q)
      (frac:numerator q)
      q))

(define (rational:denominator q)
  (if (fracnum? q)
      (frac:denominator q)
      1))

(define (rational:floor q)
  (if (fracnum? q)
      (if (negative? q)
	  (integer:- (rational:truncate q) 1)
	  (rational:truncate q))
      q))

(define (rational:ceiling q)
  (if (fracnum? q)
      (if (positive? q)
	  (integer:+ (rational:truncate q) 1)
	  (rational:truncate q))
      q))

(define (rational:truncate q)
  (if (fracnum? q)
      (integer:quotient (frac:numerator q) (frac:denominator q))
      q))

(define (rational:round q)
  (define (pos-frac q)
    (let ((rem (integer:remainder (frac:numerator q) (frac:denominator q)))
	  (qt (integer:quotient (frac:numerator q) (frac:denominator q))))
      (if (or (integer:< rem (integer:* 2 (frac:denominator q)))
	      (and (integer:= (frac:denominator q) 2)
		   (even? qt)))
	  qt
	  (+ qt 1)))

  (if (fracnum? q)
      (if (positive? q)
	  (pos-frac q)
	  (rational:negate (pos-frac (rational:negate q))))
      q)))

(define (rational:expt q1 q2)
  xxx)

