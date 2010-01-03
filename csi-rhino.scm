(load "compile")
(load "expand")
(load "cps")
(load "simplify")
(load "toplevel")

;;;
;;; A chicken-scheme program which has a sinjs REPL that
;;; interfaces with Rhino.
;;;
;;; We compile here, but do all the rest there.

(define (csi-rhino-repl)
  (call-with-output-pipe 
    "js rhino-evaller.js"
    (lambda (js-connector)
      (define (send-out str)
	#;(display str)
	#;(newline)
	(display str js-connector)
	(newline js-connector)
	(flush-output js-connector))

      (define (compile-and-run-1 prepared k)
	(let ((transformed (cps-transform prepared k)))
	  (let-values (((g-set!s l-set!s) 
			(find-modifications (list transformed))))
	    (let ((compiled (compile-form (simplify transformed
						    '() l-set!s))))
	      (send-out (string-append compiled ";"))))))

      (define (compile-and-run form k)
	(let-values ((forms (prepare form)))
	  (cond
	   ((= (length forms) 1) (compile-and-run-1 (car forms) k))
	   ((> (length forms) 1)
	    (compile-and-run-1 `(begin ,@forms) k)))))

      (send-out "load('runtime.js')\n")
      (send-out "load('rhino.js')\n")
      (send-out "rhino_initialize();")
      (call-with-input-file "library.scm"
	(lambda (lib)
	  (let next ((expr (read lib)))
	    (if (not (eof-object? expr))
		(begin
		  ;(display expr) (newline)
		  (compile-and-run expr 'sinjs_repl_noprint_k)
		  (next (read lib)))))))
      (let repl ()
	(display "sinjs> ")
	(flush-output)
	(let ((expr (read)))
	  (if (not (eof-object? expr))
	      (begin
		(compile-and-run expr 'sinjs_repl_k)
		(sleep 1)
		(repl))))))))
