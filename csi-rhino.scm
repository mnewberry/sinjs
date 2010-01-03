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

      (send-out "load('runtime.js')\n")
      (send-out "load('rhino.js')\n")
      (send-out "rhino_initialize();")
      (let repl ()
	(display "sinjs> ")
	(flush-output)
	(let ((expr (read)))
	  (if (not (eof-object? expr))
	      (let ((transformed  (cps-transform (prepare expr) 'sinjs_repl_k)))
		(let-values (((g-set!s l-set!s) 
			      (find-modifications (list transformed))))
		  (let ((compiled (compile-form (simplify transformed
							  #f l-set!s))))
		    (send-out (string-append compiled ";"))
		    (sleep 1)
		    (repl))))))))))
