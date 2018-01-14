#lang racket

(module reader racket
  (require mario/semantics)
  (require syntax/strip-context)

  (provide (rename-out [mario-read read]
                       [mario-read-syntax read-syntax]))

  (define (mario-read in)
    (syntax->datum (mario-read-syntax #f in)))

  (define (mario-read-syntax src in)
    (with-syntax ([world (parse-world (port->string in))])
      (strip-context
       #'(module mario racket
           (require mario/semantics)
	   (define data world)
	   (eval-program (world->cell data) (new-env)))))))
