#lang typed/racket

(module reader racket
  (provide read read-syntax)

  (define (read in)
    (syntax->datum (read-syntax #f in)))
  
  (define (read-syntax src in)
    (skip-whitespace in)
    (read-token src in))

  (define (read-token src in)
    (define-values (line col pos) (port-next-location in))
    (define token-match
      (regexp-match
       #px"^\
    
(define (skip-whitespace in)
  (regexp-match #px"^\\s*" in))
