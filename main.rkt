#lang typed/racket

(require "types.rkt")
(require "lexer.rkt")

(require brag)

(provide (all-defined-out))

; https://beautifulracket.com/jsonic/the-tokenizer.html
(module reader racket
  (provide read read-syntax)

  (define (read in)
    (syntax->datum (read-syntax #f in)))
  
  (define (read-syntax path port)
    (define parse-tree (parse path (make-tokenizer port)))
    (skip-whitespace in)
    (read-token src in))

  (define (read-token src in)
    (define-values (line col pos) (port-next-location in))
    
             
    (define token-match
      (regexp-match
       
    
(define (skip-whitespace in)
  (regexp-match #px"^\\s*" in))
