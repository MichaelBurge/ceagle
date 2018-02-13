#lang typed/racket

(require "types.rkt")
(require "lexer.rkt")
(require "expander.rkt")

(provide read read-syntax #%module-begin)

(module reader racket
  (require "lexer.rkt")
  (require "parser.rkt")
  
  (provide read read-syntax)

  (define (read in)
    (syntax->datum (read-syntax #f in)))

  (define (read-syntax path port)
    (define tokens (tokenize-all port))
    (define parse-tree (parse tokens))
    (define module-datum `(module c-mod "expander.rkt"
                            ,parse-tree))
    (define stx (datum->syntax #f module-datum))
    stx
    ;; (displayln tokens)
    ;; (displayln all-token-types)
    ;(pretty-print (parse-to-datum tokens))
    ))
