#lang racket

(require "lexer.rkt")
(require "parser.rkt")

(require racket/pretty)

;(require br-parser-tools)

(provide read read-syntax)

(define (read in)
  (syntax->datum (read-syntax #f in)))

(define (read-syntax path port)
  (define tokens (tokenize-all port))
  (define parse-tree (parse tokens))
  (define module-datum `(module c-mod "expander.rkt"
                          ,parse-tree))
  (datum->syntax #f module-datum)
  ;; (displayln tokens)
  ;; (displayln all-token-types)
  ;(pretty-print (parse-to-datum tokens))
  )
  ;(define parse-tree (parse path (make-tokenizer-port)))
