#lang racket

(require "lexer.rkt")

(provide read read-syntax)

(define (read in)
  (syntax->datum (read-syntax #f in)))

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer-port)))
