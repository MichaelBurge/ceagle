#lang racket

(require "types.rkt")
(require "lexer.rkt")
(require "expander.rkt")
(require "compiler.rkt")

(provide read
         read-syntax
         compile-c
         expand-c
         (all-from-out "types.rkt")

         (rename-out [ c-module-begin #%module-begin ]))

(module reader racket
  (require "lexer.rkt")
  (require "parser.rkt")
  (require pyramid/io)
  (require pyramid/globals)
  (require racket/pretty)

  (provide read read-syntax)

  (define (read in)
    (syntax->datum (read-syntax #f in)))

  (define (read-syntax path port)
    (define tokens (tokenize-all port))
    (verbose-section "Ceagle Tokens" VERBOSITY-MEDIUM
                   (pretty-print tokens))
    (define parse-tree (parse tokens))
    (verbose-section "Ceagle Parse Tree" VERBOSITY-MEDIUM
                   (pretty-print (syntax->datum parse-tree)))
    (define module-datum `(module c-mod ceagle
                            ,parse-tree))
    ;(displayln module-datum)
    (define stx (datum->syntax #f module-datum))
    stx
    ;; (displayln tokens)
    ;; (displayln all-token-types)
    ;; (pretty-print (parse-to-datum tokens))
    ))

(define-syntax (c-module-begin stx)
  (syntax-case stx ()
    ;[(_ x) #`(#%module-begin (quote #,(expand-c #'x)))]))
    [(_ x)
     #`(#%module-begin
              (provide program)
              (define program (compile-c (expand-c #'x))))]
    ))
