#lang racket

(require "types.rkt")
(require "lexer.rkt")
(require "expander.rkt")
(require "compiler.rkt")
(require (for-syntax pyramid/globals))

(provide read
         read-syntax
         compile-translation-unit
         expand-translation-unit
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
    (verbose-section "Lexer Hack state" VERBOSITY-HIGH
                     (print-lexer-hack-state))
    (define parse-tree (parse tokens))
    (verbose-section "Ceagle Parse Tree" VERBOSITY-HIGH
                   (pretty-print (syntax->datum parse-tree)))
    (define module-datum `(module c-mod ceagle
                            ,parse-tree))
    ;(displayln module-datum)
    (define stx (datum->syntax #f module-datum))
    ;; (displayln tokens)
    ;; (displayln all-token-types)
    ;; (pretty-print (parse-to-datum tokens))
    stx
    ))

(define-syntax (c-module-begin stx)
  (syntax-case stx ()
    ;[(_ x) #`(#%module-begin (quote #,(expand-c #'x)))]))
    [(_ x)
     #`(#%module-begin
        (provide program-info)
        (define program-info
          (let* ([ source-code (quote x)]
                 [ abstract-syntax (expand-translation-unit x)]
                 [ program (compile-translation-unit abstract-syntax)])
            (list source-code abstract-syntax program))))]
    ))
