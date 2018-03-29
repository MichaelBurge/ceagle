#lang racket

(require "types.rkt")
(require "lexer.rkt")
(require "expander.rkt")
(require "compiler.rkt")
(require (submod "compiler.rkt" typechecker))
(require (submod pyramid/types pyramidc))
(require (for-syntax pyramid/globals))
(require (for-syntax racket/list))
(require (for-syntax racket/pretty))
(require (for-syntax racket/syntax))
(require racket/syntax)

(provide read
         read-syntax
         compile-translation-unit
         expand-translation-unit
         (all-from-out "types.rkt")

         #%datum
         (rename-out [ c-module-begin #%module-begin]))

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
    ; (displayln module-datum)
    (define stx (datum->syntax #f module-datum))
    ;; (displayln tokens)
    ;; (displayln all-token-types)
    ;; (pretty-print (parse-to-datum tokens))
    stx
    ))

(begin-for-syntax
  ; https://github.com/mbutterick/beautiful-racket/blob/master/beautiful-racket-lib/br/private/syntax-flatten.rkt
  (define (syntax-flatten stx)
    (let* ([stx-unwrapped (syntax-e stx)]
           [maybe-pair (and (pair? stx-unwrapped) (flatten stx-unwrapped))])
      (if maybe-pair
          (append-map syntax-flatten maybe-pair)
          (list stx))))

  (define (find-property which stxs)
    (define ret (for/list ([ stx (in-list (syntax-flatten stxs)) ]
               #:when (syntax-property stx which))
                  stx))
    ret
    )
  (define (require-stxs parse-tree)
    (define count 0)
    (define (mk-tu n) (format-id parse-tree "make-tu~a" n))
    (define ret (for/list ([ stx (in-list (find-property 'require_system parse-tree))]
                           [ n (in-naturals)])
                  (set! count (+ count 1))
                  #`(require (rename-in (lib #,stx) [ make-translation-unit #,(mk-tu n)]))))
    (set! ret (append ret (list #`(define dependencies (list #,@(for/list ([ n (in-range count) ])
                                                                  #`(#,(mk-tu n) #f)))))))
    ret
    )
  )

(define-syntax (c-module-begin stx)
  (syntax-case stx ()
    ;[(_ x) #`(#%module-begin (quote #,(expand-c #'x)))]))
    [(_ x)
     #`(#%module-begin
        #,@(require-stxs #'x)
        (provide make-translation-unit)
        (define (make-translation-unit execute?)
          (define abstract-syntax (expand-translation-unit x))
          (define compiled (compile-translation-unit abstract-syntax execute?))
          (translation-unit 'ceagle
                            (quote x)
                            abstract-syntax
                            compiled
                            dependencies
                            ))

        )]))
