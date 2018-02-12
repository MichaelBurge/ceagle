#lang racket

(provide destruct)

(require (for-syntax racket/syntax))
(require (for-syntax racket/list))
(require (for-syntax racket/struct-info))

; 1. Locate the match macro
; https://github.com/racket/racket/blob/62f5b2c4e4cdefa18fa36275074ff9fe376ddaf3/racket/collects/racket/match/struct.rkt
; 2. How does it locate the struct-type-info for a type?
; (extract-struct-info (syntax-local-value #'struct-name))
; 3. Produce a list of names for a struct's fields

; 4. Write the macro to generate defines.

; http://www.greghendershott.com/fear-of-macros/pattern-matching.html
(define-syntax (destruct stx)
  (syntax-case stx ()
    [(_ ty id)
     (let* ([ si             (extract-struct-info (syntax-local-value #'ty))]
            [ accessors      (reverse (fourth si ))]
            [ accessor->name (λ (acc) (with-syntax ([ acc acc ])
                                        (format-id stx "~a~a" #'id (strip-prefix #'ty #'acc))))]
            [ names          (map accessor->name accessors)]
            [ make-def       (λ (name acc)
                               (with-syntax ([ acc (datum->syntax stx acc)]
                                             [ name name ])
                                 #`(define name (acc id))))]
            [ defs           (map make-def names accessors)])
       #`(begin #,@defs))]))

(begin-for-syntax
  (define (strip-prefix prefix name)
    (string->symbol
     (substring (symbol->string (syntax->datum name))
                (string-length (symbol->string (syntax->datum prefix)))))))
