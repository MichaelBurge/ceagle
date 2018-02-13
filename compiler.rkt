#lang typed/racket

(require "types.rkt")
(require pyramid/types)
(require pyramid/compiler)
(require pyramid/parser)
(require pyramid/utils)

(require "utils.rkt")

(provide compile-c)

(: *return-var*     (Parameterof (U Symbol  #f))) ; The value to be returned
(define *return-var*     (make-parameter  #f))
(: *return-point*   (Parameterof (U c-label #f))) ; The label that "return" jumps to
(define *return-point*   (make-parameter  #f))
(: *break-point*    (Parameterof (U c-label #f))) ; The label that "break" jumps to
(define *break-point*    (make-parameter  #f))
(: *continue-point* (Parameterof (U c-label #f))) ; The label that "continue" jumps to
(define *continue-point* (make-parameter  #f))

(module typechecker typed/racket
  (require "types.rkt")
  (provide (all-defined-out))
  
  (: *type-registry* (HashTable Symbol c-type))
  (define *type-registry* (make-hash))
  
  (: register-type! (-> Symbol c-type Void))
  (define (register-type! name ty)
    (hash-set! *type-registry* name ty))
  )
(require 'typechecker)

(: compile-c (-> c-unit Pyramid))
(define (compile-c x)
  (destruct c-unit x)
  (define decls
    (pyr-begin (map compile-declaration x-decls)))
  (define call-main
    (c-function-call (c-variable 'main) (list)))
  (pyr-begin (list decls (compile-statement call-main))))

(: compile-declaration (-> c-declaration Pyramid))
(define (compile-declaration x)
  (match x
    [(struct c-decl-var _)  (compile-decl-var x)]
    [(struct c-decl-type _) (compile-decl-type x)]
    [(struct c-decl-func _) (compile-decl-func x)]
    [_                      (error "compile-decl: unknown case" x)]))

(: compile-decl-var (-> c-decl-var Pyramid))
(define (compile-decl-var x)
  (destruct c-decl-var x)
  (pyr-definition x-name
                  (if x-init
                      (compile-expression x-init)
                      (pyr-const 0))))

(: compile-decl-type (-> c-decl-type Pyramid))
(define (compile-decl-type x)
  (destruct c-decl-type x)
  (register-type! x-name x-type)
  (pyr-begin (list)))

(: compile-decl-func (-> c-decl-func Pyramid))
(define (compile-decl-func x)
  (destruct c-decl-func x)
  (destruct c-signature x-sig)
  (: vars VariableNames)
  (define vars (map c-sigvar-name x-sig-args))
  (let ([ lbl-return (make-c-label x-name)]
        [ return-var (make-label-name x-name)])
    (parameterize ([ *return-var* return-var ]
                   [ *return-point* lbl-return ])
      (pyr-definition
       x-name
       (pyr-lambda
        vars
        (pyr-begin (listof
                    (pyr-definition return-var (pyr-const 0))
                    (compile-statement x-body)
                    (compile-label lbl-return))))))))

(: compile-statement (-> c-statement Pyramid))
(define (compile-statement x)
  (match x
    [(? c-label?)       (compile-label       x)]
    [(? c-expression?)  (compile-expression  x)]
    [(? c-switch?)      (compile-switch      x)]
    [(? c-if?)          (compile-if          x)]
    [(? c-for?)         (compile-for         x)]
    [(? c-while?)       (compile-while       x)]
    [(? c-do-while?)    (compile-do-while    x)]
    [(? c-goto?)        (compile-goto        x)]
    [(? c-block?)       (compile-block       x)]
    [(? c-return?)      (compile-return      x)]
    [(? c-break?)       (compile-break       x)]
    [(? c-continue?)    (compile-continue    x)]
    [(? c-declaration?) (compile-declaration x)]
    [_                (error "compile-statement: Unknown case" x)]))

(: compile-label (-> c-label Pyramid))
(define (compile-label x)
  (destruct c-label x)
  (let ([ name (make-label x-name)])
    (expand-pyramid
     `(asm (label ,name)))))

(: compile-expression (-> c-expression Pyramid))
(define (compile-expression x)
  (match x
    [(? c-const?)         (compile-const x)]
    [(? c-variable?)      (compile-variable x)]
    [(? c-ternary?)       (compile-ternary x)]
    [(? c-binop?)         (compile-binop x)]
    [(? c-unop?)          (compile-unop x)]
    [(? c-function-call?) (compile-function-call x)]
    [_                  (error "compile-expression: Unknown case" x)]))

(: compile-const         (-> c-const         Pyramid))
(define (compile-const x)
  (pyr-const (c-const-value x)))

(: compile-variable      (-> c-variable      Pyramid))
(define (compile-variable x)
  (pyr-variable (c-variable-name x)))

(: compile-ternary       (-> c-ternary       Pyramid))
(define (compile-ternary x)
  (destruct c-ternary x)
  (pyr-if (compile-expression x-pred)
          (compile-expression x-consequent)
          (compile-expression x-alternative)))

(: compile-binop         (-> c-binop         Pyramid))
(define (compile-binop x)
  (destruct c-binop x)
  (pyr-application (pyr-variable x-op)
                   (list (compile-expression x-left)
                         (compile-expression x-right))))

(: compile-unop          (-> c-unop          Pyramid))
(define (compile-unop x)
  (destruct c-unop x)
  (pyr-application (pyr-variable x-op)
                   (list (compile-expression x-exp))))

(: compile-function-call (-> c-function-call Pyramid))
(define (compile-function-call x)
  (destruct c-function-call x)
  (pyr-application (compile-expression x-func)
                   (map compile-expression x-args)))

(: compile-switch      (-> c-switch      Pyramid))
(define (compile-switch sw)
  (destruct c-switch sw)
  (: equal? (-> Pyramid Pyramid Pyramid))
  (define (equal? x y) (pyr-application (pyr-variable '=) (list x y)))
  (: compile-cases (-> c-switch-cases Pyramid))
  (define (compile-cases cases)
    (if (null? cases)
        (compile-statement sw-default)
        (let ([ hd (first cases) ]
              [ tl (rest  cases) ])
          (destruct c-switch-case hd)
          (pyr-if (equal? (compile-expression hd-expected)
                          (compile-expression sw-actual))
                  (compile-statement hd-body)
                  (compile-cases tl)))))
  (compile-cases sw-cs))

(: compile-if          (-> c-if          Pyramid))
(define (compile-if x)
  (destruct c-if x)
  (pyr-if (compile-expression x-pred)
          (compile-statement  x-consequent)
          (compile-statement  x-alternative)
          ))

(: compile-for         (-> c-for         Pyramid))
(define (compile-for x)
  (destruct c-for x)
  (define init (if x-init
                   (compile-declaration x-init)
                   `(begin)))
  (define post (if x-post
                   (compile-expression x-post)
                   `(begin)))
  (define pred (if x-pred
                   (compile-expression x-pred)
                   `1))
  (define lbl-begin    (make-c-label 'for-begin))
  (define lbl-continue (make-c-label 'for-continue))
  (define lbl-break    (make-c-label 'for-break))
  
  (expand-pyramid
   `(begin
      ,init
      (compile-label lbl-continue)
      (if ,pred
          (begin ,(compile-statement x-body)
                 ,(compile-jump lbl-continue))
          (compile-label lbl-break)))))

(: compile-while       (-> c-while       Pyramid))
(define (compile-while x)
  (destruct c-while x)
  (compile-for (c-for #f x-pred #f x-body)))

(: compile-do-while    (-> c-do-while    Pyramid))
(define (compile-do-while x)
  (destruct c-do-while x)
  (define lbl-begin    (make-c-label 'do-while-begin   ))
  (define lbl-continue (make-c-label 'do-while-continue))
  (define lbl-break    (make-c-label 'do-while-break   ))
  
  (parameterize ([ *break-point*    lbl-break]
                 [ *continue-point* lbl-continue])
    (expand-pyramid
     `(begin
        ,(compile-label     lbl-begin)
        ,(compile-statement x-body)
        ,(compile-label     lbl-continue)
        (if ,(compile-expression x-pred)
            ,(compile-jump lbl-begin)
            ,(compile-label lbl-break))))))
  
(: compile-goto        (-> c-goto        Pyramid))
(define (compile-goto x)
  (compile-jump (c-label (c-goto-target x))))

(: compile-block       (-> c-block       Pyramid))
(define (compile-block x)
  (compile-c-sequence (c-block-body x)))

(: compile-return      (-> c-return      Pyramid))
(define (compile-return x)
  (destruct c-return x)
  (define lbl (*return-point*))
  (define var (*return-var*))
  (if lbl
      (if x-val
          (if var
              (pyr-begin (listof (pyr-assign var (compile-expression x-val))
                                 (compile-jump lbl)))
              (error "compile-return: Unset return variable"))
          (compile-jump lbl))
      (error "compile-return: Unset return-point")))

(: compile-break       (-> c-break       Pyramid))
(define (compile-break x)
  (define lbl (*break-point*))
  (if lbl
      (compile-jump lbl)
      (error "compile-break: Unset break-point")))

(: compile-continue    (-> c-continue    Pyramid))
(define (compile-continue x)
  (define lbl (*continue-point*))
  (if lbl
      (compile-jump lbl)
      (error "compile-continue: unset continue-point)")))

(: compile-c-sequence (-> c-statements Pyramid))
(define (compile-c-sequence xs)
  (match (map compile-statement xs)
    [(? null?) (pyr-begin (list))]
    [(list y ) y]
    [ys        (pyr-begin ys)]))

(: compile-jump (-> c-label Pyramid))
(define (compile-jump x)
  (expand-pyramid
   `(asm (cg-goto ,(c-label-name x)))))

(: make-c-label (-> Symbol c-label))
(define (make-c-label name)
  (c-label (make-label-name name)))
