#lang typed/racket/no-check

(require "types.rkt")

(provide (all-defined-out))

; These expanders/shrinkers are mainly useful for serialization or printing smaller representations of values.

(: expand-type-registry (-> CQ TypeRegistry))
(define (expand-type-registry x)
  (assert x list?)
  (: ret TypeRegistry)
  (define ret (make-hash))
  (for ([ p x ])
    (hash-set! ret (car p) (cdr p)))
  ret)

(: shrink-type-registry (-> TypeRegistry CQ))
(define (shrink-type-registry x)
  (: pairs (Listof (Pairof Symbol c-type)))
  (define pairs (for/list : (Listof (Pairof Symbol c-type))
                    ([ p (hash->list x) ])
                  (cons (car p) (shrink-type (cdr p)))))
  `(,@pairs))

(: expand-type (-> CQ c-type))
(define (expand-type x)
  (match x
    [`(fixnum ,signed? ,size) (c-type-fixed signed? size)]
    [`(struct ,name . ,fs) (c-type-struct name (map expand-struct-field fs))]
    [`(union ,name . ,fs) (c-type-union name (map expand-struct-field fs))]
    [`(alias ,name ,ts) (c-type-alias name ts)]
    [`(pointer ,ty) (c-type-pointer (expand-type ty))]
    [`(void)` (c-type-void)]
    [`(c-signature ,ret . ,args) (c-signature (expand-type ret) (map expand-sigvar args))]
    ))

(: shrink-type (-> c-type CQ))
(define (shrink-type x)
  (match x
    [(struct c-type-fixed (signed? size)) `(fixnum ,signed? ,size)]
    [(struct c-type-struct (name fs)) `(struct ,name ,@(map shrink-struct-field fs))]
    [(struct c-type-union (name fs)) `(union ,name ,@(map shrink-struct-field fs))]
    [(struct c-type-alias (name ts)) `(alias ,name ,ts)]
    [(struct c-type-pointer (ty)) `(pointer ,(shrink-type ty))]
    [(struct c-type-void ()) `(void)]
    [(struct c-signature (ret args)) `(sig ,(shrink-type ret) ,@(map shrink-sigvar args))]
    ))

(: expand-struct-field (-> CQ c-type-struct-field))
(define (expand-struct-field x)
  (match x
    [`(field ,name ,ty) (c-type-struct-field name (expand-type ty))]
    ))

(: shrink-struct-field (-> c-type-struct-field CQ))
(define (shrink-struct-field x)
  (match x
    [(struct c-type-struct-field (name ty)) `(field ,name ,(shrink-type ty))]
    ))

(: expand-sigvar (-> CQ c-sigvar))
(define (expand-sigvar x)
  (match x
    [`(sigvar ,name ,ty) (c-sigvar name (expand-type ty))]
    ))

(: shrink-sigvar (-> c-sigvar CQ))
(define (shrink-sigvar x)
  (match x
    [(struct c-sigvar (name ty)) `(sigvar ,name ,(shrink-type ty))]
    ))

(: expand-expression (-> CQ c-expression))
(define (expand-expression x)
  (match x
    [`(const ,v) (c-const v)]
    [`(var ,name) (c-variable name)]
    [`(ternary ,p ,x1 ,x2) (c-ternary (expand-expression p) (expand-expression x1) (expand-expression x2))]
    [`(binop ,op ,x1 ,x2) (c-binop op (expand-expression x1) (expand-expression x2))]
    [`(unop ,op ,x) (c-unop op (expand-expression x))]
    [`(call ,func . ,args) (c-function-call (expand-expression func) (map expand-expression args))]
    [`(field-access ,record ,name) (c-field-access (expand-expression record) name)]
    [`(cast ,ty ,exp) (c-cast (expand-type ty) (expand-expression exp))]
    [`(sizeof-exp ,exp) (c-sizeof (expand-expression exp))]
    [`(sizeof-ty ,ty) (c-sizeof (expand-type ty))]
    [`(array-idx ,exp ,idx) (c-array-access (expand-expression exp) (expand-expression idx))]
    [`(exp-seq . ,exps) (c-expression-sequence (map expand-expression exps))]
    ))

(: shrink-expression (-> c-expression CQ))
(define (shrink-expression x)
  (match x
    [(struct c-const (v)) `(const ,v)]
    [(struct c-variable (name)) `(var ,name)]
    [(struct c-ternary (p x1 x2)) `(ternary ,(shrink-expression p) ,(shrink-expression x1) ,(shrink-expression x2))]
    [(struct c-binop (op x1 x2)) `(binop ,op ,(shrink-expression x1) ,(shrink-expression x2))]
    [(struct c-unop (op x)) `(unop ,op ,(shrink-expression x))]
    [(struct c-function-call (func args)) `(call ,(shrink-expression func) ,@(map shrink-expression args))]
    [(struct c-field-access (record name)) `(field-access ,(shrink-expression record) ,name)]
    [(struct c-cast (ty exp)) `(cast ,(shrink-type ty) ,(shrink-expression exp))]
    [(struct c-sizeof ((? c-expression? exp))) `(sizeof-exp ,(shrink-expression exp))]
    [(struct c-sizeof ((? c-type? exp))) `(sizeof-ty ,(shrink-type exp))]
    [(struct c-array-access (arr idx)) `(array-idx ,(shrink-expression arr) ,(shrink-expression idx))]
    [(struct c-expression-sequence (exps)) `(exp-seq ,@exps)]
    ))
