#lang typed/racket/no-check

(require "types.rkt")
(require (submod pyramid/types ast))
(require pyramid/ast)
(require pyramid/parser)
(require (submod pyramid/parser macros))
(require pyramid/utils)

(provide compile-c)

(module typechecker typed/racket
  (require "types.rkt")
  (require/typed racket/hash
    [ hash-union! (-> (HashTable Symbol c-type) (HashTable Symbol c-type) [#:combine/key (-> Symbol c-type c-type c-type)] Void)]
    [ hash-union (-> (HashTable Symbol c-type) (HashTable Symbol c-type) [#:combine/key (-> Symbol c-type c-type c-type)] (HashTable Symbol c-type))]
    )

  (require pyramid/utils)

  (provide (all-defined-out))

  (: *type-registry* (Parameterof TypeRegistry))
  (define *type-registry* (make-parameter (make-type-registry)))

  (: *variables* (Parameterof variable-table))
  (define *variables* (make-parameter (make-variable-table)))

  (: register-type! (-> Symbol c-type Void))
  (define (register-type! name ty)
    (hash-set! (*type-registry*) name ty))

  (: register-variable! (-> Symbol c-type Void))
  (define (register-variable! name ty)
    (hash-set! (*variables*) name ty))

  (: with-function-scope (All (A) (-> c-signature (-> A) A)))
  (define (with-function-scope sig f)
    (parameterize ([ *variables* (hash-copy (*variables*))])
      (hash-union! (*variables*)
                   (signature->variable-table sig)
                   #:combine/key (λ ([ k : Symbol ] [v0 : c-type] [v : c-type]) v))
      (f)))

  (: signature->variable-table (-> c-signature variable-table))
  (define (signature->variable-table sig)
    (destruct c-signature sig)
    (define ret (make-variable-table))
    (for ([ sv sig-args ])
      (hash-set! ret (c-sigvar-name sv) (c-sigvar-type sv)))
    ret)

  (: resolve-type (-> c-type c-type))
  (define (resolve-type ty)
    (match ty
      [(struct c-type-alias (name)) (resolve-type (hash-ref (*type-registry*)
                                                            name
                                                            (λ () (error "resolve-type: Unknown type" name))))]
      [_ ty]))

  (: expression-type (-> c-expression c-type))
  (define (expression-type exp)
    (define t-uint (c-type-fixed #f 256))
    (define t-int  (c-type-fixed #t 256))
    (define macro-type (c-signature t-uint '()))
    (match exp
      [(struct c-const ((? exact-positive-integer?))) t-uint]
      [(struct c-const ((? exact-integer?)))          t-int]
      [(struct c-variable (name))
       (hash-ref (*variables*) name (λ () macro-type))]
      [(struct c-ternary (_ cons _)) (expression-type cons)]
      [(struct c-binop (_ left _)) (expression-type left)]
      [(struct c-unop (_ exp)) (expression-type exp)]
      [(struct c-function-call _) (error "expression-type: Unimplemented function call" exp)]
      [(struct c-field-access _) (error "expression-type: Unimplemented field access" exp)]
      ))

  (: bits->bytes (-> Integer Integer))
  (define (bits->bytes x) (quotient x 8))

  (: type-size (-> c-type Integer))
  (define (type-size x)
    (match x
      [(struct c-type-fixed (_ bits)) (bits->bytes bits)]
      [(struct c-type-struct (fields)) (for/sum ([ field fields ])
                                         (type-size (c-type-struct-field-type field)))]
      [(struct c-type-alias _) (type-size (resolve-type x))]
      [_ (error "type-size: Unknown case" x)]))
  )
(require 'typechecker)

(: compile-c (-> c-unit Pyramid))
(define (compile-c x)
  (destruct c-unit x)
  (define decls
    (pyr-begin (map compile-declaration x-decls)))
  (define call-main
    (c-function-call (c-variable 'main) (list)))
  (quasiquote-pyramid
   `(begin (include ceagle "builtins.pmd")
           ,decls
           (%-box ,(compile-expression call-main 'rvalue)))))

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
  (register-variable! x-name x-type)
  (quasiquote-pyramid
   `(begin (define ,(pyr-const x-name) (%c-allocate-struct ,(pyr-const (type-size x-type))))
           (%c-noinline ,(pyr-const x-name)) ; TODO: The simplifier tries to inline constants like (define x 5) that are later modified.
           (%c-word-write! ,(pyr-const x-name)
                           ,(if x-init
                                (compile-expression x-init 'rvalue)
                                (compile-default-initializer x-type)))
           )))


(: compile-default-initializer (-> c-type Pyramid))
(define (compile-default-initializer ty)
  (match ty
    [(struct c-type-fixed _) (expand-pyramid `(%-unbox 0))]
    [(struct c-type-alias _) (compile-default-initializer (resolve-type ty))]
    [(struct c-type-struct _) (expand-pyramid `(%-unbox 0))] ; TODO: This doesn't initialize the whole struct.
    ))

(: compile-decl-type (-> c-decl-type Pyramid))
(define (compile-decl-type x)
  (destruct c-decl-type x)
  (register-type! x-name x-type)
  (pyr-begin (list)))

(: compile-decl-func (-> c-decl-func Pyramid))
(define (compile-decl-func x)
  (destruct c-decl-func x)
  (destruct c-signature x-sig)
  (define (sigvar-init v) (symbol-append (c-sigvar-name v)
                                         '-init))
  (: vars VariableNames)
  (define vars (map sigvar-init x-sig-args))
  (register-variable! x-name x-sig)
  (with-function-scope x-sig
    (λ ()
      (expand-pyramid
       `(define (,x-name ,@vars)
          ,@(for/list ([ arg x-sig-args ])
              `(%c-define-arg ,(c-sigvar-name arg) ,(sigvar-init arg)))
          ,(shrink-pyramid
            (with-returnpoint
              (compile-statement x-body))))))))

(: compile-statement (-> c-statement Pyramid))
(define (compile-statement x)
  (match x
    [(? c-label?)                (compile-label       x)]
    [(? c-expression-statement?) (compile-expression  (c-expression-statement-exp x) 'rvalue)]
    [(? c-switch?)               (compile-switch      x)]
    [(? c-if?)                   (compile-if          x)]
    [(? c-for?)                  (compile-for         x)]
    [(? c-while?)                (compile-while       x)]
    [(? c-do-while?)             (compile-do-while    x)]
    [(? c-goto?)                 (compile-goto        x)]
    [(? c-block?)                (compile-block       x)]
    [(? c-return?)               (compile-return      x)]
    [(? c-break?)                (compile-break       x)]
    [(? c-continue?)             (compile-continue    x)]
    [(? c-declaration?)          (compile-declaration x)]
    [_                           (error "compile-statement: Unknown case" x)]))

(: compile-label (-> c-label Pyramid))
(define (compile-label x)
  (destruct c-label x)
  (expand-pyramid
   `(asm (label (quote ,x-name)))))

(: compile-expression (-> c-expression c-value-type Pyramid))
(define (compile-expression x val-ty)
  (match x
    [(? c-const?)         (compile-const         x val-ty)]
    [(? c-variable?)      (compile-variable      x val-ty)]
    [(? c-ternary?)       (compile-ternary       x val-ty)]
    [(? c-binop?)         (compile-binop         x val-ty)]
    [(? c-unop?)          (compile-unop          x val-ty)]
    [(? c-function-call?) (compile-function-call x val-ty)]
    [(? c-field-access?)  (compile-field-access  x val-ty)]
    [_                    (error "compile-expression: Unknown case" x)]))

(: compile-const (-> c-const c-value-type Pyramid))
(define (compile-const x val-ty)
  (match val-ty
    ['lvalue (error "compile-const: A constant cannot be an lvalue" x)]
    ['rvalue (quasiquote-pyramid `(%-unbox ,(pyr-const (c-const-value x))))]
    ))

(: compile-variable (-> c-variable c-value-type Pyramid))
(define (compile-variable x val-ty)
  (define exp (pyr-variable (c-variable-name x)))
  (define exp-ty (resolve-type (expression-type x)))
  (match* (val-ty exp-ty)
    [('rvalue (struct c-signature _))  exp]
    [('rvalue (struct c-type-fixed _)) (quasiquote-pyramid `(%c-word-read ,exp))]
    [('rvalue _)                       (error "compile-variable: Unhandled case" exp-ty)]
    [('lvalue (struct c-type-fixed _)) exp]
    [('lvalue (struct c-type-struct _)) exp]
    [('lvalue _) (error "compile-variable: Unhandled case" val-ty exp-ty)]
    ))

(: compile-ternary (-> c-ternary c-value-type Pyramid))
(define (compile-ternary x val-ty)
  (destruct c-ternary x)
  (pyr-if (compile-expression x-pred        'rvalue)
          (compile-expression x-consequent  val-ty)
          (compile-expression x-alternative val-ty)))

(: compile-binop (-> c-binop c-value-type Pyramid))
(define (compile-binop x val-ty)
  (destruct c-binop x)
  (define rvalue-exp (pyr-application (op->builtin x-op)
                                      (list (compile-expression x-left 'rvalue)
                                            (compile-expression x-right 'rvalue)
                                            )))
  (if (wants-lvalue? x-op)
      (quasiquote-pyramid
       `(let ([ value ,rvalue-exp ])
          (begin (%c-word-write! ,(compile-expression x-left 'lvalue)
                                 value)
                 value)))
      rvalue-exp))

(: compile-unop (-> c-unop c-value-type Pyramid))
(define (compile-unop x val-ty)
  (destruct c-unop x)
  (define rvalue-exp (pyr-application (op->builtin x-op)
                                      (list (compile-expression x-exp 'rvalue))))
  (if (wants-lvalue? x-op)
      (quasiquote-pyramid
       `(let ([ value ,rvalue-exp ])
          (%c-word-write! ,(compile-expression x-exp 'lvalue) value)
          value))
      rvalue-exp))

(: compile-function-call (-> c-function-call c-value-type Pyramid))
(define (compile-function-call x val-ty)
  (destruct c-function-call x)
  (match val-ty
    ['lvalue (error "compile-function-call: Function calls cannot be lvalues" x)]
    ['rvalue (pyr-application (compile-expression x-func 'rvalue)
                              (map (λ (x) (compile-expression x 'rvalue))
                                   x-args))]))

(: compile-field-access (-> c-field-access c-value-type Pyramid))
(define (compile-field-access x val-ty)
  (destruct c-field-access x)
  (define ty (expression-type x-source))
  (define field-table (type-field-table ty))
  (define info (hash-ref field-table x-name))
  (destruct c-field-info info)
  (define ptr-exp
    (quasiquote-pyramid
     `(%c-struct-field ,(compile-expression x-source 'lvalue)
                       (%-unbox ,(pyr-const info-offset))
                       (%-unbox ,(pyr-const info-size)))))
  (match val-ty
    ['lvalue ptr-exp]
    ['rvalue (quasiquote-pyramid `(%c-word-read ,ptr-exp))]
    ))

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
          (pyr-if (equal? (compile-expression hd-expected 'rvalue)
                          (compile-expression sw-actual 'rvalue))
                  (compile-statement hd-body)
                  (compile-cases tl)))))
  (with-breakpoint (compile-cases sw-cs)))

(: compile-if          (-> c-if          Pyramid))
(define (compile-if x)
  (destruct c-if x)
  (pyr-if (compile-expression x-pred 'rvalue)
          (compile-statement  x-consequent)
          (compile-statement  x-alternative)
          ))

(: compile-for         (-> c-for         Pyramid))
(define (compile-for x)
  (destruct c-for x)
  (define init (if x-init
                   (compile-declaration x-init)
                   (pyr-begin (list))))
  (define post (if x-post
                   (compile-expression x-post 'rvalue)
                   (pyr-begin (list))))
  (define pred (if x-pred
                   (compile-expression x-pred 'rvalue)
                   (pyr-const 1)))

  (with-breakpoint
    (quasiquote-pyramid
     `(begin ,init
             (%c-loop-forever
              ,(with-continuepoint
                 (quasiquote-pyramid
                  `(if ,pred
                       (begin ,(compile-statement x-body)
                              ,post
                              (continue 0))
                       (break 0)))))))))

(: compile-while       (-> c-while       Pyramid))
(define (compile-while x)
  (destruct c-while x)
  (compile-for (c-for #f x-pred #f x-body)))

(: compile-do-while    (-> c-do-while    Pyramid))
(define (compile-do-while x)
  (destruct c-do-while x)
  (with-breakpoint
    (with-continuepoint
      (quasiquote-pyramid
       `(begin ,x-body
               (if ,(compile-expression x-pred 'rvalue)
                   (continue 0)
                   (break #f)))))))

(: compile-goto        (-> c-goto        Pyramid))
(define (compile-goto x)
  (compile-jump (c-label (c-goto-target x))))

(: compile-block       (-> c-block       Pyramid))
(define (compile-block x)
  (compile-c-sequence (c-block-body x)))

(: compile-return      (-> c-return      Pyramid))
(define (compile-return x)
  (destruct c-return x)
  (quasiquote-pyramid
   `(return ,(compile-expression x-val 'rvalue))))

(: compile-break       (-> c-break       Pyramid))
(define (compile-break x)
  (expand-pyramid `(break #f)))

(: compile-continue    (-> c-continue    Pyramid))
(define (compile-continue x)
  (expand-pyramid `(continue #f)))

(: compile-c-sequence (-> c-statements Pyramid))
(define (compile-c-sequence xs)
  (match (map compile-statement xs)
    [(? null?) (pyr-begin (list))]
    [(list y ) y]
    [ys        (pyr-begin ys)]))

(: compile-jump (-> c-label Pyramid))
(define (compile-jump x)
  (expand-pyramid
   `(asm (goto (label (quote ,(c-label-name x)))))))

(: make-c-label (-> Symbol c-label))
(define (make-c-label name)
  (c-label (make-label-name name)))

(: with-escapepoint (-> Symbol Pyramid Pyramid))
(define (with-escapepoint name exp)
  (expand-pyramid `(call/cc (λ (,name) ,(shrink-pyramid exp)))))

(: with-returnpoint (-> Pyramid Pyramid))
(define (with-returnpoint exp)
  (with-escapepoint 'return exp))

(: with-continuepoint (-> Pyramid Pyramid))
(define (with-continuepoint exp)
  (with-escapepoint 'continue exp))

(: with-breakpoint (-> Pyramid Pyramid))
(define (with-breakpoint exp)
  (with-escapepoint 'break exp))

(: type-field-table (-> c-type FieldTable))
(define (type-field-table ty)
  (match (resolve-type ty)
    [(struct c-type-fixed _) (make-field-table)]
    [(struct c-type-alias _) (error "type-field-table: Unexpected case" ty)]
    [(struct c-type-struct (fs)) (struct-fields->field-table fs)]
    [x (error "type-field-table: Unknown case" x)]
    ))

(: struct-fields->field-table (-> c-type-struct-fields FieldTable))
(define (struct-fields->field-table fs)
  (define ret (make-field-table))
  (define os 0)
  (for ([ f fs ])
    (let ([ size (type-size (c-type-struct-field-type f))])
      (hash-set! ret (c-type-struct-field-name f) (c-field-info os size))
      (set! os (+ os size))
      ))
  ret)

(: symbol-append (-> Symbol Symbol Symbol))
(define (symbol-append a b)
  (string->symbol (string-append (symbol->string a)
                                 (symbol->string b))))

(: op->builtin (-> Symbol Pyramid))
(define (op->builtin op)
  (define rvalue-op
    (match op
      ['<<= '<<]
      ['>>= '>>]
      ['+=  '+]
      ['-=  '-]
      ['*=  '*]
      ['/=  '/]
      ['%=  '%]
      ['\|\|= '\|\|]
      ['\|= '\|]
      ['^=  '^]
      ['&&= '&&]
      ['&=  '&]
      ['=   'right]
      ['++  '|+1|]
      ['--  '|-1|]
      [_    #f]))
  (pyr-variable (symbol-append '%c-word-op (if rvalue-op rvalue-op op)))
  )


(: int->lvalue (-> Pyramid Pyramid))
(define (int->lvalue x)
  `(%-fixnum-lvalue ,x))

(: int->rvalue (-> Pyramid Pyramid))
(define (int->rvalue x)
  `(%-unbox ,x))

(: assign-ops (Setof Symbol))
;(define assign-ops (set))
(define assign-ops (apply set '(<<= >>= += -= *= /= %= \|\|= \|= ^= &&= &= = ++ --)))

(: wants-lvalue? (-> Symbol Boolean))
(define (wants-lvalue? op)
  (set-member? assign-ops op))
