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
    [ hash-union (-> (HashTable Symbol c-type) (HashTable Symbol c-type) [#:combine/key (-> Symbol c-type c-type c-type)] (HashTable Symbol c-type))])
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
    (parameterize ([ *variables* (hash-union (*variables*)
                                             (signature->variable-table sig)
                                             #:combine/key (λ ([ k : Symbol ] [v0 : c-type] [v : c-type]) v))])
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
    (match exp
      [(struct c-const ((? exact-positive-integer?))) t-uint]
      [(struct c-const ((? exact-integer?)))          t-int]
      [(struct c-variable (name))                     (hash-ref (*variables*) name (λ () (error "expression-type: Unknown variable" name)))]
      [(struct c-ternary (_ cons _)) (expression-type cons)]
      [(struct c-binop (_ left _)) (expression-type left)]
      [(struct c-unop (_ exp)) (expression-type exp)]
      [(struct c-function-call _) (error "expression-type: Unimplemented function call" exp)]
      [(struct c-field-access _) (error "expression-type: Unimplemented field access" exp)]
      ))
  )
(require 'typechecker)

(: compile-c (-> c-unit Pyramid))
(define (compile-c x)
  (destruct c-unit x)
  (define decls
    (pyr-begin (map compile-declaration x-decls)))
  (define call-main
    (c-function-call (c-variable 'main) (list)))
  (pyr-begin
   (listof
    (expand-pyramid `(include ceagle "builtins.pmd"))
    decls
    (compile-expression call-main))))

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
  (pyr-definition x-name
                  (if x-init
                      (compile-expression x-init)
                      (compile-default-initializer x-type))))

(: compile-default-initializer (-> c-type Pyramid))
(define (compile-default-initializer ty)
  (match (resolve-type ty)
    [(struct c-type-fixed _) (pyr-const 0)]
    [(struct c-type-alias _) (error "compile-default-initializer: Unexpected case")]
    [(struct c-type-struct _) (error "compile-default-initializer: TODO - implement struct initializers")]))

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
  (expand-pyramid
   `(define (,x-name ,@vars)
      ,(shrink-pyramid
        (with-returnpoint
          (compile-statement x-body))))))

(: compile-statement (-> c-statement Pyramid))
(define (compile-statement x)
  (match x
    [(? c-label?)                (compile-label       x)]
    [(? c-expression-statement?) (compile-expression  (c-expression-statement-exp x))]
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
    [_                (error "compile-statement: Unknown case" x)]))

(: compile-label (-> c-label Pyramid))
(define (compile-label x)
  (destruct c-label x)
  (expand-pyramid
   `(asm (label (quote ,x-name)))))

(: compile-expression (-> c-expression Pyramid))
(define (compile-expression x)
  (match x
    [(? c-const?)         (compile-const x)]
    [(? c-variable?)      (compile-variable x)]
    [(? c-ternary?)       (compile-ternary x)]
    [(? c-binop?)         (compile-binop x)]
    [(? c-unop?)          (compile-unop x)]
    [(? c-function-call?) (compile-function-call x)]
    [(? c-field-access?)  (compile-field-access x)]
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
  (pyr-application (op->builtin x-op)
                   (list (compile-expression x-left)
                         (compile-expression x-right))))

(: compile-unop          (-> c-unop          Pyramid))
(define (compile-unop x)
  (destruct c-unop x)
  (pyr-application (op->builtin x-op)
                   (list (compile-expression x-exp))))

(: compile-function-call (-> c-function-call Pyramid))
(define (compile-function-call x)
  (destruct c-function-call x)
  (pyr-application (compile-expression x-func)
                   (map compile-expression x-args)))

(: compile-field-access (-> c-field-access Pyramid))
(define (compile-field-access x)
  (destruct c-field-access x)
  (define ty (expression-type x-source))
  (define field-table (type-field-table ty))
  (define info (hash-ref field-table x-name))
  (destruct c-field-info info)
  (quasiquote-pyramid
   `(%c-read-field ,(compile-expression x-source)
                   ,(pyr-const info-offset)
                   ,(pyr-const info-size))))

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
  (with-breakpoint (compile-cases sw-cs)))

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
                   (pyr-begin (list))))
  (define post (if x-post
                   (compile-expression x-post)
                   (pyr-begin (list))))
  (define pred (if x-pred
                   (compile-expression x-pred)
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
               (if ,(compile-expression x-pred)
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
   `(return ,(compile-expression x-val))))

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

(: type-size (-> c-type Integer))
(define (type-size ty)
  (match ty
    [(struct c-type-fixed _) 32]
    [_ (error "type-size: Unknown type" ty)]))

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
  (pyr-variable (symbol-append '%c- op)))
