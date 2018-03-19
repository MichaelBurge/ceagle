#lang typed/racket

(require "types.rkt")
(require (submod pyramid/types ast))
(require pyramid/ast)
(require pyramid/parser)
(require (submod pyramid/parser macros))
(require pyramid/utils)

(provide compile-c)

#|
Compilation strategy
* Flow control such as break, continue, or return are implemented by creating and calling continuations.
* Expressions always resolve to a result, and can also have an associated location.
** An rvalue is an expression that is compiled to emit a result
** An lvalue is an expression that is compiled to emit a location
** Some expressions such as constants or non-modifying binary operators are not lvalues
** rvalues and lvalues always fit inside a single machine word, simplifying parameter and return passing.
** Rules for converting expression values to rvalues and lvalues:
*** The rvalue of a machine word is that word. The lvalue is the address where such a word is stored.
*** The rvalue and the lvalue of a struct are both pointers. Ceagle does not attempt to generate local bytestrings.
**** However, the rvalue of a struct allocates and copies into a new struct, so the pointers are not equal.
*** The rvalue of a function is the code pointer for it. An lvalue is the address where such a code pointer is stored.
* Variables are always initialized with an rvalue
* Parameters are always passed as rvalues
* Return values are passed as rvalues
|#

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
      [(struct c-variable (name)) (hash-ref (*variables*) name (λ () (error "expression-type: Unknown variable" name (*variables*))))]
      [(struct c-ternary (_ cons _)) (expression-type cons)]
      [(struct c-binop (_ left _)) (expression-type left)]
      [(struct c-unop ('* exp))
       (match (expression-type exp)
         [(struct c-type-pointer (x)) x]
         [ty (error "expression-type: Attempted to dereference a non-pointer" ty exp)])]
      [(struct c-unop (_ exp)) (expression-type exp)]
      [(struct c-function-call _) (error "expression-type: Unimplemented function call" exp)]
      [(struct c-field-access (source name))
       (match (resolve-type (expression-type source))
         [(struct c-type-struct (fields))
          (match (assoc name (map (λ ([ f : c-type-struct-field ]) (cons (c-type-struct-field-name f)
                                               (c-type-struct-field-type f)))
                                  fields))
            [#f (error "expression-type: No field with name found" source name fields)]
            [(cons _ (? c-type? ty)) ty])]
         [ ty (error "expression-type: Attempted to access a field from a non-struct" ty source name)])]
      [_ (error "expression-type: Unhandled case" exp)]
      ))

  (: pointer-expression? (-> c-expression Boolean))
  (define (pointer-expression? x)
    (c-type-pointer? (expression-type x)))

  (: bits->bytes (-> Integer Integer))
  (define (bits->bytes x) (quotient x 8))

  (: type-size (-> c-type Integer))
  (define (type-size x)
    (match x
      [(struct c-type-fixed (_ bits)) (bits->bytes bits)]
      [(struct c-type-struct (fields)) (for/sum ([ field fields ])
                                         (type-size (c-type-struct-field-type field)))]
      [(struct c-type-alias _) (type-size (resolve-type x))]
      [(struct c-signature _) 32]
      [(struct c-type-pointer _) 32]
      [(struct c-type-union (fields)) (fields-max-size fields)]
      [_ (error "type-size: Unknown case" x)]))

  (: fields-max-size (-> c-type-struct-fields Integer))
  (define (fields-max-size fields)
    (apply max (map (λ ([x : c-type-struct-field ])
                      (type-size (c-type-struct-field-type x)))
                    fields)))
  )
(require 'typechecker)

(: compile-c (-> c-unit Pyramid))
(define (compile-c x)
  (destruct c-unit x)
  (register-builtins!)
  (let ([ decls     (pyr-begin (map compile-declaration x-decls)) ]
        [ call-main (c-function-call (c-variable 'main) (list))])
    (quasiquote-pyramid
     `(begin (include ceagle "builtins.pmd")
             ,decls
             (%-box ,(compile-expression call-main 'rvalue))))))

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
   `(,(pyr-const (variable-definer x-type))
     ,(pyr-const x-name)
     ,(if x-init
          (compile-expression x-init 'rvalue)
          (compile-default-initializer x-type))
     )))

(: variable-definer (-> c-type Symbol))
(define (variable-definer ty)
  (match ty
    [(struct c-type-fixed _) '%c-define-fixnum]
    [(struct c-type-struct _) '%c-define-struct]
    [(struct c-type-alias _) (variable-definer (resolve-type ty))]
    [(struct c-type-pointer _) '%c-define-pointer]
    [(struct c-type-union _) '%c-define-union]
    [_ (error "variable_definer: Unhandled case" ty)]))

(: compile-default-initializer (-> c-type Pyramid))
(define (compile-default-initializer ty)
  (match ty
    [(struct c-type-fixed _) (expand-pyramid `(%-unbox 0))]
    [(struct c-type-alias _) (compile-default-initializer (resolve-type ty))]
    [(struct c-type-struct _) (quasiquote-pyramid `(%c-allocate-struct ,(pyr-const (type-size ty))))]
    [(struct c-type-union _) (quasiquote-pyramid `(%c-allocate-struct ,(pyr-const (type-size ty))))]
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
  (: sigvar-init (-> c-sigvar VariableName))
  (define (sigvar-init v) (symbol-append (c-sigvar-name v)
                                         '-init))
  (: vars VariableNames)
  (define vars (map sigvar-init x-sig-args))
  (register-variable! x-name x-sig)
  (with-function-scope x-sig
    (λ ()
      (expand-pyramid
       `(define (,x-name ,@vars)
          ,@(for/list : PyramidQs ([ arg x-sig-args ])
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
    [(? c-cast?)          (compile-cast          x val-ty)]
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
  (define size (pyr-const (type-size exp-ty)))
  (match* (val-ty exp-ty)
    [('rvalue (struct c-signature _))  exp]
    [('rvalue (struct c-type-fixed _)) (quasiquote-pyramid `(%c-word-read ,exp))]
    [('rvalue (struct c-type-struct _))
     (quasiquote-pyramid
      `(let ([ copy (%c-allocate-struct ,size)])
         (%c-struct-copy (%-unbox ,size) ,exp copy)))]
    [('rvalue (struct c-type-pointer _)) (quasiquote-pyramid `(%c-word-read ,exp))]
    [('rvalue (struct c-type-union _))
     (quasiquote-pyramid
      `(let ([ copy (%c-allocate-struct ,size)])
         (%c-struct-copy (%-unbox ,size) ,exp copy)))]
    [('lvalue (struct c-type-fixed _)) exp]
    [('lvalue (struct c-type-struct _)) exp]
    [('lvalue (struct c-type-union _)) exp]
    [(_ _)    (error "compile-variable: Unhandled case" val-ty exp-ty)]
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
  (match x-op
    ['& (compile-expression x-exp 'lvalue)]
    ['* (compile-dereference x-exp val-ty)]
    [(? wants-lvalue?)
     (quasiquote-pyramid
      `(let ([ value ,rvalue-exp ])
         (%c-word-write! ,(compile-expression x-exp 'lvalue) value)
         value))]
    [_ rvalue-exp]
    ))

(: compile-dereference (-> c-expression c-value-type Pyramid))
(define (compile-dereference exp ty)
  (define rval (compile-expression exp 'rvalue))
  (assert exp pointer-expression?)
  (match ty
    ['lvalue rval]
    ['rvalue (quasiquote-pyramid `(%c-word-opdereference ,rval))]
    ))

(: compile-function-call (-> c-function-call c-value-type Pyramid))
(define (compile-function-call x val-ty)
  (destruct c-function-call x)
  (match val-ty
    ['lvalue (error "compile-function-call: Function calls cannot be lvalues" x)]
    ['rvalue (pyr-application (compile-expression x-func 'rvalue)
                              (map (λ ([ x : c-expression ]) (compile-expression x 'rvalue))
                                   x-args))]))

(: compile-field-access (-> c-field-access c-value-type Pyramid))
(define (compile-field-access x val-ty)
  (destruct c-field-access x)
  (define ty (expression-type x-source))
  (define field-table (type-field-table ty))
  (define info (hash-ref field-table x-name (λ () (error "compile-field-access: Field not found" (resolve-type ty) x field-table))))
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

(: compile-cast (-> c-cast c-value-type Pyramid))
(define (compile-cast x val-ty)
  (destruct c-cast x)
  (compile-expression x-exp val-ty)
  )

(: compile-switch      (-> c-switch      Pyramid))
(define (compile-switch sw)
  (destruct c-switch sw)
  (: equal? (-> Pyramid Pyramid Pyramid))
  (define (equal? x y) (pyr-application (pyr-variable '%#-=) (list x y)))
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
       `(begin ,(compile-statement x-body)
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
   (match x-val
     [ #f  `(return 0)]
     [ (? c-expression? val) `(return ,(compile-expression val 'rvalue))]
     )))

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
    [(struct c-type-pointer _) (make-field-table)]
    [(struct c-type-union (fs)) (union-fields->field-table fs)]
    [x (error "type-field-table: Unknown case" x)]
    ))

(: union-fields->field-table (-> c-type-struct-fields FieldTable))
(define (union-fields->field-table fs)
  (define ret (make-field-table))
  (define size (fields-max-size fs))
  (for ([ f fs ])
    (match (c-type-struct-field-name f)
      [#f (error "union-fields->field-table: Nested anonymous unions not supported" f)]
      [(? symbol? name) (hash-set! ret name (c-field-info 0 size))]
      ))
  ret)

(: struct-fields->field-table (-> c-type-struct-fields FieldTable))
(define (struct-fields->field-table fs)
  (define ret (make-field-table))
  (define os 0)
  (for ([ f fs ])
    (let* ([ ty (c-type-struct-field-type f) ]
           [ name (c-type-struct-field-name f) ]
           [ size (type-size ty) ]
           [ fi (c-field-info os size)])
      (match* (name ty)
        [(#f (struct c-type-union (fs2)))
         (for ([ f2 fs2])
           (let ([ name2 (c-type-struct-field-name f2) ])
             (if name2
                 (hash-set! ret name2 fi)
                 (error "struct-fields->field-table: Nested anonymous unions not supported" f2))))]
        [(#f _) (error "struct-fields->field-table: Only unions can be unnamed struct memebers" f)]
        [((? symbol? name) _)  (hash-set! ret name fi)]
        )
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

(: assign-ops (Setof Symbol))
;(define assign-ops (set))
(define assign-ops (apply set '(<<= >>= += -= *= /= %= \|\|= \|= ^= &&= &= = ++ --)))

(: wants-lvalue? (-> Symbol Boolean))
(define (wants-lvalue? op)
  (set-member? assign-ops op))

(: register-builtins! (-> Void))
(define (register-builtins!)
  (define t-int (c-type-fixed #t 256))
  (register-variable! '__builtin_set_test_result (c-signature (c-type-void) (list (c-sigvar 'expected t-int))))
  )
