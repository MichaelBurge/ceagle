#lang typed/racket

(require "types.rkt")
(require "simplifier.rkt")
(require (submod pyramid/types ast))
(require pyramid/ast)
(require pyramid/expander)
(require (submod pyramid/expander macros))
(require pyramid/utils)
(require pyramid/io)
(require pyramid/globals)

(provide compile-translation-unit)

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

Switch Statements
 1. Compile a list of all case statements bound to this switch
 2. Put them into a table with the constant as key, and a new label as value. default: or breakpoint is the fallback
 3. Generate a (cond) from the table. The condition is equality with the case X and the provided switch value. The action is a jump.
 4. When compiling default: and case: statements, produce a label deterministically-calculated from the currently active switch and its case: argument.
|#

(module typechecker typed/racket
  (require "types.rkt")
  (require (submod pyramid/types common))
  (require/typed racket/hash
    [ hash-union! (-> (HashTable Symbol c-type) (HashTable Symbol c-type) [#:combine/key (-> Symbol c-type c-type c-type)] Void)]
    [ hash-union (-> (HashTable Symbol c-type) (HashTable Symbol c-type) [#:combine/key (-> Symbol c-type c-type c-type)] (HashTable Symbol c-type))]
    )

  (require pyramid/utils)

  (provide (all-defined-out))

  (: *struct-registry* (Parameterof TypeRegistry))
  (define *struct-registry* (make-parameter (make-type-registry)))

  (: *union-registry* (Parameterof TypeRegistry))
  (define *union-registry* (make-parameter (make-type-registry)))

  (: *type-registry* (Parameterof TypeRegistry))
  (define *type-registry* (make-parameter (make-type-registry)))

  (: *variables* (Parameterof variable-table))
  (define *variables* (make-parameter (make-variable-table)))

  (: unsafe-register-type! (-> Symbol c-type c-typespace Void))
  (define (unsafe-register-type! name ty typespace)
    (hash-set! (typespace-registry typespace)
               name
               ty)
    )

  (: register-type! (-> Symbol c-type c-typespace Void))
  (define (register-type! name ty typespace)
    (maybe-register-type! ty)
    (unsafe-register-type! name ty typespace))

  (: register-variable! (-> Symbol c-type Void))
  (define (register-variable! name ty)
    (maybe-register-type! ty)
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

  (: typespace-registry (-> c-typespace TypeRegistry))
  (define (typespace-registry ty)
    (match ty
      [#f      (*type-registry*)]
      ['struct (*struct-registry*)]
      ['union  (*union-registry*)]
      ))

  ; register-type! handles typedefs. This handles "struct x { } => struct x;" associations.
  (: maybe-register-type! (-> c-type Void))
  (define (maybe-register-type! ty)
    (match ty
      [(struct c-type-fixed _) (void)]
      [(struct c-type-struct (name _)) (when name (unsafe-register-type! name ty 'struct))]
      [(struct c-type-union (name _)) (when name (unsafe-register-type! name ty 'union))]
      [(struct c-type-alias _) (void)]
      [(struct c-signature (ret args)) (maybe-register-type! ret)
                                       (for ([ arg args])
                                         (maybe-register-type! (c-sigvar-type arg))
                                         )]
      [(struct c-type-void _) (void)]
      [(struct c-type-pointer (ty)) (maybe-register-type! ty)]
      ))

  (: resolve-type (-> c-type c-type))
  (define (resolve-type ty)
    (match ty
      [(struct c-type-alias (name typespace)) (resolve-type (hash-ref (typespace-registry typespace)
                                                                      name
                                                                      (λ () (error "resolve-type: Unknown type" name typespace))))]
      [_ ty]
      ))
  (: type-field-table (-> c-type FieldTable))
  (define (type-field-table ty)
    (match (resolve-type ty)
      [(struct c-type-fixed _) (make-field-table)]
      [(struct c-type-alias _) (error "type-field-table: Unexpected case" ty)]
      [(struct c-type-struct (_ fs)) (struct-fields->field-table fs)]
      [(struct c-type-pointer _) (make-field-table)]
      [(struct c-type-union (_ fs)) (union-fields->field-table fs)]
      [x (error "type-field-table: Unknown case" x)]
      ))

  (: union-fields->field-table (-> c-type-struct-fields FieldTable))
  (define (union-fields->field-table fs)
    (define ret (make-field-table))
    (define size (fields-max-size fs))
    (for ([ f fs ])
      (match (c-type-struct-field-name f)
        [#f (error "union-fields->field-table: Nested anonymous unions not supported" f)]
        [(? symbol? name) (hash-set! ret name (c-field-info 0 (c-type-struct-field-type f)))]
        ))
    ret)

  (: struct-fields->field-table (-> c-type-struct-fields FieldTable))
  (define (struct-fields->field-table fs)
    (define ret (make-field-table))
    (define os 0)
    (for ([ f fs ])
      (let* ([ ty (c-type-struct-field-type f) ]
             [ name (c-type-struct-field-name f) ]
             [ size (type-size ty)]
             [ fi (c-field-info os ty)])
        (match* (name ty)
          [(#f (struct c-type-union (_ fs2)))
           (for ([ f2 fs2])
             (let* ([ name2 (c-type-struct-field-name f2) ]
                    [ ty2 (c-type-struct-field-type f2)]
                    [ fi2 (c-field-info os ty2)])
               (if name2
                   (hash-set! ret name2 fi2)
                   (error "struct-fields->field-table: Nested anonymous unions not supported" f2))))]
          [(#f _) (error "struct-fields->field-table: Only unions can be unnamed struct memebers" f)]
          [((? symbol? name) _)  (hash-set! ret name fi)]
          )
        (set! os (+ os size))
        ))
    ret)

  (: expression-type (-> c-expression c-type))
  (define (expression-type exp)
    (define macro-type (c-signature t-uint '()))
    (match exp
      [(struct c-const (_ signed?)) (if signed? t-int t-uint)]
      [(struct c-variable (name)) (hash-ref (*variables*) name (λ () (error "expression-type: Unknown variable" name (*variables*))))]
      [(struct c-ternary (_ cons _)) (expression-type cons)]
      [(struct c-binop (_ left _)) (expression-type left)]
      [(struct c-unop ('* exp))
       (match (expression-type exp)
         [(struct c-type-pointer (x)) x]
         [ty (error "expression-type: Attempted to dereference a non-pointer" ty exp)])]
      [(struct c-unop (_ exp)) (expression-type exp)]
      [(struct c-function-call (func _))
       (match (expression-type func)
         [(struct c-signature (ret _)) ret]
         [_ (error "expression-type: Unknown function call" func)]
         )]
      [(struct c-field-access (source name))
       (define src-ty (expression-type source))
       (define ft (type-field-table src-ty))
       (define fi (hash-ref ft name (λ () (error "expression-type: No field with name found" source name ft))))
       (c-field-info-type fi)
       ]
      [(struct c-cast (ty _)) ty]
      [(struct c-sizeof (x)) t-size]
      [(struct c-array-access (arr idx)) (match (expression-type arr)
                                           [(struct c-type-pointer (ty)) ty]
                                           [ty (error "expression-type: An array should be a pointer" ty)])]
      [(struct c-expression-sequence (exps)) (expression-type (last exps))]
      [(struct c-expression-array (exps)) (expression-type (first exps))]
      [_ (error "expression-type: Unhandled case" exp)]
      ))

  (: pointer-expression? (-> c-expression Boolean))
  (define (pointer-expression? x)
    (c-type-pointer? (expression-type x)))

  (: type-size (-> c-type Size))
  (define (type-size x)
    (match (resolve-type x)
      [(struct c-type-fixed (_ bytes)) bytes]
      [(struct c-type-struct (_ fields)) (for/sum : Size ([ field fields ])
                                           (type-size (c-type-struct-field-type field)))]
      [(struct c-signature _) 32]
      [(struct c-type-pointer _) 32]
      [(struct c-type-union (_ fields)) (fields-max-size fields)]
      [(struct c-signature _) 32 ]
      [_ (error "type-size: Unknown case" x)]))

  (: fields-max-size (-> c-type-struct-fields Size))
  (define (fields-max-size fields)
    (apply max (map (λ ([x : c-type-struct-field ])
                      (type-size (c-type-struct-field-type x)))
                    fields)))

  )
(require 'typechecker)

(: *switch-counter* (Parameterof Counter))
(define *switch-counter* (make-parameter 0))

(: *switch-base* (Parameterof Counter))
(define *switch-base* (make-parameter 0))

(: *current-function* (Parameterof Symbol))
(define *current-function* (make-parameter 'TOPLEVEL))

(: compile-translation-unit (-> c-unit Boolean Pyramid))
(define (compile-translation-unit x execute?)
  (verbose-section "Ceagle AST" VERBOSITY-LOW
                   (pretty-print x))
  (set! x (simplify x))
  (destruct c-unit x)
  (verbose-section "Ceagle Simplified AST" VERBOSITY-MEDIUM
                   (pretty-print x))
  (register-builtins!)
  (let ([ decls     (pyr-begin (map compile-declaration x-decls)) ]
        [ call-main (c-function-call (c-variable 'main) (list))])
    (quasiquote-pyramid
     `(begin (require ceagle "builtins.pmd")
             ,decls
             ,(if execute?
                  (quasiquote-pyramid `(%#-box ,(compile-expression call-main 'rvalue)))
                  (pyr-begin null))))))

(: compile-declaration (-> c-declaration Pyramid))
(define (compile-declaration x)
  (match x
    [(struct c-decl-var     _) (compile-decl-var     x)]
    [(struct c-decl-type    _) (compile-decl-type    x)]
    [(struct c-decl-func    _) (compile-decl-func    x)]
    [_                         (error "compile-decl: unknown case" x)]))

(: compile-decl-var (-> c-decl-var Pyramid))
(define (compile-decl-var x)
  (destruct c-decl-var x)
  (register-variable! x-name x-type)
  (make-macro-application #`(#,(variable-definer x-type) #,x-name #,(shrink-pyramid
                                                                     (if x-init
                                                                         (compile-expression x-init 'rvalue)
                                                                         (compile-default-initializer x-type)))))
  )

(: variable-definer (-> c-type PyramidQ))
(define (variable-definer ty)
  (match (resolve-type ty)
    [(struct c-type-fixed   _) #'%c-define-fixnum]
    [(struct c-type-struct  _) #'%c-define-struct]
    [(struct c-type-pointer _) #'%c-define-pointer]
    [(struct c-type-union   _) #'%c-define-union]
    [ty (error "variable_definer: Unhandled case" ty)]))

(: compile-default-initializer (-> c-type Pyramid))
(define (compile-default-initializer ty)
  (match (resolve-type ty)
    [(struct c-type-fixed  _) (expand-pyramid #'(unbox 0))]
    [(struct c-type-struct _) (expand-pyramid #`(%c-allocate-struct #,(type-size ty)))]
    [(struct c-type-union  _) (expand-pyramid #`(%c-allocate-struct #,(type-size ty)))]
    ))

(: compile-decl-type (-> c-decl-type Pyramid))
(define (compile-decl-type x)
  (destruct c-decl-type x)
  (register-type! x-name x-type #f)
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
      (pyr-definition x-name
                      (pyr-lambda vars
                                  (quasiquote-pyramid
                                   `(begin ,@(for/list : Pyramids ([ arg x-sig-args ])
                                               (expand-pyramid
                                                (unsafe-cast #`(%c-define-arg #,(c-sigvar-name arg) #,(sigvar-init arg)))))
                                           ,(with-returnpoint
                                              (compile-statement x-body)))))))))

(: compile-statement (-> c-statement Pyramid))
(define (compile-statement x)
  (match x
    [(? c-labeled?)              (compile-labeled         x)]
    [(? c-labeled-case?)         (compile-labeled-case    x)]
    [(? c-labeled-default?)      (compile-labeled-default x)]
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

(: compile-labeled (-> c-labeled Pyramid))
(define (compile-labeled x)
  (destruct c-labeled x)
  (expand-pyramid
   #`(begin (asm (label (quote #,x-name)))
            #,(shrink-pyramid (compile-statement x-body)))))

(: compile-labeled-statement (-> label c-statement Pyramid))
(define (compile-labeled-statement lbl stmt)
  (expand-pyramid
   #`(begin (asm (label (quote #,(label-name lbl))))
           #,(shrink-pyramid (compile-statement stmt))))
  )

(: compile-labeled-case (-> c-labeled-case Pyramid))
(define (compile-labeled-case x)
  (compile-labeled-statement (switch-case-label x)
                             (c-labeled-case-body x))
  )

(: compile-labeled-default (-> c-labeled-default Pyramid))
(define (compile-labeled-default x)
  (compile-labeled-statement (switch-default-label x)
                             (c-labeled-default-body x))
  )

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
    [(? c-array-access?)  (compile-array-access  x val-ty)]
    [(? c-expression-sequence?) (compile-expression-sequence x val-ty)]
    [(? c-expression-array?) (compile-expression-array x val-ty)]
    [_                    (error "compile-expression: Unknown case" x)]))

(: compile-const (-> c-const c-value-type Pyramid))
(define (compile-const x val-ty)
  (restrict-output-range
   (expression-type x)
   (match val-ty
     ['lvalue (error "compile-const: A constant cannot be an lvalue" x)]
     ['rvalue (expand-pyramid #`(unbox #,(c-const-value x)))]
     )))

(: compile-variable (-> c-variable c-value-type Pyramid))
(define (compile-variable x val-ty)
  (define exp (pyr-variable (c-variable-name x)))
  (define exp-ty (resolve-type (expression-type x)))
  (define size (expand-pyramid #`(unbox #,(type-size exp-ty))))
  (match* (val-ty exp-ty)
    [('rvalue (struct c-signature   _))  exp]
    [('rvalue (struct c-type-fixed  _)) (quasiquote-pyramid `(%c-word-read ,exp))]
    [('rvalue (struct c-type-struct _))
     (quasiquote-pyramid
      `(let ([ copy (%c-allocate-struct ,size)])
         (%c-struct-copy ,size ,exp copy)))]
    [('rvalue (struct c-type-pointer _)) (quasiquote-pyramid `(%c-word-read ,exp))]
    [('rvalue (struct c-type-union   _))
     (quasiquote-pyramid
      `(let ([ copy (%c-allocate-struct ,size)])
         (%c-struct-copy ,size ,exp copy)))]
    [('lvalue (struct c-type-fixed  _)) exp]
    [('lvalue (struct c-type-struct _)) exp]
    [('lvalue (struct c-type-union  _)) exp]
    [('lvalue (struct c-type-pointer (ptr-ty))) exp]
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
  (define vop (assign-op->value-op x-op))
  (define ty (resolve-type (expression-type x)))
  (define (struct? _) (c-type-struct? ty))
  (restrict-output-range
   ty
   (match x-op
     ['+ (compile-binop (c-binop 'raw+
                                 (c-binop '* x-left
                                          (c-const (type-increment-size (expression-type x-left)) #t))
                                 (c-binop '* x-right
                                          (c-const (type-increment-size (expression-type x-right)) #t)))
                        'rvalue)]
     ['- (compile-binop (c-binop 'raw-
                                 (c-binop '* x-left
                                          (c-const (type-increment-size (expression-type x-left)) #t))
                                 (c-binop '* x-right
                                          (c-const (type-increment-size (expression-type x-right)) #t)))
                        'rvalue)]
     [(and '= (? struct?)) (quasiquote-pyramid
                            `(%#-memcpy ,(compile-expression x-left 'lvalue)
                                        ,(compile-expression x-right 'rvalue)
                                        ,(expand-pyramid #`(unbox #,(type-size (expression-type x-left))))))]
     [ _
       (let* ([ signed? (expression-signed? x) ]
              [ rvalue-exp (quasiquote-pyramid
                            `(,(op->builtin x-op signed?)
                              ,(compile-expression x-left 'rvalue)
                              ,(compile-expression x-right 'rvalue)))])
         (if vop ; vop is only true if x-op was an assignment
             (quasiquote-pyramid
              `(let ([ value ,(compile-binop (c-binop vop x-left x-right) 'rvalue)])
                 (begin (%c-word-write! ,(compile-expression x-left 'lvalue)
                                        value)
                        value)))
             rvalue-exp))]
     )))

; x+1 on a T pointer increases the word in x by sizeof(T).
(: type-increment-size (-> c-type Size))
(define (type-increment-size ty)
  (match (resolve-type ty)
    [(struct c-type-pointer (ty2)) (type-size ty2)]
    [(struct c-type-fixed _) 1]
    ))

(: compile-unop (-> c-unop c-value-type Pyramid))
(define (compile-unop x val-ty)
  (destruct c-unop x)
  (define rvalue-exp (pyr-application (op->builtin x-op #f)
                                      (list (compile-expression x-exp 'rvalue))
                                      #f))
  (define (wants-old?)
    (match x-op
      ['post++ #t]
      ['post-- #t]
      ['pre++  #f]
      ['pre--  #f]
      ))
  (define ty (expression-type x))
  (restrict-output-range
   ty
   (match x-op
     ['& (compile-expression x-exp 'lvalue)]
     ['* (compile-dereference x-exp val-ty)]
     ['- (compile-expression (c-binop '- (c-const 0 #t) x-exp) val-ty)]
     ['+ (compile-expression x-exp val-ty)]
     ['pre++ (compile-expression (c-binop '+= x-exp (c-const 1 #t)) val-ty)]
     ['pre-- (compile-expression (c-binop '-= x-exp (c-const 1 #t)) val-ty)]
     ['post++
      (assert (equal? val-ty 'rvalue))
      (quasiquote-pyramid
       `(let* ([ old   ,(compile-expression x-exp 'rvalue) ])
          ,(compile-expression (c-binop '+= x-exp (c-const 1 #t)) 'rvalue)
          old))]
     ['post--
      (assert (equal? val-ty 'rvalue))
      (quasiquote-pyramid
       `(let* ([ old   ,(compile-expression x-exp 'rvalue) ])
          ,(compile-expression (c-binop '-= x-exp (c-const 1 #t)) 'rvalue)
          old))]
     [_ rvalue-exp]
     )))

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
    ['rvalue (quasiquote-pyramid
              `(,(compile-expression x-func 'rvalue)
                ,@(map (λ ([ x : c-expression ])
                         (compile-expression x 'rvalue))
                       x-args)))]
    ))

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
                       ,(pyr-const info-offset #f)
                       ,(pyr-const (type-size info-type) #f))))

  (match val-ty
    ['lvalue ptr-exp]
    ['rvalue (quasiquote-pyramid `(%c-word-read ,ptr-exp))]
    ))

(: compile-cast (-> c-cast c-value-type Pyramid))
(define (compile-cast x val-ty)
  (destruct c-cast x)
  (compile-expression x-exp val-ty)
  )

(: compile-array-access (-> c-array-access c-value-type Pyramid))
(define (compile-array-access x val-ty)
  (destruct c-array-access x)
  (assert (equal? val-ty 'rvalue)) ; TODO: lvalue
  (quasiquote-pyramid
   `(%#-mem-read ,(compile-expression x-array 'rvalue) ,(compile-expression x-index 'rvalue)))
  )

(: compile-expression-sequence (-> c-expression-sequence c-value-type Pyramid))
(define (compile-expression-sequence x val-ty)
  (destruct c-expression-sequence x)
  (quasiquote-pyramid
   `(begin ,@(map (λ ([ exp : c-expression ])
                    (compile-expression exp val-ty))
                  x-exps)))
  )

(: compile-expression-array (-> c-expression-array c-value-type Pyramid))
(define (compile-expression-array x val-ty)
  (destruct c-expression-array x)
  (assert (equal? val-ty 'rvalue))
  (quasiquote-pyramid
   `(%#-mem-alloc-init (%#-* %#-WORD ,(pyr-const (length x-exps) #f))
                       ,@(map (λ ([ exp : c-expression ])
                                (compile-expression exp 'rvalue))
                              x-exps)))
  )

; See "Switch Statements"
(: compile-switch      (-> c-switch      Pyramid))
(define (compile-switch sw)
  (with-switch-base
    (define-values (cases default) (switch-labels sw))
    (define label-after (make-label 'switch-after))
    (: jump (-> label Pyramid))
    (define (jump lbl) (expand-pyramid #`(asm (goto (label (quote #,(label-name lbl)))))))
    (: make-condition-entry (-> c-labeled-case Pyramid))
    (define (make-condition-entry c)
      (define cond-expr (c-labeled-case-expected c))
      (quasiquote-pyramid
       `((,(compile-expression cond-expr 'rvalue)) ,(jump (switch-case-label c))))
      )
    (: make-jump-table (-> c-labeled-cases (Maybe c-labeled-default) Pyramids))
    (define (make-jump-table cases default)
      (append (map make-condition-entry cases)
              (list (quasiquote-pyramid `(else ,(if default
                                                    (jump (switch-default-label default))
                                                    (expand-pyramid #'(break 0))))))))
    (with-breakpoint
      (quasiquote-pyramid
       `(begin (%c-case ,(compile-expression (c-switch-actual sw) 'rvalue)
                        ,@(make-jump-table cases default))
               ,(compile-statement (c-switch-body sw)))))
    ))

(: switch-labels (-> c-switch (Values c-labeled-cases (Maybe c-labeled-default))))
(define (switch-labels x)
  (: cases c-labeled-cases)
  (define cases null)
  (: default (Maybe c-labeled-default))
  (define default #f)
  (: register-case! (-> c-labeled-case Void))
  (define (register-case! x)    (set! cases (cons x cases)))
  (: register-default! (-> c-labeled-default Void))
  (define (register-default! x) (set! default x))
  (for ([ y (c-stmt-descendants x #:stop-on? c-switch?)])
    (match y
      [(struct c-labeled-case (expected body)) (register-case! y)]
      [(struct c-labeled-default (body))       (register-default! y)]
      [_ (void)]
      ))
  (values cases default)
  )

(: cvalue->symbol (-> CValue Symbol))
(define (cvalue->symbol cv)
  (match cv
    [(? integer?) (string->symbol (format "~v" cv))]
    ))

(: switch-case-label (-> c-labeled-case label))
(define (switch-case-label c)
  (destruct c-labeled-case c)
  (match c-expected
    [(struct c-const (cv _))
     (label (symbol-append 'switch-case-
                           (cvalue->symbol (*switch-base*))
                           (cvalue->symbol cv)))]
    [(struct c-variable (name))
     (label (symbol-append 'switch-case-
                           (cvalue->symbol (*switch-base*))
                           name))]
    ))

(: switch-default-label (-> c-labeled-default label))
(define (switch-default-label x)
  (destruct c-labeled-default x)
  (label (symbol-append 'switch-default-
                        (cvalue->symbol (*switch-base*))))
  )

; (: with-switch-base (-> (-> Pyramid) Pyramid))
(define-syntax-rule (with-switch-base body ...)
  (parameterize ([ *switch-base* (tick-counter! *switch-counter*)])
    body ...))

(: c-stmt-children (-> c-statement c-statements))
(define (c-stmt-children x)
  (match x
    [(struct c-labeled (_ st)) (list st)]
    [(struct c-labeled-case (_ st)) (list st)]
    [(struct c-labeled-default (st)) (list st)]
    [(struct c-expression-statement _) (list)]
    [(struct c-switch (_ st)) (list st)]
    [(struct c-if (_ st1 st2)) (list st1 st2)]
    [(struct c-for (_ _ _ st)) (list st)]
    [(struct c-while (_ st)) (list st)]
    [(struct c-do-while (_ st)) (list st)]
    [(struct c-goto _) (list)]
    [(struct c-block (sts)) sts]
    [(struct c-return _) (list)]
    [(struct c-break _) (list)]
    [(struct c-continue _) (list)]
    ))

(: c-stmt-descendants (-> c-statement [#:stop-on? (-> c-statement Boolean)] c-statements))
(define (c-stmt-descendants x #:stop-on? [ stop-on? (λ (x) #f)])
  (cons x (apply append (map (λ ([ st : c-statement ])
                               (if (stop-on? st)
                                   (list)
                                   (c-stmt-descendants st #:stop-on? stop-on?)))
                             (c-stmt-children x)))))

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
  (define init (map compile-declaration x-init))
  (define post (if x-post
                   (compile-expression x-post 'rvalue)
                   (expand-pyramid #'(begin))))
  (define pred (if x-pred
                   (compile-expression x-pred 'rvalue)
                   (expand-pyramid #'#t)))

  (with-breakpoint
    (quasiquote-pyramid
     `(begin ,@init
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
  (compile-for (c-for '() x-pred #f x-body)))

(: compile-do-while    (-> c-do-while    Pyramid))
(define (compile-do-while x)
  (destruct c-do-while x)
  (with-breakpoint
    (quasiquote-pyramid
     `(%c-loop-forever
       ,(with-continuepoint
          (quasiquote-pyramid
           `(begin ,(compile-statement x-body)
                   (if ,(compile-expression x-pred 'rvalue)
                       (continue 0)
                       (break #f)))))))))

(: compile-goto        (-> c-goto        Pyramid))
(define (compile-goto x)
  (compile-jump (c-goto-target x)))

(: compile-block       (-> c-block       Pyramid))
(define (compile-block x)
  (compile-c-sequence (c-block-body x)))

(: compile-return      (-> c-return      Pyramid))
(define (compile-return x)
  (destruct c-return x)
  (match x-val
    [ #f (quasiquote-pyramid `(return 0))]
    [ (? c-expression? val)  (quasiquote-pyramid `(return ,(compile-expression val 'rvalue)))]
    ))

(: compile-break       (-> c-break       Pyramid))
(define (compile-break x)
  (expand-pyramid #'(break #f)))

(: compile-continue    (-> c-continue    Pyramid))
(define (compile-continue x)
  (expand-pyramid #'(continue #f)))

(: compile-c-sequence (-> c-statements Pyramid))
(define (compile-c-sequence xs)
  (match (map compile-statement xs)
    [(? null?) (pyr-begin (list))]
    [(list y ) y]
    [ys        (pyr-begin ys)]))

(: compile-jump (-> Symbol Pyramid))
(define (compile-jump x)
  (expand-pyramid
   #`(asm (goto (label (quote #,x))))))

;; (: make-c-label (-> Symbol c-label))
;; (define (make-c-label name)
;;   (c-labeled (make-label-name name)))

(: with-escapepoint (-> Symbol Pyramid Pyramid))
(define (with-escapepoint name exp)
  (expand-pyramid #`(call/cc (λ (#,name) #,(shrink-pyramid exp)))))

(: with-returnpoint (-> Pyramid Pyramid))
(define (with-returnpoint exp)
  (with-escapepoint 'return exp))

(: with-continuepoint (-> Pyramid Pyramid))
(define (with-continuepoint exp)
  (with-escapepoint 'continue exp))

(: with-breakpoint (-> Pyramid Pyramid))
(define (with-breakpoint exp)
  (with-escapepoint 'break exp))

(: assign-op->value-op (-> Symbol (U #f Symbol)))
(define (assign-op->value-op op)
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
    [_    #f]
    ))

(: op->builtin (-> Symbol Boolean Pyramid))
(define (op->builtin op signed?)
  (define rvalue-op
    (match op
      [(? assign-op->value-op x) x]
      ['<  (if signed? 's<  'u<)]
      ['>  (if signed? 's>  'u>)]
      ['<= (if signed? 's<= 'u<=)]
      ['>= (if signed? 's>= 'u>=)]
      [_ op]))
  (pyr-variable (symbol-append '%c-word-op rvalue-op))
  )

(: assign-ops (Setof Symbol))
;(define assign-ops (set))
(define assign-ops (apply set '(<<= >>= += -= *= /= %= \|\|= \|= ^= &&= &= =)))

(: wants-lvalue? (-> Symbol Boolean))
(define (wants-lvalue? op)
  (set-member? assign-ops op))

(: register-builtins! (-> Void))
(define (register-builtins!)
  (register-variable! '__builtin_set_test_result          (c-signature (c-type-void) (list (c-sigvar 'expected t-int))))
  (register-variable! '__builtin_ctzll                    (c-signature t-int (list (c-sigvar 'x t-int))))
  (register-variable! '__builtin_clzll                    (c-signature t-int (list (c-sigvar 'x t-int))))
  (register-variable! '__builtin_bswap64                  (c-signature t-int (list (c-sigvar 'x t-int))))
  (register-variable! '__builtin_trap                     (c-signature (c-type-void) (list (c-sigvar 'x t-int))))

  (register-variable! '__builtin_print_word               (c-signature t-int (list (c-sigvar 'x t-int))))
  (register-variable! '__builtin_set_max_iterations       (c-signature t-int (list (c-sigvar 'x t-int))))
  (register-variable! '__builtin_set_max_simulator_memory (c-signature t-int (list (c-sigvar 'x t-int))))
  )

(: type-signed? (-> c-type Boolean))
(define (type-signed? x)
  (match (resolve-type x)
    [(struct c-type-fixed (signed? _)) signed?]
    [(struct c-type-pointer _) #f]
    [y (error "type-signed?: Unable to determine signedness of type" y)]
    ))

(: expression-signed? (-> c-expression Boolean))
(define (expression-signed? x)
  (type-signed? (expression-type x))
  )

; A 64-bit integer needs to be restricted to the 64 bits after a 256-bit addition or similar is performed.
(: restrict-output-range (-> c-type Pyramid Pyramid))
(define (restrict-output-range ty x)
  (match (resolve-type ty)
    [(struct c-type-fixed (_ 32)) x]
    [(struct c-type-fixed (signed? sz)) (make-macro-application
                                         #`(%c-restrict-bytes #,(shrink-pyramid x) (unbox #,sz) #,signed?))]
    [_ x]
    ))
