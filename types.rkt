#lang typed/racket

(require typed/racket/unsafe)
(require (submod pyramid/types common))

(provide (all-defined-out))

(define-type CValue (U Integer String Char))

(define-type (Maybe A) (U A #f))

(define-type c-typespace (U #f 'struct 'union))

(struct c-declaration () #:transparent)
(struct c-expression () #:transparent)

(struct c-type-fixed ([ signed? : Boolean ] [ bytes : Size ]) #:mutable #:transparent)
(struct c-type-struct-field ([ name : (U #f Symbol) ] [ type : c-type ]) #:mutable #:transparent)
(struct c-type-struct ([ name : (Maybe Symbol) ] [ fs : c-type-struct-fields ]) #:transparent)
(struct c-type-union ([ name : (Maybe Symbol) ] [ fs : c-type-struct-fields ]) #:transparent)
(struct c-type-alias ([ name : Symbol ] [ typespace : c-typespace]) #:transparent)
(struct c-type-pointer ([ type : c-type ]) #:transparent)
(struct c-type-void () #:transparent)
(struct c-signature ([ ret  : c-type ] [ args : c-sigvars ]) #:transparent)

(: t-int c-type)
(define t-int (c-type-fixed #t 32))
(: t-uint c-type)
(define t-uint (c-type-fixed #f 32))

(define-type c-type-struct-fields (Listof c-type-struct-field))
(define-type c-type (U c-type-fixed c-type-struct c-type-alias c-signature c-type-pointer c-type-void c-type-union))
(define-predicate c-type? c-type)

(struct c-unit  ([ decls : c-declarations ]) #:transparent)
(struct c-decl-var  c-declaration ([ name : Symbol ] [ type : c-type ] [ init : (Maybe c-expression) ]) #:mutable #:transparent)
(struct c-decl-type c-declaration ([ name : Symbol ] [ type : c-type ]) #:transparent)
(struct c-decl-func c-declaration ([ name : Symbol ] [ sig : c-signature ] [ body : c-statement ]) #:transparent)
(define-type c-modifier (U 'extern 'static '*))

(struct c-labeled     ([ name : Symbol ] [ body : c-statement ]) #:transparent)
(struct c-labeled-case ([ expected : c-expression ] [ body : c-statement ]) #:transparent)
(struct c-labeled-default ([ body : c-statement ]) #:transparent)
(struct c-expression-statement ([ exp : c-expression ]) #:transparent)
(struct c-switch      ([ actual : c-expression ] [ body : c-statement ]) #:transparent)
(struct c-if  ([ pred : c-expression ] [ consequent : c-statement ] [ alternative : c-statement ]) #:transparent)
(struct c-for ([ init : (Listof c-decl-var) ] [ pred : (Maybe c-expression) ] [ post : (Maybe c-expression) ] [ body : c-statement ]) #:transparent)
(struct c-while ([ pred : c-expression ] [ body : c-statement ]) #:transparent)
(struct c-do-while ([ pred : c-expression ] [ body : c-statement ]) #:transparent)
(struct c-goto ([ target : Symbol ]) #:transparent)
(struct c-block ([ body : c-statements ]) #:transparent)
(struct c-return ([ val : (Maybe c-expression)]) #:transparent)
(struct c-break () #:transparent)
(struct c-continue () #:transparent)

(struct c-sigvar    ([ name : Symbol ] [ type : c-type   ]) #:transparent)

(define-type c-statement (U c-labeled
                            c-labeled-case
                            c-labeled-default
                            c-expression-statement
                            c-switch
                            c-if
                            c-for
                            c-while
                            c-do-while
                            c-goto
                            c-block
                            c-return
                            c-break
                            c-continue
                            c-declaration))

(struct c-const    c-expression ([ value : CValue ]) #:transparent)
(struct c-variable c-expression ([ name : Symbol ]) #:transparent)
(struct c-ternary  c-expression ([ pred : c-expression ] [ consequent : c-expression ] [ alternative : c-expression ]) #:transparent)
(struct c-binop    c-expression ([ op : Symbol ] [ left : c-expression ] [ right : c-expression ]) #:transparent)
(struct c-unop     c-expression ([ op : Symbol ] [ exp : c-expression ]) #:transparent)
(struct c-function-call c-expression ([ func : c-expression ] [ args : c-expressions ]) #:transparent)
(struct c-field-access  c-expression ([ source : c-expression ] [ name : Symbol ]) #:transparent)
(struct c-cast     c-expression ([ type : c-type ] [ exp : c-expression ]) #:transparent)
(struct c-sizeof   c-expression ([ value : (U c-type c-expression)]) #:transparent)
(struct c-array-access c-expression ([ array : c-expression ] [ index : c-expression ]) #:transparent)
(struct c-expression-sequence c-expression ([ exps : c-expressions ]) #:transparent)

(struct c-field-info ([ offset : Integer ] [ size : Integer ]) #:transparent)

(define-type c-value-type (U 'lvalue 'rvalue))
(define-type c-statements (Listof c-statement))
(define-type c-modifiers (Listof c-modifier))
(define-type c-declarations (Listof c-declaration))
(define-type c-expressions (Listof c-expression))
(define-type c-sigvars     (Listof c-sigvar))
(define-type c-signatures  (Listof c-signature))
(define-type c-labeled-cases (Listof c-labeled-case))
(define-type c-labeled-defaults (Listof c-labeled-default))

(define-type FieldTable (HashTable Symbol c-field-info))
(define-type TypeRegistry (HashTable Symbol c-type))
(define-type variable-table (HashTable Symbol c-type))

(: make-field-table (-> FieldTable))
(define (make-field-table) (make-hash))

(: make-variable-table (-> variable-table))
(define (make-variable-table) (make-hash))

(: make-type-registry (-> TypeRegistry))
(define make-type-registry make-hash)

(define-type CQ Sexp)
