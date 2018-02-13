#lang typed/racket

(provide (all-defined-out))

(define-type CValue (U Integer String))

(define-type (Maybe A) (U A #f))

(struct c-declaration () #:transparent)
(struct c-expression () #:transparent)

(struct c-type-fixed ([ signed? : Boolean ] [ bits : Integer ]) #:transparent)
(struct c-type-struct-field ([ name : Symbol ] [ type : c-type ]) #:transparent)
(struct c-type-struct ([ fs : c-type-struct-fields ]) #:transparent)

(define-type c-type-struct-fields (Listof c-type-struct-field))
(define-type c-type (U c-type-fixed c-type-struct))

(struct c-unit  ([ decls : c-declarations ]) #:transparent)
(struct c-decl-var  c-declaration ([ name : Symbol ] [ type : c-type ] [ init : (Maybe c-expression) ] [ mods : c-modifiers ]) #:transparent)
(struct c-decl-type c-declaration ([ name : Symbol ] [ type : c-type ]) #:transparent)
(struct c-decl-func c-declaration ([ name : Symbol ] [ sig : c-signature ] [ body : c-statement ]) #:transparent)
(define-type c-modifier (U 'extern 'static))

(struct c-label       ([ name : Symbol ]) #:transparent)
(struct c-switch-case ([ expected : c-expression ] [ body : c-statement ]) #:transparent)
(struct c-switch      ([ actual : c-expression ] [ cs : c-switch-cases] [ default : c-statement ]) #:transparent)
(struct c-if  ([ pred : c-expression ] [ consequent : c-statement ] [ alternative : c-statement ]) #:transparent)
(struct c-for ([ init : (Maybe c-decl-var) ] [ pred : (Maybe c-expression) ] [ post : (Maybe c-expression) ] [ body : c-statement ]) #:transparent)
(struct c-while ([ pred : c-expression ] [ body : c-statement ]) #:transparent)
(struct c-do-while ([ pred : c-expression ] [ body : c-statement ]) #:transparent)
(struct c-goto ([ target : Symbol ]) #:transparent)
(struct c-block ([ body : c-statements ]) #:transparent)
(struct c-return ([ val : (Maybe c-expression)]) #:transparent)
(struct c-break () #:transparent)
(struct c-continue () #:transparent)

(struct c-sigvar    ([ name : Symbol ] [ type : c-type   ]) #:transparent)
(struct c-signature ([ ret  : c-type ] [ args : c-sigvars ]) #:transparent)
(define-type c-statement (U c-label
                            c-expression
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

(define-type c-statements (Listof c-statement))
(define-type c-modifiers (Listof c-modifier))
(define-type c-declarations (Listof c-declaration))
(define-type c-switch-cases (Listof c-switch-case))
(define-type c-expressions (Listof c-expression))
(define-type c-sigvars     (Listof c-sigvar))
(define-type c-signatures  (Listof c-signature))
