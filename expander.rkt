#lang racket

(require "types.rkt")

(provide expand-c)

(define (expand-c stx)
  (match (syntax->datum stx)
    [`(c . ,xs) (c-unit (map expand-declaration xs))]))

(define (expand-declaration x)
  (match x
    [`(function_declaration ,mods ,ret-type ,name ,args ,body)
     (c-decl-func (string->symbol name)
                  (c-signature (expand-type ret-type)
                               (map expand-argument args))
                  (expand-statement body))]
    [`(function_declaration ,mods ,ret-type ,name ,body)
     (c-decl-func (string->symbol name)
                  (c-signature (expand-type ret-type) '())
                  (expand-statement body))]
    [`(typedef ,ty ,name) (c-decl-type (string->symbol name)
                                       (expand-type ty))]
    [`(declaration ,ty (declaration_variable (variable (variable_modifier ,mods) ... ,name) ,init ...))
     (let ([ init-exp (if (null? init)
                          #f
                          (expand-expression (first init)))])
       (c-decl-var (string->symbol name) (apply-mods (expand-type ty) mods) init-exp))]
    [_ (error "expand-declaration: Unknown syntax" x)]
    ))

(define (expand-statement x)
  (match x
    [`(label_definition ,id ,stmt)  (c-block (list (c-label (string->symbol id)) (expand-statement stmt)))]
    [`(expression_statement ,exp)   (c-expression-statement (expand-expression exp))]
    [`(switch ,actual . ,cases)     (c-switch actual (map expand-switch-case cases))]
    [`(if ,pred ,cons)              (c-if (expand-expression pred) (expand-statement cons) (c-block '()))]
    [`(if ,pred ,cons ,alt)         (c-if (expand-expression pred) (expand-statement cons) (expand-statement alt))]
    [`(for ,init ,pred ,post ,body) (c-for (if (null? init) #f (expand-statement init))
                                           (if (null? pred) #f (expand-expression pred))
                                           (if (null? post) #f (expand-expression post))
                                           (expand-statement body))]
    [`(while ,pred ,body)           (c-while (expand-expression pred) (expand-statement body))]
    [`(do_while ,body ,pred)        (c-do-while (expand-expression pred) (expand-statement body))]
    [`(goto ,id)                    (c-goto (string->symbol id))]
    [`(block . ,xs)                 (c-block (map expand-statement xs))]
    [`(return ,x)                   (c-return (expand-expression x))]
    [`(break)                       (c-break)]
    [`(continue)                    (c-continue)]
    [`(declaration ,ty (declaration_variable (variable (variable_modifier ,mods) ... ,name) ,init ...))
     (let ([ init-exp (if (null? init)
                          #f
                          (expand-expression (first init)))])
       (c-decl-var (string->symbol name) (apply-mods (expand-type ty) mods) init-exp))]
    [`(empty)                       (c-block '())]
    [`(sequence . ,xs)              (c-block (map expand-statement xs))]
    [_ (error "expand-statement: Unknown syntax" x)]
    ))

(define (expand-expression x)
  (match x
    [`(function_call ,operator . ,operands) (c-function-call (expand-expression operator) (map expand-expression operands))]
    [`(integer ,val)                        (c-const val)]
    [`(char ,val)                           (c-const (string-ref val 0))]
    [`(variable ,name)                      (c-variable (string->symbol name))]
    [`(variable (variable_modifier "*") ,name) (c-unop '* (c-variable (string->symbol name)))]
    [`(ternary ,pred ,cons ,alt)            (c-ternary (expand-expression pred) (expand-expression cons) (expand-expression alt))]
    [`(binop ,left "." (variable ,right))   (c-field-access (expand-expression left) (string->symbol right))]
    [`(binop ,left ,op ,right)              (c-binop (string->symbol op) (expand-expression left) (expand-expression right))]
    [`(unop ,op ,exp)                       (c-unop (string->symbol op) (expand-expression exp))]
    [`(postop ,exp ,op)                     (c-unop (string->symbol op) (expand-expression exp))] ; TODO: Post-op should be different from unop.
    [`(c_cast ,ty ,exp)                     (c-cast (expand-type ty) (expand-expression exp))]
    [_ (error "expand-expression: Unknown syntax" x)]
    ))

(define (expand-switch-case x)
  (match x
    [`(switch_case ,expected ,seq) (c-switch-case expected (c-block (map expand-statement seq)))]
    [`(switch_default ,seq)        (c-switch-default (c-block (map expand-statement seq)))]
    [_ (error "expand-switch-case: Unknown syntax" x)]
    ))

(define (expand-type x)
  (match x
    ['(signed_char)            (c-type-fixed #t 8)]
    ['(unsigned_char)          (c-type-fixed #f 8)]
    ['(signed_int)             (c-type-fixed #t 256)]
    ['(unsigned_int)           (c-type-fixed #f 256)]
    [`(struct ,(? string? name) ... . ,fields) (c-type-struct
                                                (map expand-struct-field fields))]
    [`(union ,(? string? name) ... . ,fields) (c-type-union
                                               (map expand-struct-field fields))]
    [(? string? name)          (c-type-alias (string->symbol name))]
    [_ (error "expand-type: Unknown type" x)]))

(define (expand-struct-field x)
  (match x
    [`(declaration ,ty)
     (match (expand-type ty)
       [(and (struct c-type-union (fields)) x) (c-type-struct-field #f x)]
       [_ (error "expand-struct-field: Only unions can be anonymous")]
       )]
    [`(declaration ,ty (declaration_variable (variable (variable_modifier ,mods) ... ,name)))
     (c-type-struct-field (string->symbol name) (apply-mods (expand-type ty) mods))]
    [_ (error "expand-struct-field: Unknown syntax" x)]))

(define (expand-argument x)
  (match x
    [`(function_argument ,type (variable (variable_modifier ,mods) ... ,name))
     (c-sigvar (string->symbol name) (apply-mods (expand-type type) mods))]
    [_ (error "expand-argument: Unknown syntax" x)]
    ))

(define (apply-mods ty mods)
  (match mods
    [(? null?) ty]
    [(cons "*" rest) (c-type-pointer (apply-mods ty rest))]
    [_ (error "apply-mods: Unhandled case" ty mods)]
    ))

(define (collapse-nested-anonymous-structs x) (flatten x))
