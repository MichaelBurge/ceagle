#lang racket

(require (for-syntax racket))

(require "types.rkt")

(provide (rename-out [ c-module-begin #%module-begin ]))

(define-syntax (c-module-begin stx)
  (syntax-case stx ()
    [(_ x) #`(#%module-begin (quote #,(expand-c #'x)))]))

(begin-for-syntax
  (define (expand-c stx)
    (match (syntax->datum stx)
      [`(c . ,xs) `(c-unit ,@(map expand-declaration xs))]))
  (define (expand-declaration x)
    (match x
      [`(function_declaration ,mods ,ret-type ,name ,body) `(c-decl-func ,name (c-signature ,ret-type ()) ,(expand-statement body))]
      [_ (error "expand-declaration: Unknown syntax" x)]))
  (define (expand-statement x)
    (match x
      [`(label_definition ,id)        `(c-label ,id)]
      [`(expression_statement ,exp)   `(c-expression-statement ,(expand-expression exp))]
      [`(switch ,actual . ,cases)     `(c-switch ,actual ,(map expand-switch-case cases))]
      [`(if ,pred ,cons ,alt)         `(c-if ,(expand-expression pred) ,(expand-statement cons) ,(expand-statement alt))]
      [`(for ,init ,pred ,post ,body) `(c-for ,(if (null? init) #f (expand-declaration init))
                                              ,(if (null? pred) #f (expand-expression pred))
                                              ,(if (null? post) #f (expand-expression post))
                                              ,body)]
      [`(while ,pred ,body)           `(c-while ,(expand-expression pred) ,(expand-statement body))]
      [`(do_while ,body ,pred)        `(c-do-while ,(expand-expression pred) ,(expand-statement body))]
      [`(goto ,id)                    `(c-goto ,id)]
      [`(block . ,xs)                 `(c-block (map expand-statement xs))]
      [`(return ,x)                   `(c-return ,(expand-expression x))]
      [`(break)                       `(c-break)]
      [`(continue)                    `(c-continue)]
      [`(declaration ,ty (declaration_variable ,name ,init)) `(c-decl-var ,name ,ty ,(expand-expression init) ())]
      [`(empty)                       `(c-block ())]
      [`(sequence . ,xs)              `(c-block ,(map expand-statement xs))]
      [_ (error "expand-statement: Unknown syntax" x)]
      ))
  (define (expand-expression x)
    (match x
      [`(function_call ,operator . ,operands) `(c-function-call ,(expand-expression operator) ,(map expand-expression operands))]
      [`(integer ,val)                        `(c-const ,val)]
      [`(char ,val)                           `(c-const ,val)]
      [`(variable ,name)                      `(c-variable ,(string->symbol name))]
      [`(ternary ,pred ,cons ,alt)            `(c-ternary ,(expand-expression pred) ,(expand-expression cons) ,(expand-expression alt))]
      [`(binop ,left ,op ,right)              `(c-binop ,(string->symbol op) ,(expand-expression left) ,(expand-expression right))]
      [`(unop ,op ,exp)                       `(c-unop ,(string->symbol op) ,(expand-expression exp))]
      [`(postop ,exp ,op)                     `(c-unop ,(string->symbol op) ,(expand-expression exp))] ; TODO: Post-op should be different from unop.
      [_ (error "expand-expression: Unknown syntax" x)]
      ))
  (define (expand-switch-case x)
    (match x
      [`(switch_case ,expected ,seq) `(c-switch-case ,expected (c-block ,(map expand-statement seq)))]
      [`(switch_default ,seq)        `(c-switch-default (c-block ,(map expand-statement seq)))]
      [_ (error "expand-switch-case: Unknown syntax" x)]
      ))
  )
