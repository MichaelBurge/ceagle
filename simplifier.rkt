#lang typed/racket

(provide simplify)

(require "types.rkt")
(require pyramid/utils)

(define-type (Endo A) (-> A A))

(define-type c-pass (Endo c-unit))

(: simplify c-pass)
(define (simplify u)
  (set! u (pass-constant-folding u))
  u)

(: lift-expression-transformer (Endo (Endo c-expression)))
(define (lift-expression-transformer f)
  (: tf (Endo c-expression))
  (define (tf exp) ((lift-expression-transformer f) exp))
  (λ ([ exp : c-expression ])
    (f (match exp
         [(struct c-const _) exp]
         [(struct c-variable _) exp]
         [(struct c-ternary (pred x1 x2)) (c-ternary (tf pred) (tf x1) (tf x2))]
         [(struct c-binop (op x1 x2)) (c-binop op (tf x1) (tf x2))]
         [(struct c-unop (op x)) (c-unop op (tf x))]
         [(struct c-function-call (func args)) (c-function-call (tf func) (map tf args))]
         [(struct c-field-access (source name)) (c-field-access (tf source) name)]
         [(struct c-cast (ty x)) (c-cast ty (tf x))]
         [(struct c-sizeof ((? c-type?))) exp]
         [(struct c-sizeof ((? c-expression? exp))) (c-sizeof (tf exp))]
         [(struct c-array-access (arr idx)) (c-array-access (tf arr) (tf idx))]
         [(struct c-expression-sequence (exps)) (c-expression-sequence (map tf exps))]
         ))))

(: lift-statement-transformer (Endo (Endo c-statement)))
(define (lift-statement-transformer f)
  (: stf (Endo c-statement))
  (define (stf st) ((lift-statement-transformer f) st))
  (λ ([ st : c-statement ])
    (f (match st
         [(struct c-labeled (name st)) (c-labeled name (stf st))]
         [(struct c-labeled-case (exp st)) (c-labeled-case exp (stf st))]
         [(struct c-labeled-default (st)) (c-labeled-default (stf st))]
         [(struct c-expression-statement (exp)) st]
         [(struct c-switch (exp body)) (c-switch exp (stf body))]
         [(struct c-if (pred cons alt)) (c-if pred (stf cons) (stf alt))]
         [(struct c-for (inits b c body)) (c-for (cast (map stf inits) c-decl-vars) b c (stf body))]
         [(struct c-while (pred body)) (c-while pred (stf body))]
         [(struct c-do-while (pred body)) (c-do-while pred (stf body))]
         [(struct c-goto (name)) st]
         [(struct c-block (body)) (c-block (map stf body))]
         [(struct c-return _) st]
         [(struct c-break _) st]
         [(struct c-continue _) st]
         [(struct c-decl-var _) st]
         [(struct c-decl-type _) st]
         [(struct c-decl-func (name sig body)) (c-decl-func name sig (stf body))]
         ))))

(: expression-transform (-> (Endo c-expression) (Endo c-unit)))
(define (expression-transform f)
  (statement-transform (statement-expression-transform f))
  )

(: statement-expression-transform (-> (Endo c-expression) (Endo c-statement)))
(define (statement-expression-transform f)
  (: tf (Endo c-expression))
  (define tf (lift-expression-transformer f))
  (: stf (Endo c-statement))
  (define (stf st) ((lift-statement-transformer (statement-expression-transform f)) st))
  (λ ([ st : c-statement ])
    (match st
      [(struct c-labeled (name st)) (c-labeled name (stf st))]
      [(struct c-labeled-case (exp body)) (c-labeled-case (tf exp) (stf body))]
      [(struct c-labeled-default (body)) (c-labeled-default (stf body))]
      [(struct c-expression-statement (exp)) (c-expression-statement (tf exp))]
      [(struct c-switch (actual body)) (c-switch (tf actual) (stf body))]
      [(struct c-if (pred cons alt)) (c-if (tf pred) (stf cons) (stf alt))]
      [(struct c-for (inits pred post body)) (c-for (cast (map stf inits) c-decl-vars) (and pred (tf pred)) (and post (tf post)) (stf body))]
      [(struct c-while (pred body)) (c-while (tf pred) (stf body))]
      [(struct c-do-while (pred body)) (c-do-while (tf pred) (stf body))]
      [(struct c-goto (name)) st]
      [(struct c-block (sts)) (c-block (map stf sts))]
      [(struct c-return (x)) (if x (c-return (tf x)) (c-return #f))]
      [(struct c-break _) st]
      [(struct c-continue _) st]
      [(struct c-decl-var (name ty init)) (if init (c-decl-var name ty (tf init)) st)]
      [(struct c-decl-type _) st]
      [(struct c-decl-func (name sig body)) (c-decl-func name sig (stf body))]
      )))

(: statement-transform (-> (Endo c-statement) c-pass))
(define (statement-transform f)
  (: stf (Endo c-statement))
  (define stf (lift-statement-transformer f))
  (λ ([ u : c-unit])
    (destruct c-unit u)
    (c-unit (cast (map stf u-decls) c-declarations))
    ))

(: pass-constant-folding c-pass)
(define pass-constant-folding
  (expression-transform
   (λ ([ exp : c-expression ])
     (match exp
       [(struct c-unop ('- (struct c-const ((? integer? v)))))
        (c-const (- 0 v))]
       [_ exp]))))
