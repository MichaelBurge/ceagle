#lang racket

(require "types.rkt")
(require (for-syntax syntax/parse))
(require (for-syntax syntax/parse/debug))

(provide expand-translation-unit
         specifier-set-type)

(begin-for-syntax
  (define-syntax-class translation-unit
    #:attributes (ast)
    [pattern ((~literal translation_unit) decls:external-declaration ...)
             #:with ast #'(c-unit (append decls.statements ...))])

  (define-syntax-class external-declaration
    #:attributes (statements)
    [pattern ((~literal external_declaration) (~or* decl:function-definition decl:declaration decl:any-include))
             #:with statements #'decl.statements]
    )

  (define-syntax-class function-definition
    #:attributes (statements)
    [pattern ((~literal function_definition) specs:declaration-specifiers decl:declarator stmt:compound-statement)
             #:with statement #'(c-decl-func decl.name (c-signature specs.type decl.sigvars) stmt.statement)
             #:with statements #'(list statement)]
    [pattern ((~literal function_definition) specs:declaration-specifiers decl:declarator args:declaration-list stmt:compound-statement)
             #:with statement #'(error "Parsing function-definition: Unhandled case") ; (c-decl-func decl.name (c-signature specs.type args.declarations) stmt.statement)
             #:with statements #'(list statement)]
    )

  (define-syntax-class declaration-specifiers
    #:attributes (specifiers type typedef?)
    [pattern ((~literal declaration_specifiers) (~or* spec:storage-class-specifier spec:type-specifier spec:type-qualifier) ...)
             #:with specifiers #'(list spec.specifier ...)
             #:with type #'(specifier-set-type t-int spec.specifier ...)
             #:with typedef? #'(set-member? (set spec.specifier ...) 'typedef)]
    )
  (define-syntax-class declarator
    #:attributes (name sigvars type-modifier)
    [pattern ((~literal declarator) ptr:pointer decl:direct-declarator)
             #:with name #'decl.name
             #:with sigvars #'decl.sigvars
             #:with type-modifier #'ptr.type-modifier]
    [pattern ((~literal declarator) decl:direct-declarator)
             #:with name #'decl.name
             #:with sigvars #'decl.sigvars
             #:with type-modifier #'identity]
    )
  (define-syntax-class direct-declarator
    #:attributes (name sigvars)
    [pattern ((~literal direct_declarator) name-sym:identifier)
             #:with name #'(quote name-sym)
             #:with sigvars '()]
    [pattern ((~literal direct_declarator) decl:direct-declarator "[" (~optional constant_expression) "]")
             #:with name #'decl.name
             #:with sigvars #'()]
    [pattern ((~literal direct_declarator) decl:direct-declarator "(" params:parameter-type-list ")")
             #:with name #'decl.name
             #:with sigvars #'params.sigvars]
    [pattern ((~literal direct_declarator) decl:direct-declarator "(" params:identifier-list ")")
             #:with name #'(error "Parsing direct-declarator: Unhandled nameless variable")
             #:with sigvars #'(error "Parsing direct-declarator: Unhandled nameless variable")]
    [pattern ((~literal direct_declarator) decl:direct-declarator "(" ")")
             #:with name #'decl.name
             #:with sigvars #''()]
    )

  (define-syntax-class identifier-list
    #:attributes (names)
    [pattern ((~literal identifier_list) id:identifier ...)
             #:with names #'(list id ...)]
    )

  (define-syntax-class parameter-type-list
    #:attributes (sigvars)
    [pattern ((~literal parameter_type_list) params:parameter-list)
             #:with sigvars #'params.sigvars]
    [pattern ((~literal parameter_type_list) params:parameter-list (~literal ELLIPSIS))
             #:with sigvars #'(error "Parsing parameter-type-list: Unhandled varargs")]
    )

  (define-syntax-class parameter-list
    #:attributes (sigvars)
    [pattern ((~literal parameter_list) param:parameter-declaration ...)
             #:with sigvars #'(list param.sigvar ...)]
    )

  (define-syntax-class parameter-declaration
    #:attributes (sigvar)
    [pattern ((~literal parameter_declaration) specs:declaration-specifiers decl:declarator)
             #:with sigvar #'(c-sigvar decl.name (decl.type-modifier (apply specifier-set-type (cons t-int specs.specifiers))))]
    [pattern ((~literal parameter_declaration) specs:declaration-specifiers decl:abstract-declarator)
             #:with sigvar #'(error "Parsing parameter-declaration: Unhandled abstract declarator")]
    [pattern ((~literal parameter_declaration) specs:declaration-specifiers)
             #:with sigvar #'(error "Parsing parameter-declaration: Unhandled only declaration-specifiers")]
    )

  (define-syntax-class declaration-list
    #:attributes (declarations)
    [pattern ((~literal declaration_list) decl:declaration ...)
             #:with declarations #'(append decl.declarations ...)]
    )
  (define-syntax-class declaration
    #:attributes (declarations statements)
    [pattern ((~literal declaration) specs:declaration-specifiers (~optional decls:init-declarator-list))
             ; If there are no variables declared, we still need to generate a fake one so that
             ; struct x { ... } forms create a "struct x" type as a side effect.
             #:with declarations (if (attribute decls)
                                     #'(let ([ stmts
                                               (map (λ (type->decl) (type->decl specs.type))
                                                    decls.type->declarations)])
                                         (if specs.typedef?
                                             (map decl-var->typedef stmts)
                                             stmts))
                                     #'(list (c-decl-var (gensym 'unused-declaration) specs.type #f)))
             #:with statements #'declarations]
    )
  (define-syntax-class init-declarator-list
    #:attributes (type->declarations)
    [pattern ((~literal init_declarator_list) decl:init-declarator ...)
             #:with type->declarations #'(list decl.type->declaration ...)]
    )
  (define-syntax-class init-declarator
    #:attributes (type->declaration)
    [pattern ((~literal init_declarator) decl:declarator (~optional init:initializer))
             #:with type->declaration (if (attribute init.expression)
                                          #'(λ (ty) (c-decl-var decl.name (decl.type-modifier ty) init.expression))
                                          #'(λ (ty) (c-decl-var decl.name (decl.type-modifier ty) #f)))]
    )
  (define-syntax-class initializer
    #:attributes (expression)
    [pattern ((~literal initializer) (~or* expr:assignment-expression expr:initializer-list))
             #:with expression #'expr.expression]
    )
  (define-syntax-class initializer-list
    #:attributes (expression)
    [pattern ((~literal initializer_list) exp:initializer ...)
             #:with expression #'(c-expression-array (list exp.expression ...))]
    )
  (define-syntax-class statement
    #:attributes (statement statements)
    [pattern ((~literal statement) (~or* stmt:labeled-statement
                                         stmt:compound-statement
                                         stmt:expression-statement
                                         stmt:selection-statement
                                         stmt:iteration-statement
                                         stmt:jump-statement))
             #:with statement #'stmt.statement
             #:with statements #'(list stmt.statement)]
    )
  (define-syntax-class labeled-statement
    #:attributes (statement)
    [pattern ((~literal labeled_statement) (~literal DEFAULT) stmt:statement)
     #:with statement #'(c-labeled-default stmt.statement)]
    [pattern ((~literal labeled_statement) (~literal CASE) expr:constant-expression stmt:statement)
     #:with statement #'(c-labeled-case expr.expression stmt.statement)]
    [pattern ((~literal labeled_statement) label:identifier stmt:statement)
     #:with statement #'(c-labeled (quote label) stmt.statement)]
    )
  (define-syntax-class compound-statement
    #:attributes (statement)
    [pattern ((~literal compound_statement) "{" (~or* stmt:declaration stmt:statement) ... "}")
             #:with statement #'(c-block (append stmt.statements ...))]
    )
  (define-syntax-class expression-statement
    #:attributes (statement expression)
    [pattern ((~literal expression_statement) (~optional expr:expression))
             #:with expression (if (attribute expr) #'expr.expression #'(c-const 0 #t))
             #:with statement (if (attribute expr) #'(c-expression-statement expression) #'(c-block '()))]
    )
  (define-syntax-class selection-statement
    #:attributes (statement)
    [pattern ((~literal selection_statement) (~literal IF) cond:expression stmt:statement (~optional s-else:statement))
             #:with statement #`(c-if cond.expression stmt.statement #,(if (attribute s-else) #'s-else.statement #'(c-block '())))]
    [pattern ((~literal selection_statement) (~literal SWITCH) cond:expression stmt:statement)
             #:with statement #`(c-switch cond.expression stmt.statement)]
    )

  (define-syntax-class iteration-statement
    #:attributes (statement)
    #:datum-literals (WHILE DO FOR)
    [pattern ((~literal iteration_statement) WHILE cond:expression body:statement)
             #:with statement #'(c-while cond.expression body.statement)]
    [pattern ((~literal iteration_statement) DO body:statement cond:expression)
             #:with statement #'(c-do-while cond.expression body.statement)]
    [pattern ((~literal iteration_statement) FOR init:declaration cond:expression-statement (~optional post:expression #:defaults ([ post.expression #'#f])) body:statement)
             #:with statement #`(c-for init.declarations cond.expression post.expression body.statement)]
    )
  (define-syntax-class jump-statement
    #:attributes (statement)
    [pattern ((~literal jump_statement) (~literal GOTO) label:identifier) #:with statement #'(c-goto (quote label))]
    [pattern ((~literal jump_statement) (~literal CONTINUE)) #:with statement #'(c-continue)]
    [pattern ((~literal jump_statement) (~literal BREAK)) #:with statement #'(c-break)]
    [pattern ((~literal jump_statement) (~literal RETURN)) #:with statement #'(c-return (quote #f))]
    [pattern ((~literal jump_statement) (~literal RETURN) expr:expression) #:with statement #'(c-return expr.expression)]
    )
  (define-syntax-class expression
    #:attributes (expression)
    [pattern ((~literal expression) expr:assignment-expression)
             #:with expression #'expr.expression]
    [pattern ((~literal expression) left:expression "," right:assignment-expression)
             #:with expression #'(c-expression-sequence (list left.expression right.expression))]
    )
  (define-syntax-class assignment-expression
    #:attributes (expression)
    [pattern ((~literal assignment_expression) expr:conditional-expression)
             #:with expression #'expr.expression]
    [pattern ((~literal assignment_expression) left:unary-expression op:assignment-operator right:assignment-expression)
             #:with expression #'(c-binop (quote op.operator) left.expression right.expression)]
    )
  (define-syntax-class conditional-expression
    #:attributes (expression)
    [pattern ((~literal conditional_expression) expr:logical-or-expression)
             #:with expression #'expr.expression]
    [pattern ((~literal conditional_expression) condition:logical-or-expression "?" consequent:expression ":" alternative:conditional-expression)
             #:with expression #'(c-ternary condition.expression consequent.expression alternative.expression)]
    )
  (define-syntax-class logical-or-expression
    #:attributes (expression)
    [pattern ((~literal logical_or_expression) expr:logical-and-expression)
             #:with expression #'expr.expression]
    [pattern ((~literal logical_or_expression) left:logical-or-expression "||" right:logical-and-expression)
             #:with expression #'(c-binop '\|\| left.expression right.expression)]
    )
  (define-syntax-class logical-and-expression
    #:attributes (expression)
    [pattern ((~literal logical_and_expression) expr:inclusive-or-expression)
             #:with expression #'expr.expression]
    [pattern ((~literal logical_and_expression) left:logical-and-expression "&&" right:inclusive-or-expression)
             #:with expression #'(c-binop '&& left.expression right.expression)]
    )
  (define-syntax-class inclusive-or-expression
    #:attributes (expression)
    [pattern ((~literal inclusive_or_expression) expr:exclusive-or-expression)
             #:with expression #'expr.expression]
    [pattern ((~literal inclusive_or_expression) left:inclusive-or-expression "|" right:exclusive-or-expression)
             #:with expression #'(c-binop '\| left.expression right.expression)]
    )
  (define-syntax-class exclusive-or-expression
    #:attributes (expression)
    [pattern ((~literal exclusive_or_expression) expr:and-expression)
             #:with expression #'expr.expression]
    [pattern ((~literal exclusive_or_expression) left:exclusive-or-expression "^" right:and-expression)
             #:with expression #'(c-binop '^ left.expression right.expression)]
    )
  (define-syntax-class and-expression
    #:attributes (expression)
    [pattern ((~literal and_expression) expr:equality-expression)
             #:with expression #'expr.expression]
    [pattern ((~literal and_expression) left:and-expression "&" right:equality-expression)
             #:with expression #'(c-binop '& left.expression right.expression)]
    )
  (define-syntax-class equality-expression
    #:attributes (expression)
    [pattern ((~literal equality_expression) expr:relational-expression)
             #:with expression #'expr.expression]
    [pattern ((~literal equality_expression) left:equality-expression "==" right:relational-expression)
             #:with expression #'(c-binop '== left.expression right.expression)]
    [pattern ((~literal equality_expression) left:equality-expression "!=" right:relational-expression)
             #:with expression #'(c-binop '!= left.expression right.expression)]
    )
  (define-syntax-class relational-expression
    #:attributes (expression)
    [pattern ((~literal relational_expression) expr:shift-expression)
             #:with expression #'expr.expression]
    [pattern ((~literal relational_expression) left:relational-expression "<" right:shift-expression)
             #:with expression #'(c-binop '< left.expression right.expression)]
    [pattern ((~literal relational_expression) left:relational-expression ">" right:shift-expression)
             #:with expression #'(c-binop '> left.expression right.expression)]
    [pattern ((~literal relational_expression) left:relational-expression "<=" right:shift-expression)
             #:with expression #'(c-binop '<= left.expression right.expression)]
    [pattern ((~literal relational_expression) left:relational-expression ">=" right:shift-expression)
             #:with expression #'(c-binop '>= left.expression right.expression)]
    )
  (define-syntax-class shift-expression
    #:attributes (expression)
    [pattern ((~literal shift_expression) expr:additive-expression)
             #:with expression #'expr.expression]
    [pattern ((~literal shift_expression) left:shift-expression "<<" right:additive-expression)
             #:with expression #'(c-binop '<< left.expression right.expression)]
    [pattern ((~literal shift_expression) left:shift-expression ">>" right:additive-expression)
             #:with expression #'(c-binop '>> left.expression right.expression)]
    )
  (define-syntax-class additive-expression
    #:attributes (expression)
    [pattern ((~literal additive_expression) expr:multiplicative-expression)
             #:with expression #'expr.expression]
    [pattern ((~literal additive_expression) left:additive-expression "+" right:multiplicative-expression)
             #:with expression #'(c-binop '+ left.expression right.expression)]
    [pattern ((~literal additive_expression) left:additive-expression "-" right:multiplicative-expression)
             #:with expression #'(c-binop '- left.expression right.expression)]
    )

  (define-syntax-class multiplicative-expression
    #:attributes (expression)
    [pattern ((~literal multiplicative_expression) expr:cast-expression)
             #:with expression #'expr.expression]
    [pattern ((~literal multiplicative_expression) left:multiplicative-expression "*" right:cast-expression)
             #:with expression #'(c-binop '* left.expression right.expression)]
    [pattern ((~literal multiplicative_expression) left:multiplicative-expression "/" right:cast-expression)
             #:with expression #'(c-binop '/ left.expression right.expression)]
    [pattern ((~literal multiplicative_expression) left:multiplicative-expression "%" right:cast-expression)
             #:with expression #'(c-binop '% left.expression right.expression)]
    )

  (define-syntax-class cast-expression
    #:attributes (expression)
    [pattern ((~literal cast_expression) expr:unary-expression)
             #:with expression #'expr.expression]
    [pattern ((~literal cast_expression) "(" ty:type-name ")" expr:cast-expression)
             #:with expression #'(c-cast ty.type expr.expression)]
    )

  (define-syntax-class unary-operator
    #:attributes (operator)
    [pattern ((~literal unary_operator) "&") #:with operator '&]
    [pattern ((~literal unary_operator) "*") #:with operator '*]
    [pattern ((~literal unary_operator) "+") #:with operator '+]
    [pattern ((~literal unary_operator) "-") #:with operator '-]
    [pattern ((~literal unary_operator) "~") #:with operator '~]
    [pattern ((~literal unary_operator) "!") #:with operator '!]
    )

  (define-syntax-class unary-expression
    #:attributes (expression)
    [pattern ((~literal unary_expression) expr:postfix-expression)                #:with expression #'expr.expression]
    [pattern ((~literal unary_expression) "++" expr:unary-expression)             #:with expression #'(c-unop 'pre++ expr.expression)]
    [pattern ((~literal unary_expression) "--" expr:unary-expression)             #:with expression #'(c-unop 'pre-- expr.expression)]
    [pattern ((~literal unary_expression) op:unary-operator expr:cast-expression) #:with expression #'(c-unop (quote op.operator) expr.expression)]
    [pattern ((~literal unary_expression) "sizeof" expr:unary-expression)         #:with expression #'(c-sizeof expr.expression)]
    [pattern ((~literal unary_expression) "sizeof" "(" ty:type-name ")")          #:with expression #'(c-sizeof ty.type)]
    )
  (define-syntax-class postfix-expression
    #:attributes (expression)
    [pattern ((~literal postfix_expression) expr:primary-expression) #:with expression #'expr.expression]
    [pattern ((~literal postfix_expression) array:postfix-expression "[" index:expression "]") #:with expression #'(c-array-access array.expression index.expression)]
    [pattern ((~literal postfix_expression) function:postfix-expression "(" ")") #:with expression #'(c-function-call function.expression '())]
    [pattern ((~literal postfix_expression) function:postfix-expression "(" args:argument-expression-list ")") #:with expression #'(c-function-call function.expression args.expressions)]
    [pattern ((~literal postfix_expression) record:postfix-expression "." field:identifier)  #:with expression #'(c-field-access record.expression (quote field))]
    [pattern ((~literal postfix_expression) record:postfix-expression "->" field:identifier) #:with expression #'(c-field-access (c-unop '* record.expression) (quote field))]
    [pattern ((~literal postfix_expression) expr:postfix-expression "++") #:with expression #'(c-unop 'post++ expr.expression)]
    [pattern ((~literal postfix_expression) expr:postfix-expression "--") #:with expression #'(c-unop 'post-- expr.expression)]
    )
  (define-syntax-class primary-expression
    #:attributes (expression)
    [pattern ((~literal primary_expression) name:identifier) #:with expression #'(c-variable (quote name))]
    [pattern ((~literal primary_expression) value:uinteger)  #:with expression #'(c-const (quote value.value) #f)]
    [pattern ((~literal primary_expression) value:sinteger)  #:with expression #'(c-const (quote value.value) #t)]
    [pattern ((~literal primary_expression) value:string)    #:with expression #'(c-const (quote value) #f)]
    [pattern ((~literal primary_expression) "(" expr:expression ")") #:with expression #'expr.expression]
    )
  (define-syntax-class constant-expression
    #:attributes (expression)
    [pattern ((~literal constant_expression) expr:conditional-expression) #:with expression #'expr.expression]
    )
  (define-syntax-class type-name
    #:attributes (type)
    [pattern ((~literal type_name) specs:specifier-qualifier-list) #:with type #'specs.type]
    [pattern ((~literal type_name) specs:specifier-qualifier-list decl:abstract-declarator) #:with type #'(decl.type-modifier specs.type)]
    )

  (define-syntax-class abstract-declarator
    #:attributes (type-modifier)
    [pattern ((~literal abstract_declarator) ptr:pointer) #:with type-modifier #'c-type-pointer]
    [pattern ((~literal abstract_declarator) decl:direct_abstract_declarator) #:with type-modifier #'decl.type-modifier]
    [pattern ((~literal abstract_declarator) ptr:pointer decl:direct_abstract_declarator) #:with type-modifier #'(error "Parsing abstract-declarator: Unimplemented")]
    )

  (define-syntax-class type-qualifier
    #:attributes (specifier)
    #:datum-literals (CONST VOLATILE)
    [pattern ((~literal type_qualifier) CONST) #:with specifier #''const]
    [pattern ((~literal type_qualifier) VOLATILE) #:with specifier #''volatile]
    )

  (define-syntax-class direct_abstract_declarator)

  (define-syntax-class argument-expression-list
    #:attributes (expressions)
    [pattern ((~literal argument_expression_list) exprs:assignment-expression ...) #:with expressions #'(list exprs.expression ...)]
    )
  (define-syntax-class specifier-qualifier-list
    #:attributes (specifiers type)
    [pattern ((~literal specifier_qualifier_list) (~or* spec:type-specifier spec:type-qualifier) ...)
             #:with specifiers #'(list spec.specifier ...)
             #:with type #'(apply specifier-set-type (cons t-int specifiers))]
    )
  (define-syntax-class type-specifier
    #:attributes (specifier)
    #:datum-literals (VOID CHAR SHORT INT LONG FLOAT DOUBLE SIGNED UNSIGNED)
    [pattern ((~literal type_specifier) VOID)  #:with specifier #''void]
    [pattern ((~literal type_specifier) CHAR)  #:with specifier #''char]
    [pattern ((~literal type_specifier) SHORT) #:with specifier #''short]
    [pattern ((~literal type_specifier) INT)   #:with specifier #''int]
    [pattern ((~literal type_specifier) LONG)  #:with specifier #''long]
    [pattern ((~literal type_specifier) FLOAT) #:with specifier #''float]
    [pattern ((~literal type_specifier) DOUBLE) #:with specifier #''double]
    [pattern ((~literal type_specifier) SIGNED) #:with specifier #''signed]
    [pattern ((~literal type_specifier) UNSIGNED) #:with specifier #''unsigned]
    [pattern ((~literal type_specifier) ty:struct-or-union-specifier) #:with specifier #'ty.type]
    [pattern ((~literal type_specifier) ty:enum-specifier) #:with specifier #'ty.type]
    [pattern ((~literal type_specifier) ty:identifier) #:with specifier #'(c-type-alias (quote ty) #f)]
    )
  (define-syntax-class storage-class-specifier
    #:attributes (specifier)
    #:datum-literals (TYPEDEF EXTERN STATIC AUTO REGISTER)
    [pattern ((~literal storage_class_specifier) TYPEDEF) #:with specifier #''typedef]
    [pattern ((~literal storage_class_specifier) EXTERN) #:with specifier #''extern]
    [pattern ((~literal storage_class_specifier) STATIC) #:with specifier #''static]
    [pattern ((~literal storage_class_specifier) AUTO) #:with specifier #''auto]
    [pattern ((~literal storage_class_specifier) REGISTER) #:with specifier #''register]
    )
  (define-syntax-class pointer
    #:attributes (type-modifier)
    [pattern ((~literal pointer)) #:with type-modifier #'c-type-pointer]
    [pattern ((~literal pointer) tqls:type-qualifier ... (~optional ptr:pointer)) #:with type-modifier #'(error "Parsing pointer: Unimplemented")]
    )
  (define-syntax-class struct-or-union-specifier
    #:attributes (type)
    #:datum-literals (STRUCT UNION)
    [pattern ((~literal struct_or_union_specifier) STRUCT name:identifier)
             #:with type #'(c-type-alias (quote name) 'struct)]
    [pattern ((~literal struct_or_union_specifier) UNION name:identifier)
             #:with type #'(c-type-alias (quote name) 'union)]
    [pattern ((~literal struct_or_union_specifier) STRUCT (~optional name:identifier) "{" decls:struct-declaration ... "}")
             #:with type (if (attribute name)
                             #'(c-type-struct (quote name) (append decls.struct-fields ...))
                             #'(c-type-struct #f (append decls.struct-fields ...)))]
    [pattern ((~literal struct_or_union_specifier) UNION (~optional name:identifier) "{" decls:struct-declaration ... "}")
             #:with type (if (attribute name)
                             #'(c-type-union (quote name) (append decls.struct-fields ...))
                             #'(c-type-union #f (append decls.struct-fields ...)))]
    )
  (define-syntax-class struct-declaration
    #:attributes (struct-fields)
    [pattern ((~literal struct_declaration) specifiers:specifier-qualifier-list)
             #:with struct-fields #'(list (c-type-struct-field #f specifiers.type))]
    [pattern ((~literal struct_declaration) specifiers:specifier-qualifier-list decls:struct-declarator-list)
             #:with struct-fields #'(map (λ (type->field) (type->field specifiers.type))
                                         decls.type->fields)]
    )
  (define-syntax-class struct-declarator-list
    #:attributes (type->fields)
    [pattern ((~literal struct_declarator_list) decls:struct-declarator ...)
             #:with type->fields #'(list decls.type->field ...)]
    )
  (define-syntax-class struct-declarator
    #:attributes (type->field)
    [pattern ((~literal struct_declarator) decl:declarator) #:with type->field #'(λ (ty) (c-type-struct-field decl.name (decl.type-modifier ty)))]
    [pattern ((~literal struct_declarator) expr:constant-expression) #:with type->field #'(error "Parsing struct-declarator: Unhandled case")]
    [pattern ((~literal struct_declarator) decl:declarator expr:constant-expression) #:with type->field #'(error "Parsing struct-declarator: Unhandled case")]
    )
  (define-syntax-class enum-specifier
    #:attributes (type)
    [pattern ((~literal enum_specifier) x ...) #:with type #'(error "Parsing enum-specifier: Unhandled case")]
    )

  (define-syntax-class any-include
    #:attributes (statements)
    [pattern ((~literal any_include) path:string)
             #:with statements #''()]
    )

  (define-syntax-class uinteger
    [pattern ((~literal uinteger) value:integer)]
    )
  (define-syntax-class sinteger
    [pattern ((~literal sinteger) value:integer)]
    )

  (define-syntax-class assignment-operator
    #:attributes (operator)
    [pattern ((~literal assignment_operator) "=") #:with operator '=]
    [pattern ((~literal assignment_operator) "*=") #:with operator '*=]
    [pattern ((~literal assignment_operator) "/=") #:with operator '/=]
    [pattern ((~literal assignment_operator) "%=") #:with operator '%=]
    [pattern ((~literal assignment_operator) "+=") #:with operator '+=]
    [pattern ((~literal assignment_operator) "-=") #:with operator '-=]
    [pattern ((~literal assignment_operator) "<<=") #:with operator '<<=]
    [pattern ((~literal assignment_operator) ">>=") #:with operator '>>=]
    [pattern ((~literal assignment_operator) "&=") #:with operator '&=]
    [pattern ((~literal assignment_operator) "^=") #:with operator '^=]
    [pattern ((~literal assignment_operator) "|=") #:with operator '\|= ]
    )

  )

(define-syntax (expand-translation-unit stx)
  (syntax-parse stx
    [(_ unit:translation-unit) #'unit.ast]
    ))

(define (decl-var->typedef x)
  (match x
    [(struct c-decl-var (name ty #f)) (c-decl-type name ty)]
    [_ (error "decl-var->typedef: Unable to convert variable to type declaration" x)]
    ))

(define (specifier-set-type init-ty . specifiers)
  (define (apply-specifier x ty)
    (match x
      ['int ty]
      ['signed ty]
      ['typedef ty] ; Ignore typedef, but we still need to determine a type from the other specifiers.
      ['const ty ]
      ['static ty ]
      ['void (c-type-void)]
      ['unsigned
       (match ty
         [(struct c-type-fixed (signed? size)) (c-type-fixed #f size)]
         [_ (error "specifier-set-type: Unhandle unsigned type")])]
      ['char (match ty [(struct c-type-fixed (signed? size))
                        (c-type-fixed signed? size)])]
      [(struct c-type-alias _) x]
      [(struct c-type-struct _) x]
      [(struct c-type-union _) x]
      [_ (error "specifier-set-type: Unhandled specifier" x)]))
  (let loop ([ xs specifiers ]
             [ ty init-ty ])
    (match xs
      ['() ty]
      [(cons y ys) (loop ys (apply-specifier y ty))]
      )))
