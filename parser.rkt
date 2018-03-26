#lang brag

; Reference: https://www.lysator.liu.se/c/ANSI-C-grammar-y.html

translation_unit: ( external_declaration | /";") *

primary_expression
: IDENTIFIER
| INTEGER
| STRING_LITERAL
| "(" expression ")"

postfix_expression
: primary_expression
| postfix_expression "[" expression "]"
| postfix_expression "(" [ argument_expression_list ] ")"
| postfix_expression "." IDENTIFIER
| postfix_expression "->" IDENTIFIER
| postfix_expression "++"
| postfix_expression "--"

argument_expression_list: [ assignment_expression /"," ]* assignment_expression

unary_expression
: postfix_expression
| "++" unary_expression
| "--" unary_expression
| unary_operator cast_expression
| SIZEOF unary_expression
| SIZEOF "(" type_name ")"

unary_operator
: "&"
| "*"
| "+"
| "-"
| "~"
| "!"

cast_expression
: unary_expression
| "(" type_name ")" cast_expression

multiplicative_expression
: cast_expression
| multiplicative_expression "*" cast_expression
| multiplicative_expression "/" cast_expression
| multiplicative_expression "%" cast_expression

additive_expression
: multiplicative_expression
| additive_expression "+" multiplicative_expression
| additive_expression "-" multiplicative_expression

shift_expression
: additive_expression
| shift_expression "<<" additive_expression
| shift_expression ">>" additive_expression

relational_expression
: shift_expression
| relational_expression "<" shift_expression
| relational_expression ">" shift_expression
| relational_expression "<=" shift_expression
| relational_expression ">=" shift_expression

equality_expression
: relational_expression
| equality_expression "==" relational_expression
| equality_expression "!=" relational_expression

and_expression
: equality_expression
| and_expression "&" equality_expression

exclusive_or_expression
: and_expression
| exclusive_or_expression "^" and_expression

inclusive_or_expression
: exclusive_or_expression
| inclusive_or_expression "|" exclusive_or_expression

logical_and_expression
: inclusive_or_expression
| logical_and_expression "&&" inclusive_or_expression

logical_or_expression
: logical_and_expression
| logical_or_expression "||" logical_and_expression

conditional_expression
: logical_or_expression
| logical_or_expression "?" expression ":" conditional_expression

assignment_expression
: conditional_expression
| unary_expression assignment_operator assignment_expression

assignment_operator
: "="
| "*="
| "/="
| "%="
| "+="
| "-="
| "<<="
| ">>="
| "&="
| "^="
| "|="

expression
: assignment_expression
| expression ',' assignment_expression

constant_expression
: conditional_expression

declaration
: declaration_specifiers [init_declarator_list] /";"

declaration_specifiers: (storage_class_specifier | type_specifier | type_qualifier)+

init_declarator_list: [ init_declarator /"," ]* init_declarator

init_declarator
: declarator
| declarator /"=" initializer

storage_class_specifier
: TYPEDEF
| EXTERN
| STATIC
| AUTO
| REGISTER

type_specifier
: VOID
| CHAR
| SHORT
| INT
| LONG
| FLOAT
| DOUBLE
| SIGNED
| UNSIGNED
| struct_or_union_specifier
| enum_specifier
| TYPE_NAME

struct_or_union_specifier
: (STRUCT | UNION) /[ IDENTIFIER ] [ "{" struct_declaration* "}" ]

struct_declaration
: specifier_qualifier_list [ struct_declarator_list ] /";"

specifier_qualifier_list: (type_specifier | type_qualifier)*

struct_declarator_list: ( struct_declarator /"," )* struct_declarator

struct_declarator
: declarator
| /":" constant_expression
| declarator /":" constant_expression

enum_specifier
: /ENUM [ IDENTIFIER ] [/"{" enumerator_list /"}"]
| /ENUM [ IDENTIFIER ]
| /ENUM                [/"{" enumerator_list /"}"]

enumerator_list
: [ enumerator_list "," ] enumerator

enumerator
: IDENTIFIER ["=" constant_expression]

type_qualifier
: CONST
| VOLATILE

declarator
: [ pointer ] direct_declarator

direct_declarator
: IDENTIFIER
| "(" declarator ")"
| direct_declarator "[" constant_expression "]"
| direct_declarator "[" "]"
| direct_declarator "(" parameter_type_list ")"
| direct_declarator "(" identifier_list ")"
| direct_declarator "(" ")"

pointer
: /"*" type_qualifier* [ pointer ]

parameter_type_list
: parameter_list [ "," ELLIPSIS ]

parameter_list: ( parameter_declaration /",")* parameter_declaration

parameter_declaration
: declaration_specifiers declarator
| declaration_specifiers abstract_declarator
| declaration_specifiers

identifier_list: (IDENTIFIER /",")* IDENTIFIER

type_name
: specifier_qualifier_list [ abstract_declarator ]

abstract_declarator
: pointer
| direct_abstract_declarator
| pointer direct_abstract_declarator

direct_abstract_declarator
: "(" abstract_declarator ")"
| "[" [ constant_expression ] "]"
| direct_abstract_declarator "[" [ constant_expression ]"]"
| "(" [ parameter_type_list ] ")"
| direct_abstract_declarator "(" parameter_type_list ")"

initializer
: assignment_expression
| /"{" initializer_list /[","] /"}"

initializer_list
: [ initializer_list "," ] initializer

statement
: labeled_statement
| compound_statement
| expression_statement
| selection_statement
| iteration_statement
| jump_statement

labeled_statement
: IDENTIFIER /":" statement
| CASE constant_expression /":" statement
| DEFAULT /":" statement

compound_statement
: "{" (declaration | statement)* "}"

declaration_list: declaration*

expression_statement
: [ expression ] /";"

selection_statement
: IF /"(" expression /")" statement
| IF /"(" expression /")" statement /ELSE statement
| SWITCH /"(" expression /")" statement

iteration_statement
: WHILE /"(" expression /")" statement
| DO statement /WHILE /"(" expression /")" /";"
| FOR /"(" declaration expression_statement [expression] /")" statement

jump_statement
: GOTO IDENTIFIER /";"
| CONTINUE /";"
| BREAK /";"
| RETURN /";"
| RETURN expression /";"

external_declaration
: function_definition
| declaration

function_definition
: [declaration_specifiers] declarator [ declaration_list ] compound_statement
