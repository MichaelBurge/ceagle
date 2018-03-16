#lang brag

c: top_level_declaration*

@top_level_declaration: declaration | type_declaration | function_declaration

declaration:
  type (declaration_variable COMMA)* declaration_variable /SEMI
| union /SEMI

declaration_variable: variable [/"=" expression]

@type_declaration: typedef

function_declaration: function_modifiers type identifier /LPAREN [function_arguments] /RPAREN /LCURLY sequence /RCURLY

function_modifiers: (extern | static)*
extern: /EXTERN STRING
static: /STATIC

/function_arguments: (function_argument /COMMA)* function_argument

function_argument: type variable

@binop_op:
  "."
| "->"
| "<<="
| ">>="
| "+="
| "-="
| "*="
| "/="
| "%="
| "||="
| "|="
| "^="
| "&&="
| "&="
| "<<"
| ">>"
| "+"
| "-"
| "*"
| "/"
| "%"
| "||"
| "|"
| "^"
| "&&"
| "&"
| "!="
| "<="
| "<"
| ">="
| ">"
| "=="
| "="

@unop_op:
  "++"
| "--"
| "~"
| "-"
| "!"
| "*"

@postop_op: "++" | "--"

@simple_expression:
  function_call
| integer
| char
| variable
| /LPAREN expression /RPAREN

@expression:
  simple_expression
| ternary
| unop
| binop
| postop
| c_cast

integer: INTEGER
char: ONECHAR
variable: [variable_modifier] identifier
variable_modifier: "*"
ternary: expression /"?" expression /COLON expression
binop: expression binop_op expression
unop: unop_op expression
postop: expression postop_op
function_call: simple_expression /LPAREN [(expression /COMMA)* expression] /RPAREN

c_cast: /LPAREN type /RPAREN expression

block: /LCURLY statement* /RCURLY
sequence: statement*

@statement:
  label_definition
| expression_statement
| switch
| if
| for
| while
| do_while
| goto
| block
| return
| break
| continue
| declaration
| empty

expression_statement: expression /SEMI
label_definition: identifier /COLON statement
goto: /GOTO identifier /SEMI
return: /RETURN expression /SEMI
break: /BREAK /SEMI
continue: /CONTINUE /SEMI
empty: /SEMI

switch: /SWITCH /LPAREN expression /RPAREN /LCURLY (switch_case | switch_default)* /RCURLY
switch_case: /CASE expression /COLON sequence
switch_default: /DEFAULT /COLON sequence
if: /IF /LPAREN expression /RPAREN statement [/ELSE statement]
for: /FOR /LPAREN (declaration | empty) ((expression /SEMI)| empty) [expression] /RPAREN statement
while: /WHILE /LPAREN expression /RPAREN statement
do_while: /DO statement /WHILE /LPAREN expression /RPAREN /SEMI

struct: /STRUCT identifier /LCURLY declaration* /RCURLY
union: /UNION [ identifier ] /LCURLY declaration* /RCURLY [ identifier* ]

@type:
  unsigned_char
| unsigned_int
| signed_char
| signed_int
| void
| struct
| union
| identifier
| /CONST type

@identifier: IDENTIFIER
unsigned_char: /UNSIGNED /CHAR | /CHAR
unsigned_int: /UNSIGNED /INT
signed_char: /SIGNED /CHAR
signed_int: /SIGNED /INT | /INT
void: /VOID

typedef: /TYPEDEF type identifier /SEMI
