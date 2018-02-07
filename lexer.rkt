#lang racket

(require br/parser/tools/lex)

(provide tokenize)

(define-lex-abbrev identifier
  (: alphabetic
     (:* (:or alphabetic numeric))))

(define (c-lexer next)
  (lexer-src-pos
   [(eof)      eof       ]
   ; Syntax regions
   [whitespace          (token 'WHITESPACE lexeme #:skip? #t)]
   [(from/to "//" "\n") (token 'SCOMMENT   lexeme #:skip? #t)]
   [(from/to "/*" "*/") (token 'MCOMMENT   lexeme #:skip? #t)]
   ; Constants
   [(: "0x" (+ numeric)) (token 'INTEGER (bytes->integer (hex-string->bytes lexeme) #f))]
   [(+ numeric)          (token 'INTEGER (string->integer lexeme))]
   ; Punctuation
   ["{"        (token 'LCURLY    )]
   ["}"        (token 'RCURLY    )]
   ["("        (token 'LPAREN    )]
   [")"        (token 'RPAREN    )]
   ["["        (token 'LSQUARE   )]
   ["]"        (token 'RSQUARE   )]
   [";"        (token 'SEMI      )]
   [","        (token 'COMMA     )]
   ["."        (token 'DOT       )]
   [":"        (token 'COLON     )]
   ["'"        (token 'SQUOTE    )]
   ["\""       (token 'DQUOTE    )]
   ; Operators
   ["<<="      (token 'LSH-ASSIGN )]
   [">>="      (token 'RSH-ASSIGN )]
   ["+="       (token 'ADD-ASSIGN )]
   ["-="       (token 'SUB-ASSIGN )]
   ["*="       (token 'MUL-ASSIGN )]
   ["/="       (token 'DIV-ASSIGN )]
   ["%="       (token 'MOD-ASSIGN )]
   ["||="      (token 'LOR-ASSIGN )]
   ["|="       (token 'BOR-ASSIGN )]
   ["^="       (token 'XOR-ASSIGN )]
   ["&&="      (token 'LAND-ASSIGN)]
   ["&="       (token 'BAND-ASSIGN)]
   ["~="       (token 'NEG-ASSIGN )]
   ["++"       (token 'INC        )]
   ["--"       (token 'DEC        )]
   
   ["<<"       (token 'LSH       )]
   [">>"       (token 'RSH       )]
   ["+"        (token 'ADD       )]
   ["-"        (token 'SUB       )]
   ["*"        (token 'MUL       )]
   ["/"        (token 'DIV       )]
   ["%"        (token 'MOD       )]
   ["||"       (token 'LOR       )]
   ["|"        (token 'BOR       )]
   ["^"        (token 'XOR       )]
   ["&&"       (token 'LAND      )]
   ["&"        (token 'BAND      )]
   ["~"        (token 'NEG       )]

   ["!="       (token 'NEQ       )]
   ["!"        (token 'NOT       )]
   ["<="       (token 'LE        )]
   ["<"        (token 'LT        )]
   [">="       (token 'GE        )]
   [">"        (token 'GT        )]
   ["=="       (token 'EQ        )]
   ["="        (token 'ASSIGN    )]
   
   ; Keywords
   ["typedef"  (token 'TYPEDEF )]
   ["struct"   (token 'STRUCT  )]
   ["int"      (token 'INT     )]
   ["void"     (token 'VOID    )]
   ["char"     (token 'CHAR    )]
   ["union"    (token 'UNION   )]
   ["unsigned" (token 'UNSIGNED)]
   ["const"    (token 'CONST   )]
   ["static"   (token 'STATIC  )]
   ["return"   (token 'RETURN  )]
   ["if"       (token 'IF      )]
   ["else"     (token 'ELSE    )]
   ["continue" (token 'CONTINUE)]
   ["break"    (token 'BREAK   )]
   ["switch"   (token 'SWITCH  )]
   ["case"     (token 'CASE    )]
   ["default"  (token 'DEFAULT )]
   ["goto"     (token 'GOTO    )]
   ["extern"   (token 'EXTERN  )]
   ["while"    (token 'WHILE   )]
   ["for"      (token 'FOR     )]
   ["static"   (token 'STATIC  )]
   [identifier (token 'IDENTIFIER lexeme)]
   ))
   
(define (make-tokenizer ip)
  (port-count-lines! ip)
  (define (next-token)
    (c-lexer ip))
  next-token)

(define (tokenize-all ip)
  (define tokenizer (make-tokenizer ip))
  (define (loop)
    (match (tokenizer)
      [#f null]
      [x  (cons x (loop))]))
  (loop))
