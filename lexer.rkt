#lang racket

(require br-parser-tools/lex)
(require binaryio)
(require file/sha1)

(require brag/support)

(provide make-tokenizer
         tokenize-all)

(define-lex-abbrev identifier
  (:: (:or alphabetic "_")
     (:* (:or alphabetic numeric "_"))))

(define-lex-abbrev hex (:or numeric "a" "b" "c" "d" "e" "f" "A" "B" "C" "D" "E" "F"))

(define (operator x) (token x x))

(define c-lexer
  (lexer-src-pos
   [(eof)               eof]
   ; Syntax regions
   [(:+ whitespace)     (token 'WHITESPACE lexeme #:skip? #t)]
   [(from/to "//" "\n") (token 'SCOMMENT   lexeme #:skip? #t)]
   [(from/to "/*" "*/") (token 'MCOMMENT   lexeme #:skip? #t)]
   ; Constants
   [(:: "0x" (:+ hex) "ULL") (token 'INTEGER (string->number (substring lexeme 2 (- (string-length lexeme) 3))))]
   [(:: "0x" (:+ hex))       (token 'INTEGER (string->number (substring lexeme 2) 16))]
   [(:: (:+ numeric)  "ULL") (token 'INTEGER (string->number (substring lexeme 0 (- (string-length lexeme) 3))))]
   [(:+ numeric)             (token 'INTEGER (string->number lexeme))]
   [(:: "'" (char-complement "'") "'") (token 'ONECHAR (substring lexeme 1 2))]
   ["'\\''"                  (token 'ONECHAR "'")]
   [(:: "\""                 (:* (char-complement "\"")) "\"") (token 'STRING lexeme)]
   ; Punctuation
   ["{"  (operator 'LCURLY )]
   ["}"  (operator 'RCURLY )]
   ["("  (operator 'LPAREN )]
   [")"  (operator 'RPAREN )]
   ["["  (operator 'LSQUARE)]
   ["]"  (operator 'RSQUARE)]
   [";"  (operator 'SEMI   )]
   [","  (operator 'COMMA  )]
   [":"  (operator 'COLON  )]
   ["'"  (operator 'SQUOTE )]
   ["\"" (operator 'DQUOTE )]
   ; Operators
   [(:or "?" "<<=" ">>=" "+=" "-=" "*=" "/=" "%=" "||="
         "|=" "^=" "&&=" "&=" "~=" "++" "--" "<<" ">>" "+"
         "-"  "*"  "/"   "%"  "||" "|"  "^"  "&&" "&"  "~"
         "!=" "!"  "<="  "<"  ">=" ">"  "==" "=" "->" ".") lexeme]

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
   ["do"       (token 'DO      )]
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
    (define tok (tokenizer))
    (match tok
      [(? eof-object?) null]
      [(struct position-token ((? eof-object?) start end)) null]
      [_ (cons tok (loop))])
    )
  (loop))
