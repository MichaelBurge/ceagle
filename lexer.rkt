#lang racket

(require br-parser-tools/lex)
(require binaryio)
(require file/sha1)

(require brag/support)

(provide make-tokenizer
         tokenize-all
         print-lexer-hack-state)

(define-lex-abbrev identifier
  (:: (:or alphabetic "_")
     (:* (:or alphabetic numeric "_"))))

(define-lex-abbrev hex (:or numeric "a" "b" "c" "d" "e" "f" "A" "B" "C" "D" "E" "F"))

(define (operator x) (token x x))
(define (keyword x) (token x x))

(define *types* (mutable-set))
(define *gathering-type?* #f)
(define *last-identifier* #f)
(define *brace-level* 0)

(define (print-lexer-hack-state)
  (println `([ TYPES . ,*types* ]
             [ GATHERING-TYPE? . ,*gathering-type?*]
             [ LAST-IDENTIFIER . ,*last-identifier*])))

(define (begin-gathering-type)
  ;(println `(START))
  (set! *gathering-type?* #t))

(define (maybe-gather id)
  ;(println `(DEBUG ,id))
  (when (and *gathering-type?* (= *brace-level* 0))
    (set! *last-identifier* id)))

(define (stop-gathering)
  ;(println `(COMMIT ,*gathering-type?* ,*last-identifier* ,*brace-level*))
  (when (and *gathering-type?* *last-identifier* (= *brace-level* 0))
    (set-add! *types* *last-identifier*)
    (set! *gathering-type?* #f)
    (set! *last-identifier* #f)
    )
  )

(define (lbrace)
  (set! *brace-level* (+ *brace-level* 1)))

(define (rbrace)
  (set! *brace-level* (- *brace-level* 1)))

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
   [(:: "'" (char-complement "'") "'") (token 'INTEGER (char->integer (string-ref lexeme 1)))]
   ["'\\''"                  (token 'ONECHAR (char->integer #\'))]
   [(:: "\""                 (:* (char-complement "\"")) "\"") (token 'STRING_LITERAL lexeme)]
   ; Punctuation
   ["{"   (begin (lbrace) lexeme )]
   ["}"   (begin (rbrace) lexeme )]
   ["("   lexeme ]
   [")"   lexeme ]
   ["["   lexeme ]
   ["]"   lexeme ]
   [";"   (begin (stop-gathering) lexeme )]
   [","   lexeme ]
   [":"   lexeme ]
   ["'"   lexeme ]
   ["\""  lexeme ]
   ["..." lexeme ]
   ; Operators
   [(:or "?" "<<=" ">>=" "+=" "-=" "*=" "/=" "%=" "||="
         "|=" "^=" "&&=" "&=" "~=" "++" "--" "<<" ">>" "+"
         "-"  "*"  "/"   "%"  "||" "|"  "^"  "&&" "&"  "~"
         "!=" "!"  "<="  "<"  ">=" ">"  "==" "=" "->" ".") lexeme]

   ; Keywords
   ["typedef"  (begin (begin-gathering-type) (keyword 'TYPEDEF ))]
   ["struct"   (keyword 'STRUCT  )]
   ["int"      (keyword 'INT     )]
   ["void"     (keyword 'VOID    )]
   ["char"     (keyword 'CHAR    )]
   ["union"    (keyword 'UNION   )]
   ["unsigned" (keyword 'UNSIGNED)]
   ["const"    (keyword 'CONST   )]
   ["static"   (keyword 'STATIC  )]
   ["return"   (keyword 'RETURN  )]
   ["if"       (keyword 'IF      )]
   ["else"     (keyword 'ELSE    )]
   ["continue" (keyword 'CONTINUE)]
   ["break"    (keyword 'BREAK   )]
   ["switch"   (keyword 'SWITCH  )]
   ["case"     (keyword 'CASE    )]
   ["default"  (keyword 'DEFAULT )]
   ["goto"     (keyword 'GOTO    )]
   ["extern"   (keyword 'EXTERN  )]
   ["while"    (keyword 'WHILE   )]
   ["do"       (keyword 'DO      )]
   ["for"      (keyword 'FOR     )]
   ["static"   (keyword 'STATIC  )]
   ["volatile" (keyword 'VOLATILE)]
   [identifier (let ([ sym (string->symbol lexeme)])
                 (maybe-gather sym)
                 (if (set-member? *types* sym)
                     (token 'TYPE_NAME sym)
                     (token 'IDENTIFIER sym)))]
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
