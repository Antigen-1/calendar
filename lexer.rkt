#lang racket/base
(require brag/support br-parser-tools/lex-sre)
(provide tokenize)

#; (-> (or/c string? symbol?) any/c srcloc? token?)
(define (make-token type value srcloc)
  (token type value
         #:line (srcloc-line srcloc)
         #:column (srcloc-column srcloc)
         #:position (srcloc-position srcloc)
         #:span (srcloc-span srcloc)))

;; Lexer abbreviations
(define-lex-abbrev expr-prefix #\@)
(define-lex-abbrev expr-number (+ (/ #\0 #\9)))
(define-lex-abbrev expr-mode (or "wd" "yd" "y" "m" "w" "d"))
(define-lex-abbrev operator (or #\& #\| #\!))
(define-lex-abbrev end #\;)
(define-lex-abbrev left-paren #\()
(define-lex-abbrev right-paren #\))
(define-lex-abbrev separator #\,)
(define-lex-abbrev escape #\`)
(define-lex-abbrev delimiter (or end operator left-paren right-paren separator escape))
(define-lex-abbrev id (seq (~ expr-prefix delimiter) (* (~ delimiter))))

;; Operator table
(define table (hasheq '& 'AND '\| 'OR '! 'NOT))

#; (-> input-port? token?)
(define get-token
  (lexer
   (operator
    (let ((symbol (string->symbol lexeme)))
      (make-token 'OP (hash-ref table symbol) lexeme-srcloc)))
   ((concatenation expr-prefix expr-number expr-mode)
    (make-token 'EXP (substring lexeme 1) lexeme-srcloc))
   (left-paren (make-token 'LP #f lexeme-srcloc))
   (right-paren (make-token 'RP #f lexeme-srcloc))
   (separator (make-token 'SP #f lexeme-srcloc))
   ((from/to escape escape)
    (make-token 'MOP (string->symbol (trim-ends "`" lexeme "`")) lexeme-srcloc))
   (id
    (make-token 'ID (string->symbol lexeme) lexeme-srcloc))
   (end (void))))

#; (-> input-port? (-> token?))
(define (tokenize port) (lambda () (get-token port)))

(module+ test
  (require rackunit)
  (define proc (tokenize (open-input-string "!@4m|ab@cd(),`a`;")))
  (check-equal? (token-struct-type (proc)) 'OP)
  (check-equal? (token-struct-val (proc)) "4m")
  (check-equal? (token-struct-val (proc)) 'OR)
  (check-equal? (token-struct-val (proc)) 'ab@cd)
  (check-equal? (token-struct-type (proc)) 'LP)
  (check-equal? (token-struct-type (proc)) 'RP)
  (check-equal? (token-struct-type (proc)) 'SP)
  (check-equal? (token-struct-val (proc)) 'a)
  (check-true (void? (proc))))
