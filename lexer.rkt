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
(define-lex-abbrev operator2 (or #\& #\|))
(define-lex-abbrev operator1 #\!)
(define-lex-abbrev end #\;)
(define-lex-abbrev delimiter (or operator2 end operator1))
(define-lex-abbrev id (seq (~ expr-prefix delimiter) (* (~ delimiter))))

#; (-> input-port? token?)
(define get-token
  (lexer
   (operator2
    (let ((symbol (string->symbol lexeme)))
      (make-token 'OP2 (if (eq? symbol '&) 'AND 'OR) lexeme-srcloc)))
   ((concatenation expr-prefix expr-number expr-mode)
    (make-token 'EXP (substring lexeme 1) lexeme-srcloc))
   (operator1 (make-token 'OP1 'NOT lexeme-srcloc))
   (id
    (make-token 'ID (string->symbol lexeme) lexeme-srcloc))
   (end (void))))

#; (-> input-port? (-> token?))
(define (tokenize port) (lambda () (get-token port)))

(module+ test
  (require rackunit)
  (define proc (tokenize (open-input-string "!@4m|ab@cd;")))
  (check-equal? (token-struct-type (proc)) 'OP1)
  (check-equal? (token-struct-val (proc)) "4m")
  (check-equal? (token-struct-val (proc)) 'OR)
  (check-equal? (token-struct-val (proc)) 'ab@cd)
  (check-true (void? (proc))))
