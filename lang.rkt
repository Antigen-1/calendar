#lang racket/base
(require (for-syntax racket/base)
         syntax/parse/define
         racket/date
         "filter.rkt" "readtable.rkt" "record.rkt")
(provide (all-from-out racket/base)
         (rename-out (#%filter _filter)
                     (#%unit _unit)
                     (add-record! send!))
         $-readtable
         )

;; Compile-time operator table
(begin-for-syntax
  ;; Lookup and check
  #; (-> symbol? exact-integer? syntax? any)
  (define (operator-identifier/check-arity op a stx)
    (define table (hasheq 'AND (cons #'filter-conjoin 2)
                          'NOT (cons #'filter-negate 1)
                          'OR (cons #'filter-disjoin 2)))
    (define (raise-not-found)
      (raise-syntax-error #f "Operator not found." stx))
    (define (operator-identifier op)
      (car (hash-ref table op raise-not-found)))
    (define (operator-arity op)
      (cdr (hash-ref table op raise-not-found)))

    (let ((arity (operator-arity op))
          (id (operator-identifier op)))
      (if (= a arity)
          id
          (raise-syntax-error
           #f
           (format "Mismatched arity:\n\tExpected: ~a;\n\tGiven: ~a." arity a)
           stx)))))

;; Syntax classes
(begin-for-syntax
  (define-syntax-class complex-filter
    #:description "Complex filter"
    #:datum-literals (_operator)
    ;; All operators are of the same precedence
    ;; All operators are right-associative
    (pattern (first (_operator op:id) . rest)
             #:with filter #`(#,(operator-identifier/check-arity (syntax->datum #'op) 2 #'op) first (#%filter . rest)))
    (pattern ((_operator op:id) ft ...)
             #:with filter #`(#,(operator-identifier/check-arity (syntax->datum #'op) 1 #'op) (#%filter ft ...)))))

;; Expander for the $ DSL
(define-syntax-parser #%filter
  ((_ . ft:complex-filter)
   #'ft.filter)
  ((_ ft)
   #'ft))
(define-syntax-parser #%unit
  ((_ v:str)
   #'((compose1 date-filter->predicate string->date-filter) v))
  ((_ v:id)
   #'v))

(module+ test
  (require rackunit)
  (define pred1 (#%unit "0wd"))
  (define pred2 (#%unit pred1))
  (define pred (#%filter pred2
                         (_operator OR)
                         (#%unit "1wd")
                         (_operator OR)
                         (#%unit "2wd")
                         (_operator OR)
                         (#%unit "3wd")
                         (_operator OR)
                         (#%unit "4wd")
                         (_operator OR)
                         (#%unit "5wd")
                         (_operator OR)
                         (#%unit "6wd")))
  (check-true (pred (current-date)))
  (check-false ((#%filter pred (_operator AND) (_operator NOT) pred) (current-date))))
