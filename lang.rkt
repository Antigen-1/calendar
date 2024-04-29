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

;; Syntax classes
(begin-for-syntax
  (define-syntax-class complex-filter
    #:description "Complex filter"
    #:datum-literals (_operator1 _operator2)
    ;; All operators are of the same precedence
    ;; All operators are right-associative
    (pattern (first (_operator2 op) . rest)
             #:with filter #'((#%operator op) first (#%filter . rest)))
    (pattern ((_operator1 op) ft ...)
             #:with filter #'((#%operator op) (#%filter ft ...)))))

;; Expander for the $ dsl
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
(define-syntax-parser #%operator
  ((_ (~datum OR))
   #'filter-disjoin)
  ((_ (~datum AND))
   #'filter-conjoin)
  ((_ (~datum NOT))
   #'filter-negate))

(module+ test
  (require rackunit)
  (define pred1 (#%unit "0wd"))
  (define pred2 (#%unit pred1))
  (define pred (#%filter pred2
                         (_operator2 OR)
                         (#%unit "1wd")
                         (_operator2 OR)
                         (#%unit "2wd")
                         (_operator2 OR)
                         (#%unit "3wd")
                         (_operator2 OR)
                         (#%unit "4wd")
                         (_operator2 OR)
                         (#%unit "5wd")
                         (_operator2 OR)
                         (#%unit "6wd")))
  (check-true (pred (current-date)))
  (check-false ((#%filter pred (_operator2 AND) (_operator1 NOT) pred) (current-date))))
