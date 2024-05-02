#lang racket/base
(require (for-syntax racket/base)
         syntax/parse/define
         racket/date
         "date.rkt" "readtable.rkt" "record.rkt")
(provide (all-from-out racket/base racket/date)
         (rename-out (add-record! send!))
         $-readtable
         _script
         _filter
         _application
         _primitive
         _operator)

;; Expander
(begin-for-syntax
  (define-syntax-class operator
    #:description "Operator"
    (pattern (~datum &)
             #:with real #'filter-conjoin)
    (pattern (~datum \|)
             #:with real #'filter-disjoin)
    (pattern (~datum !)
             #:with real #'filter-negate)
    (pattern op:id
             #:with real #'op))
  (define-splicing-syntax-class rest-argument
    (pattern (~seq #f arg)
             #:with argument #'arg))
  (define-splicing-syntax-class arguments
    #:description "Operands"
    (pattern (~seq)
             #:with arguments #'())
    (pattern (~seq arg rest:rest-argument ...)
             #:with arguments #'(arg rest.argument ...))
    ))
(define-syntax (_script stx)
  (syntax-parse stx
    ((_ o)
     #'(compose1 o erase-date))))
(define-syntax (_filter stx)
  (syntax-parse stx
    ((_ o) #'o)))
(define-syntax (_application stx)
  (syntax-parse stx
    ((_ op #f arguments:arguments #f)
     #'(op . arguments.arguments))))
(define-syntax (_primitive stx)
  (syntax-parse stx
    ((_ v:string)
     #'(make-date-filter-predicate v))
    ((_ v:id)
     #'v)))
(define-syntax (_operator stx)
  (syntax-parse stx
    ((_ op:operator)
     #'op.real)))

(module+ test
  (require rackunit)
  (define dt (current-date))
  (define pred (_script
                (_filter (_application (_operator &)
                                       #f
                                       (_primitive "0wd")
                                       #f
                                       (_primitive "1wd")
                                       #f
                                       (_primitive "2wd")
                                       #f
                                       (_primitive "3wd")
                                       #f
                                       (_primitive "4wd")
                                       #f
                                       (_primitive "5wd")
                                       #f
                                       (_primitive "6wd")
                                       #f))))
  (check-false (pred dt)))
