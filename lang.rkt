#lang racket/base
(require (for-syntax racket/base)
         syntax/parse/define
         racket/runtime-path racket/date racket/draw racket/class racket/contract
         "filter.rkt" "notify.rkt")
(provide begin lambda define #%module-begin #%app
         (rename-out (my-quote quote))
         disjoin conjoin
         (contract-out (notify
                        (->* ((-> date? boolean?) string?)
                             (#:body (or/c string? #f)
                              #:timeout (or/c #f (>=/c 0))
                              #:urgency (or/c "low" "normal" "critical")
                              #:category (or/c string? #f))
                             any))))

;; Runtime Resources
(define-runtime-path svg "racket-logo.svg")

;; Constants
(define interval (* 24 60 60 1000))
(define icon (make-object bitmap% svg))

;; Syntax classes
(begin-for-syntax
  (define-syntax-class literal
    #:description "Real literal"
    (pattern v:str)
    (pattern v:boolean)
    (pattern v:number)))

;; New interposition points
(define-syntax-parser my-quote
  ((_ v:id)
   #'(date-filter->predicate (symbol->date-filter 'v)))
  ((_ v:literal)
   #'(quote v)))

;; Notifier
#; (->* ((-> date? boolean?) string?)
        (#:body (or/c string? #f)
         #:timeout (or/c #f (>=/c 0))
         #:urgency (or/c "low" "normal" "critical")
         #:category (or/c string? #f))
        any)
(define (notify f
                s
                #:body (b #f)
                #:timeout (t #f)
                #:urgency (u "normal")
                #:category (c #f))
  (let loop ((ms (current-milliseconds)))
    (cond ((f (current-date))
           ((make-notifier #:icon icon)
            #:summary s
            #:body b
            #:timeout t
            #:urgency u
            #:category c)))
    (sync (handle-evt (alarm-evt (+ ms interval))
                      (lambda (_) (loop (current-milliseconds)))))))
