#lang racket/base
(require (for-syntax racket/base)
         syntax/parse/define
         racket/runtime-path racket/date racket/draw racket/contract racket/dict
         "filter.rkt" "notify.rkt" "readtable.rkt")
(provide (all-from-out racket/base)
         (rename-out (#%filter _filter)
                     (#%operator _operator)
                     (#%unit _unit))
         $-readtable
         (contract-out (send!
                        (->* ((-> date? boolean?) string?)
                             (#:body (or/c string? #f)
                              #:timeout (or/c #f (>=/c 0))
                              #:urgency (or/c "low" "normal" "critical")
                              #:category (or/c string? #f))
                             any))))

;; Runtime Resources
(define-runtime-path svg "racket-logo.jpeg")

;; Constants
(define interval (* 24 60 60 1000))
(define icon (read-bitmap svg))

;; Records
#; (list/c (-> date? boolean?) string? (or/c string? #f) (or/c #f (>=/c 0)) (or/c "low" "normal" "critical") (or/c string? #f))
;;------------------------------------------
;; Apply the notifier to the record and date
#; (-> <Notifier> <Record> date? any)
(define (maybe-apply-record notify record date)
  (if ((car record) date)
      (keyword-apply/dict notify (map cons '(#:summary #:body #:timeout #:urgency #:category) (cdr record)) null)
      (void)))

;; Record registry
#; (-> <Record> any)
#; (-> (listof <Record>))
(define-values
  (add-record! get-records)
  (let ((cell (make-thread-cell null)))
    ;; Avoid concurrency problems
    (values
     (lambda (rc)
       (thread-cell-set! cell (cons rc (thread-cell-ref cell))))
     (lambda ()
       (reverse (thread-cell-ref cell))))))
;;------------------------------------------

;; Syntax classes
(begin-for-syntax
  (define-syntax-class literal
    #:description "Real literal"
    (pattern v:str)
    (pattern v:boolean)
    (pattern v:number))
  (define-syntax-class complex-filter
    #:description "Complex filter"
    (pattern (first op . rest))))

;; Expander for the $ dsl
(define-syntax-parser #%filter
  ((_ . ft:complex-filter)
   #'(ft.op ft.first (#%filter . ft.rest)))
  ((_ ft)
   #'ft))
(define-syntax-parser #%unit
  ((_ v:str)
   #'((compose1 date-filter->predicate string->date-filter) v))
  ((_ v:id)
   #'v))
(define-syntax-parser #%operator
  ((_ (~datum OR))
   #'disjoin)
  ((_ (~datum AND))
   #'conjoin))

;; Notifiers
#; (->* (#:summary string?)
        (#:body (or/c string? #f)
         #:timeout (or/c #f (>=/c 0))
         #:urgency (or/c "low" "normal" "critical")
         #:category (or/c string? #f))
        any)
(define notifier (make-notifier #:icon icon))

;; Client
#; (->* ((-> date? boolean?) string?)
        (#:body (or/c string? #f)
         #:timeout (or/c #f (>=/c 0))
         #:urgency (or/c "low" "normal" "critical")
         #:category (or/c string? #f))
        any)
(define (send! f
               s
               #:body (b #f)
               #:timeout (t #f)
               #:urgency (u "normal")
               #:category (c #f))
  (add-record! (list f s b t u c)))

(module+ server
  (require racket/exn)
  (provide make-server)

  ;; Loggers
  (define exception-logger (make-logger #f (current-logger)))

  ;; Auxiliary functions
  #; (-> (-> any) any)
  (define (call/excetion-logger proc)
    (with-handlers ((exn:fail? (lambda (e) (log-message exception-logger 'error 'Exception (exn->string e)))))
      (proc)))

  ;; This procedure must be called in the current thread after records are created and registered
  ;; Call the notifier with all records
  (define (make-server once?)
    (define records (get-records))
    (if (null? records)
        (void)
        (let loop ((ms (current-milliseconds))
                   (dt (current-date)))
          (map (lambda (rc) (call/excetion-logger (lambda () (maybe-apply-record notifier rc dt))))
               records)
          (sync (handle-evt (if once? never-evt (alarm-evt (+ ms interval)))
                            (lambda (_)
                              (loop (current-milliseconds) (current-date))))
                (handle-evt (if once? always-evt never-evt)
                            void))))))
