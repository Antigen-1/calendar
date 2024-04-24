#lang racket/base
(require (for-syntax racket/base)
         syntax/parse/define
         racket/runtime-path racket/date racket/draw racket/contract racket/dict
         "filter.rkt" "notify.rkt")
(provide begin lambda define #%top-interaction #%app
         (rename-out (my-quote quote)
                     (my-datum #%datum))
         disjoin conjoin
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
  (let ((bx (box null)))
    (values
     (lambda (rc)
       (set-box! bx (cons rc (unbox bx))))
     (lambda ()
       (reverse (unbox bx))))))
;;------------------------------------------

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
(define-syntax-parse-rule (my-datum . v)
  (my-quote v))

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
  (provide make-server-thread)

  ;; Loggers
  (define exception-logger (make-logger #f (current-logger)))

  ;; Auxiliary functions
  #; (-> (-> any) any)
  (define (call/excetion-logger proc)
    (with-handlers ((exn:fail? (lambda (e) (log-message exception-logger 'error 'Exception (exn->string e)))))
      (proc)))

  ;; This procedure must be called after notifiers are created and registered
  ;; Create a thread that listens to all notifier threads
  (define (make-server-thread)
    (thread
     (lambda ()
       (define records (get-records))
       (if (null? records)
           (void)
           (let loop ((ms (current-milliseconds))
                      (dt (current-date)))
             (map (lambda (rc) (call/excetion-logger (lambda () (maybe-apply-record notifier rc dt))))
                  records)
             (sync (handle-evt (alarm-evt (+ ms interval))
                               (lambda (_)
                                 (loop (current-milliseconds) (current-date)))))))))))
