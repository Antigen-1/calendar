#lang racket/base
(require (for-syntax racket/base)
         syntax/parse/define
         racket/runtime-path racket/date racket/draw racket/contract racket/exn racket/async-channel
         "filter.rkt" "notify.rkt")
(provide begin lambda define #%top-interaction #%app
         (rename-out (my-quote quote)
                     (my-datum #%datum))
         disjoin conjoin
         (contract-out (notify
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

;; Loggers
(define exception-logger (make-logger #f (current-logger)))

;; Thread registry
#; (-> thread? any)
#; (-> (listof thread?))
(define-values
  (add-thread! get-threads)
  (let ((ch (make-async-channel)))
    (values
     (lambda (th) (async-channel-put ch th))
     (lambda ()
       (for/list ((th (in-producer (lambda () (sync/timeout 0 ch)) #f)))
         th)))))

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
  ((compose1 add-thread! thread)
   (letrec ((loop
             (lambda ((ms (current-milliseconds)))
               (with-handlers ((exn:fail? (lambda (e)
                                            (log-message
                                             exception-logger
                                             'error
                                             'Exception
                                             (exn->string e)))))
                 (cond ((f (current-date))
                        ((make-notifier #:icon icon)
                         #:summary s
                         #:body b
                         #:timeout t
                         #:urgency u
                         #:category c)))
                 (sync (handle-evt (alarm-evt (+ ms interval))
                                   (lambda (_) (loop (current-milliseconds)))))))))
     loop)))

(module+ server
  (provide server-thread)
  ;; This module must be instantiated after all notifier threads are created and registered

  ;; A thread that listens to all notifier threads
  (define server-thread
    (thread
     (lambda ()
       (let loop ((threads (get-threads)))
         (if (null? threads) (void) (loop (remove (apply sync threads) threads eq?))))))))
