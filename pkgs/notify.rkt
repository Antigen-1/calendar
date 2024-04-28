#lang racket/base
(require racket/exn racket/draw racket/dict racket/date racket/class
         racket/runtime-path
         libnotify
         (for-syntax racket/base)
         "../pkg.rkt")
(provide install)

;; Server
;;------------------------------------------
;; Runtime Resources
(define-runtime-path svg (build-path 'up "resource" "racket-logo.jpeg"))

;; Create a notifier with an icon
#; (->* ()
        (#:icon (or/c #f path-string? (is-a?/c bitmap%)))
        (->* (#:summary string?)
             (#:body (or/c string? #f)
              #:timeout (or/c #f (>=/c 0))
              #:urgency (or/c 'low 'normal 'critical)
              #:category (or/c string? #f))
             any))
(define ((make-notifier #:icon (icon #f))
         #:summary summary
         #:body (body #f)
         #:timeout (timeout #f)
         #:urgency (urgency 'normal)
         #:category (category #f)
         )
  (send
   (make-object notification%
                summary
                body
                icon
                timeout
                urgency
                category)
   show))

;; Apply the notifier to the record and date
#; (-> <Notifier> record/c date? any)
(define (maybe-apply-record notify record date)
  (if ((car record) date)
      (keyword-apply/dict notify (map cons '(#:summary #:body #:timeout #:urgency #:category) (cdr record)) null)
      (void)))

;; Constants
(define interval (* 24 60 60 1000))
(define icon (read-bitmap svg))

;; Notifiers
#; (->* (#:summary string?)
        (#:body (or/c string? #f)
         #:timeout (or/c #f (>=/c 0))
         #:urgency (or/c 'low 'normal 'critical)
         #:category (or/c string? #f))
        any)
(define notifier (make-notifier #:icon icon))

;; Loggers
(define exception-logger (make-logger #f (current-logger)))

;; Auxiliary functions
#; (-> (-> any) any)
(define (call/excetion-logger proc)
  (with-handlers ((exn:fail? (lambda (e) (log-message exception-logger 'error 'Exception (exn->string e)))))
    (proc)))

;; Call the notifier with all records whose date filter returns #t
#; (-> (listof record/c) date? any)
(define (notify/once records (dt (current-date)))
  (map (lambda (rc) (call/excetion-logger (lambda () (maybe-apply-record notifier rc dt))))
       records))
#; (-> (listof record/c) any)
(define (notify/loop records)
  (if (null? records)
      (void)
      (let loop ((ms (current-milliseconds))
                 (dt (current-date)))
        (notify/once records dt)
        (sync (handle-evt (alarm-evt (+ ms interval))
                          (lambda (_)
                            (loop (current-milliseconds) (current-date))))))))

;; Package installer
#; (-> any)
(define (install)
  (put "notify-once" notify/once)
  (put "notify-loop" notify/loop))
;;------------------------------------------
