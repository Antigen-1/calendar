#lang racket/base
(require racket/class racket/contract racket/draw libnotify)
(provide (contract-out (make-notifier (->* () (#:icon (or/c #f (is-a?/c bitmap%)))
                                           (->* (#:summary string?)
                                                (#:body (or/c string? #f)
                                                 #:timeout (or/c #f (>=/c 0))
                                                 #:urgency (or/c "low" "normal" "critical")
                                                 #:category (or/c string? #f))
                                                any)))))

;; Create a notifier with an icon
;; Read the contract in the contract-out clause
(define ((make-notifier #:icon (icon #f))
         #:summary summary
         #:body (body #f)
         #:timeout (timeout #f)
         #:urgency (urgency "normal")
         #:category (category #f)
         )
  (send
   (make-object notification%
                summary
                body
                icon
                timeout
                (string->symbol urgency)
                category)
   show))
