#lang racket/base
(require racket/contract)
(provide (contract-out (add-record!
                        (->* ((-> date? boolean?) string?)
                             (#:body (or/c string? #f)
                              #:timeout (or/c #f (>=/c 0))
                              #:urgency (or/c "low" "normal" "critical")
                              #:category (or/c string? #f))
                             any))
                       (get-records (-> any)))
         record/c)

;; Records
;;------------------------------------------
;; Contracts
(define record/c (list/c (-> date? boolean?) string? (or/c string? #f) (or/c #f (>=/c 0)) (or/c "low" "normal" "critical") (or/c string? #f)))

;; Record registry
;; The provide form includes their contracts
(define-values
  (add-record! get-records)
  (let ((cell (make-thread-cell null)))
    ;; Avoid concurrency problems
    (values
     (lambda (f s #:body (b #f) #:timeout (t #f) #:urgency (u "normal") #:category (c #f))
       (thread-cell-set! cell (cons (list f s b t u c) (thread-cell-ref cell))))
     (lambda ()
       (reverse (thread-cell-ref cell))))))
;;------------------------------------------
