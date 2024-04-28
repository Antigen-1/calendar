#lang racket/base
(require racket/contract "record.rkt")
(provide (contract-out (put (-> (and/c string? (not/c (lambda (v) (hash-has-key? table v))))
                                (-> (listof record/c) any) any))
                       (get (-> string? any))))

;; Package management
;;------------------------------------------
(define table (make-hash))

(define (put name proc)
  (hash-set! table name proc))
(define (get name) (hash-ref table name))
;;------------------------------------------
