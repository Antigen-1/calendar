#lang racket/base
(provide const1 disjoin1 conjoin1)

;; Simplified const function
#; (-> any/c (-> any/c any/c))
(define ((const1 v) _)
  v)
;; Simplified disjoin function
#; (->* () () #:rest (listof predicate/c) predicate/c)
(define (disjoin1 . ps)
  (if (null? ps)
      (const1 #f)
      (let ((first (car ps))
            (rest (cdr ps)))
        (if (null? rest)
            first
            (lambda (v) (or (first v) ((apply disjoin1 rest) v)))))))
;; Simplified conjoin function
#; (->* () () #:rest (listof predicate/c) predicate/c)
(define (conjoin1 . ps)
  (if (null? ps)
      (const1 #t)
      (let ((first (car ps))
            (rest (cdr ps)))
        (if (null? rest)
            first
            (lambda (v) (and (first v) ((apply disjoin1 rest) v)))))))
