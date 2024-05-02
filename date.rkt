#lang hasket
(require racket/match racket/string racket/contract "function.rkt")
(provide (contract-out (erase-date (-> date? any))
                       (make-date-filter-predicate (-> string? (-> date? any)))
                       (rename disjoin filter-disjoin filter-conbinator/c)
                       (rename conjoin filter-conjoin filter-conbinator/c)
                       (rename negate filter-negate filter-mapper/c))
         date-filter?)

(define-datatype date-filter date-filter?
  (week-day (day (integer-in 0 6)))
  (year-day (day (integer-in 0 365)))
  (year (number exact-integer?))
  (month (number (integer-in 1 12)))
  (day (number (integer-in 1 31)))
  )

;; Erase unecessary fields
#; (-> date? date?)
(define (erase-date dt)
  (struct-copy date dt (second 0) (minute 0) (hour 0) (dst? #f) (time-zone-offset 0)))

;; Map date filter structures to predicates
#; (-> date-filter? (-> date? boolean?))
(define (date-filter->predicate f)
  (lambda/curry/match
   (((date _ _ _ day month year week-day year-day _ _))
    (cases date-filter f
           ((week-day d) (= d week-day))
           ((year-day d) (= d year-day))
           ((year number) (= number year))
           ((month number) (= number month))
           ((day number) (= number day))))))

;; Static information
(define symbols '(wd yd y m d))
(define constructors (list week-day year-day year month day))
(define assocs (map cons symbols constructors))
(define pattern (regexp (format "([0-9]+)(~a)" (string-join (map symbol->string symbols) "|"))))

;; Parser
#; (-> string? date-filter?)
(define (string->date-filter str)
  (match (regexp-match pattern str)
    ((list _ num-str sym-str)
     #:do [(define number (string->number num-str))
           (define symbol (string->symbol sym-str))]
     ((cdr (assq symbol assocs)) number))
    (_ (raise (make-exn:fail:contract
               (format "string->date-filter:: Expected: ~s; Given: ~s."
                       pattern
                       str)
               (current-continuation-marks))))))

;; Composed predicate constructor
#; (-> string? (-> date? boolean?))
(define (make-date-filter-predicate str)
  (date-filter->predicate (string->date-filter str)))

;; Contracts
(define filter-conbinator/c (->* () () #:rest (listof (-> date? boolean?)) any))
(define filter-mapper/c (-> (-> date? boolean?) any))

;; Filter combinators
#; filter-conbinator/c
(define (disjoin . ps)
  (apply disjoin1 ps))
#; filter-conbinator/c
(define (conjoin . ps)
  (apply conjoin1 ps))
#; filter-mapper/c
(define (negate p)
  (negate1 p))

(module+ test
  (require rackunit racket/date)
  (check-true (date-filter? (string->date-filter "6wd")))
  (check-true (date-filter? (string->date-filter "10yd")))
  (check-true (date-filter? (string->date-filter "2024y")))
  (check-true (date-filter? (string->date-filter "10m")))
  (check-true (date-filter? (string->date-filter "10d")))
  (check-exn exn:fail:contract? (lambda () (string->date-filter "w")))
  (check-true
   (let ((date (current-date)))
     ((apply disjoin (map make-date-filter-predicate
                          (list "0wd" "1wd" "2wd" "3wd" "4wd" "5wd" "6wd")))
      date)))
  (check-false
   (let ((date (current-date)))
     ((apply conjoin (map make-date-filter-predicate
                          (list "1m" "2m" "3m" "4m" "5m" "6m" "7m" "8m" "9m" "10m" "11m" "12m")))
      date))))
