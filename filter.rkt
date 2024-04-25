#lang hasket
(require racket/match racket/string racket/contract "function.rkt")
(provide (contract-out (date-filter->predicate (-> date-filter? (-> date? any)))
                       (string->date-filter (-> string? any))
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

;; Contracts
(define filter-conbinator/c (-> (-> date? boolean?) (-> date? boolean?) any))
(define filter-mapper/c (-> (-> date? boolean?) any))

;; Filter combinators
#; filter-conbinator/c
(define (disjoin p1 p2)
  (disjoin1 p1 p2))
#; filter-conbinator/c
(define (conjoin p1 p2)
  (conjoin1 p1 p2))
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
     ((foldl
       disjoin
       (const1 #f)
       (map (compose1 date-filter->predicate string->date-filter symbol->string)
            (list '0wd '1wd '2wd '3wd '4wd '5wd '6wd)))
      date)))
  (check-false
   (let ((date (current-date)))
     ((foldl
       conjoin
       (const1 #t)
       (map (compose1 date-filter->predicate string->date-filter symbol->string)
            (list '1m '2m '3m '4m '5m '6m '7m '8m '9m '10m '11m '12m)))
      date))))
