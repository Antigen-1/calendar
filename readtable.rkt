#lang racket/base
(require "lexer.rkt" "parse.rkt"
         syntax/strip-context)
(provide $-readtable)

;; Readers for the $ DSL
#; (->* () (any/c input-port?) syntax?)
(define ($-read-syntax (name #f) (in (current-input-port)))
  (strip-context (parse name (tokenize in))))
#; (-> () (input-port?) any)
(define ($-read (in (current-input-port)))
  (syntax->datum ($-read-syntax #f in)))

;; Readtables for the $ DSL
(define $-readtable
  (make-readtable (current-readtable)
                  #\$
                  'non-terminating-macro
                  (lambda (trigger input (name #f) (line #f) (column #f) (position #f))
                    (port-count-lines! input)
                    (set-port-next-location! input line column position)
                    ($-read-syntax name input))))

(module+ test
  (require rackunit)
  (check-equal?
   (parameterize ((current-readtable $-readtable))
     (read (open-input-string "$`a`(@4m,|(&(@5m,e),d));")))
   '(_script
     (_filter (_application (_operator a)
                            #f
                            (_filter (_primitive "4m"))
                            #f
                            (_filter (_application (_operator OR)
                                                   #f
                                                   (_filter (_application (_operator AND)
                                                                          #f
                                                                          (_filter (_primitive "5m"))
                                                                          #f
                                                                          (_filter (_primitive e))
                                                                          #f))
                                                   #f
                                                   (_filter (_primitive d))
                                                   #f))
                            #f))))
  (let ((port (open-input-string "abcd$@4m;")))
    (parameterize ((current-readtable $-readtable))
      (read-string 4 port)
      (define stx (read-syntax 'port port))
      (check-true (= 5 (syntax-position stx))))))
