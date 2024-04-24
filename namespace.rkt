#lang racket/base
(require racket/contract)
(provide (contract-out (namespace-require/full
                        (-> (or/c module-path? resolved-module-path? module-path-index?)
                            namespace?
                            namespace?
                            any))))

;; Map module-path?, resolved-module-path? and module-path-index? to usable module path
#; (-> (or/c module-path? resolved-module-path? module-path-index?)
       (or/c module-path? resolved-module-path?))
(define (->usable-module-path v)
  (cond ((module-path? v) v)
        ((resolved-module-path? v) v)
        (else (module-path-index-resolve v))))

;; Attach and import a module from src
#; (-> (or/c module-path? resolved-module-path? module-path-index?)
       namespace?
       namespace?
       any)
(define (namespace-require/full mod src dest)
  (define usable-mod-path (->usable-module-path mod))
  (namespace-attach-module src usable-mod-path dest)
  (parameterize ((current-namespace dest))
    (namespace-require usable-mod-path)))
