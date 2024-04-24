#lang racket/base
(require racket/runtime-path (for-syntax racket/base))

(define-runtime-module-path-index lang "lang.rkt")
(define-runtime-module-path-index server '(submod "lang.rkt" server))

(module namespace racket/base
  (require "lang.rkt")
  (provide anchor)
  (define-namespace-anchor anchor))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline racket/contract
           raco/command-name
           (submod ".." namespace)
           "namespace.rkt")

  (define where (box null))
  (command-line
    #:program (short-program+command-name)
    #:multi
    [("-p" "--path") path "Specify the script" (set-box! where (cons path (unbox where)))]
    #:args ()
    (define/contract paths (listof path-string?) (reverse (unbox where)))
    (with-handlers ((exn:break? void))
      (define ns (make-base-empty-namespace))
      (namespace-require/full lang (namespace-anchor->empty-namespace anchor) ns)
      (parameterize ((current-namespace ns))
        (map load paths)
        (sync (handle-evt ((dynamic-require server 'make-server-thread)) void))))))
