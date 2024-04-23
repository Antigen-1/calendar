#lang racket/base
(require racket/runtime-path)

(define-runtime-module-path-index lang "lang.rkt")

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
           (submod ".." namespace))

  (define where (box #f))
  (command-line
    #:program (short-program+command-name)
    #:once-each
    [("-p" "--path") path "Specify the script" (set-box! where path)]
    #:args ()
    (define/contract path string? (unbox where))
    (parameterize ((current-namespace (module->namespace lang (namespace-anchor->empty-namespace anchor))))
      (with-handlers ((exn:break? void))
        (load path)))))
