#lang racket/base
(module reader syntax/module-reader calendar/lang
        #:wrapper1 (lambda (t) (parameterize ((current-readtable $-readtable)) (t)))
        (require "readtable.rkt"))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline racket/contract
           raco/command-name
           (submod "lang.rkt" server)
           )

  (define where (box null))
  (define once? (box #f))
  (command-line
   #:program (short-program+command-name)
   #:once-each
   [("-o" "--once") "Run the script once" (set-box! once? #t)]
   #:multi
   [("-p" "--path") path "Specify the script" (set-box! where (cons path (unbox where)))]
   #:args ()
   (define/contract paths (listof path-string?) (reverse (unbox where)))
   (with-handlers ((exn:break? void))
     (map (lambda (p) (dynamic-require p #f)) paths)
     (sync (handle-evt (make-server-thread (unbox once?)) void)))))
