
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                 ;;
;; BREAM / toplevel.scm                                            ;;
;;   --  The toplevel module, for doing all the phases of the      ;;
;;       compilation, i.e.                                         ;;
;;         Reading the source module in + (all the library funs,   ;;
;;         syntax rewriting (cond, let*, etc.),                    ;;
;;         type resolving,                                         ;;
;;         compilation to level0-code,                             ;;
;;         and Verilog-output to files.                            ;;
;;                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Unless otherwise mentioned, all the files in this code tree are
;; Copyright (C) 2010-2011 Antti Karttunen, subject to the terms of the GPL v2.
;; See the file COPYING for more information.  Contact the author for
;; further developments at the address: <his-firstname>.<his-surname>@gmail.com
;;

;;
;; Antti Karttunen ("karttu") started writing this module May 20 2011,
;; by transferring some stuff from compile1.scm
;;

;;
;; Edited May 25-27 2011 by karttu.
;;   The first testable implementation of type-resolver.
;;   (See run-typeresolve-testset)
;;
;; Edited    Jul 27 2011 by karttu.
;;    Added toc argument to call to expsynta.
;;
;; Edited    Aug 17 2011 by karttu.
;;    Now compile-topmodule expects that the name of toplevel-file
;;    should end with extension ".brm.scm", not just ".scm".
;;
;; Edited    Aug 20 2011 by karttu.
;;    Added toc-create-toplevel-define-wrapper for creating a special
;;    function definition wrapper around a toplevel call, which takes
;;    care of adding the additional required I/O-signals,
;;    expanding the syntax and type resolving, before the wrapped
;;    call is attached to the front of the same toc-to-be-compiled-items list
;;    as all the other functions that are encountered in compiling process.
;;
;; Edited    Aug 21 2011 by karttu.
;;   Removed the definition of
;;   *XXX-BREAM-DEFAULT-WIDTH-UNTIL-RESOLVING-FULLY-IMPLEMENTED*
;;   (see compile1.scm for the reason).
;;
;; Edited    Aug 25 2011 by karttu.
;;   Added to toc-create-toplevel-define-wrapper definitions for
;;   input-signalx8 switch and a call to wait_until_start_debounced
;;   The latter immediately commented out, as it doesn't produce
;;   Verilog-code which would start.
;;
;; Edited    Aug 26 2011 by karttu.
;;   Added max-passes argument to compile-topmodule, our main entry point.
;;

(cd "/home/hacklab/FPGA/")

(load "bream/src/utilits1.scm") ;; Some common utility functions.

(load "bream/src/combfscm.scm") ;; Combinational bream functions for Scheme.

(load "bream/src/typesch1.scm") ;; The Typing Scheme #1.

(load "bream/src/srcstrct.scm") ;; For accessing type-annotated S-expr trees.

(load "bream/src/funinfos.scm") ;; Funinfo-structure.

(load "bream/src/dispatch.scm") ;; Must come before lev0veri.scm

(load "bream/src/expsynta.scm") ;; For cond, or, etc.

(load "bream/src/expwirms.scm") ;; For expanding wirm-macros.

(load "bream/src/compile1.scm") ;; The compilation module itself.

(load "bream/src/typreslv.scm") ;; Type (i.e. width) resolver.

(load "bream/src/combopti.scm") ;; For optimizing level-0 combinational exprs.

(load "bream/src/readdefs.scm") ;; Functions for reading in Bream function
;;                                 definitions


(load "bream/src/lev0veri.scm") ;; The Level0->Verilog backend.




(define-structure (ewp (keyword-constructor) (copier))

    error-printer

    warning-printer
)


(define-structure (toc (keyword-constructor) (copier))

    outdir-name ;; A path to an output directory.

    error-printer

    warning-printer

    logging-printer

    logging-printer2

    compiled-forms

    rewritten-forms

    typeresolving-forms

    typeresolving-passes-made

    typeresolving-max-passes

    typeresolving-max-width

    calling-convention-in-use

    resolving-convention-in-use

    rewriting-convention-in-use

    wirm-rewriting-convention-in-use

    rewriting-changes-made

    defined-functions

    already-compiled-items ;; A list of fullnames (with type-signature).

    to-be-compiled-items ;; An assoc list of (fullname . fundef) pairs.

    logging-port

    logging-level

    toplevel-exit ;; An exit continuation we can call after fatal errors.

    coc-dbg-show-wiredefs?
)


(define (make-fresh-toc modulename max-passes loglevel exit-to-top fundefines)
              (make-toc 'outdir-name modulename
                        'warning-printer warn
                        'error-printer error


;; Use as: ((toc-logging-printer trc) toc where 1 (format #t "...") extras)

                        'logging-printer
                       (lambda (where toc on-level-n . rest)
                               (if (>= (toc-logging-level toc) on-level-n)
                                   (begin
                                      (format (toc-logging-port toc) "~a: "
                                                    where
                                      )
                                      (apply (toc-warning-printer toc) rest)
                                   )
                               )
                       )

                        'logging-printer2
                       (lambda (where toc on-level-n str extra-tep)
                               (if (>= (toc-logging-level toc) on-level-n)
                                   (begin
                                      (format (toc-logging-port toc) "~a: ~a"
                                                    where str
                                      )
                                      (texp-print-with-nl extra-tep
                                                        (toc-logging-port toc)
                                      )
                                   )
                               )
                       )

                        'logging-port (current-output-port)
                        'logging-level loglevel

                        'toplevel-exit exit-to-top

                        'compiled-forms *compiled-forms*
                        'rewritten-forms  *rewriting-forms*

                        'typeresolving-forms *typeresolving-forms*

                        'typeresolving-passes-made 0
                        'typeresolving-max-width 64
                        'typeresolving-max-passes max-passes

                        'calling-convention-in-use *BREAM-CALLING-CONVENTION*
                        'resolving-convention-in-use *BREAM-CALLING-CONVENTION*

                        'rewriting-convention-in-use
                                            *BREAM-REWRITING-CONVENTION*
                        'wirm-rewriting-convention-in-use
                                            *BREAM-WIRM-REWRITING-CONVENTION*

                        'rewriting-changes-made 0

                        'defined-functions fundefines
                        'already-compiled-items (list (list))
                        'to-be-compiled-items (list (list (list)))

                        'coc-dbg-show-wiredefs? #t ;; XXX - For now.
              )
)



(define (toc-fullname->out-module-name toc fullname)
   (format #f "~a/~a.v" (toc-outdir-name toc) fullname)
)


;; Should be in the beginning of every file containing top-level invocations:
;; (Our top-level innovation?)

(define *BREAM-CALLING-CONVENTION* '(regular 0.1 draft))
(define *BREAM-REWRITING-CONVENTION* '(regular 0.1 draft))
(define *BREAM-WIRM-REWRITING-CONVENTION* '(wirm 0.1 draft))

(define *BREAM-LIB-MODULES*
   (keep-matching-items
      (directory-read (make-pathname #f #f
;;                        '(absolute "home" "karttu" "bream" "src" "breamlib")
                          '(relative "bream" "src" "breamlib")
                           #f #f #f
                      )
      )
      (lambda (pn) (string-suffix-ci? ".BRM.SCM" (->namestring pn)))
   )
)


(define (toc-add-fullname.fundef-to-be-compiled-list! toc fullname fundef fis)
  (let ((still-uncompiled (toc-to-be-compiled-items toc)))
    (cond ((and (not (memq fullname
                           (toc-already-compiled-items toc)))
                (not (assoc fullname still-uncompiled))
           )
             (attach! (list fullname fundef fis) still-uncompiled)
          )
    )
  )
)


(define (toc-create-toplevel-define-wrapper toc top-call-fis)
  (let* ((name_et_fargs
           (untyped (list (untyped '<toplevel-call>)
                          (type-et-elem (type-i-of-definite-width 8) 'switch)
                          (type-et-elem type-1bit-output 'txd)
                    )
           )
         )
         (u-fargs (t-rest name_et_fargs))
         (newbody
;; This hack doesn't work now, so commented. Just use the given top-call-fis
;;                (untyped ;; XXX -- Should we inherit type from top-call-fis?
;;                 (list (untyped 'seq2)
;;                       (type-et-elem
;;                              type-1bit-input
;;                              (list (untyped 'wait_until_start_debounced))
;;                       )
                         (funinfo-sexpanded-source toc top-call-fis)
;;                 )
;;                )
         )
        )

     (if (not (typeresolve-toplevel-call! toc newbody
                  (make-list-of-toplevel-initial-type-bindings u-fargs)
              )
         )
         (begin
              ((toc-warning-printer toc)
 "Could not complete the type-annotation for toplevel call or one of the included functions: "
              )
              (texp-print-with-nl src (toc-logging-port toc))
              ((toc-toplevel-exit toc) 1)
         )
     )

     (untyped
          (list (untyped `<define-toplevel-call>)
                name_et_fargs
                newbody
          )
     )
  )
)


;; the toplevel-file-name should the name (without .brm.scm extension)
;; of the file, where the toplevel-call is located, together
;; with some additional calling convention definitions, etc.
;; The file should be situated in directory of the same name,
;; i.e. <modulename>/<modulename>.brm.scm

(define (compile-topmodule modulename loglevel max-passes)
  (call-with-current-continuation
    (lambda (exit-to-top)
       (let* ((module-file-name (format #f "~a/~a.brm.scm" modulename modulename))
              (ewp (make-ewp 'warning-printer warn 'error-printer error))
              (defs-and-calls (read-source-file! module-file-name
                                                 ewp
                                                 (list)
                                                 (list (list))
                              )
              )
              (toplevel-defs (car defs-and-calls))
              (toplevel-calls (remove null? (cdr defs-and-calls)))
             )

          (cond ((< (length toplevel-calls) 1)
                     ((ewp-error-printer ewp)
                         (format #f
 "compile-topmodule: File ~s contains no toplevel calls!\n"
                                 module-file-name
                         )
                     )
               )
               ((> (length toplevel-calls) 1)
                     ((ewp-error-printer ewp)
                         (format #f
 "compile-topmodule: File ~s contains more than one toplevel call:\n"
                                 module-file-name
                         )
                         toplevel-calls
                     )
               )
               (else
                  (compile-toplevel-call-to-directory
                             (cdr (first toplevel-calls))
                             modulename
                             (or max-passes 101)
                             toplevel-defs
                             loglevel
                             exit-to-top
                 )
              )
          )
       )
    )
  )
)

;; compile-topmodule calls this, after it has read in the toplevel-call:
;; (Can be called also by programmer directly, when testing,
;; in that case, give as a modulename the name of subdirectory
;; where the resulting Verilog-files will be written to,
;; and give toplevel-defs as '()

(define (compile-toplevel-call-to-directory top-call-fis modulename max-passes toplevel-defs loglevel exit-to-top)

  (let* ((fundefines (read-library-files
                                  (make-ewp 'warning-printer warn
                                            'error-printer error
                                  )
                                  *BREAM-LIB-MODULES*
                                  toplevel-defs
                     )
         )
         (toc (make-fresh-toc modulename max-passes loglevel exit-to-top fundefines))
         (callees-fundef (toc-create-toplevel-define-wrapper toc top-call-fis))
        )
     (begin
        (toc-add-fullname.fundef-to-be-compiled-list!
                               toc
                               (string->symbol modulename)
                               callees-fundef
                               top-call-fis
        )
        (toc-compile-until-no-items-left toc)
     )
  )
)


;; Items yet to be compiled might be either ordinary (external) Scheme
;; functions or wirms. In both cases, a new Verilog output file
;; is created for each distinct one.

;; fff stands for (list fullname fundef fis) (where fis = funinfo-structure).

(define (toc-compile-until-no-items-left toc)
   (let loop ((i 0))
     (if (null? (caar (toc-to-be-compiled-items toc)))
         i
         (let ((fff (pop! (toc-to-be-compiled-items toc))))
            (toc-compile-fundef toc (first fff) (second fff) (third fff))
            (loop (+ 1 i))
         )
     )
  )
)


;; testset-one-test-with-typeresolve
;; testset-one-test-with-expsynta



;; Run e.g. as (run-typeresolve-testset "/home/karttu/bream/src/breamlib/restests.lst.scm" 1)

(define (run-typeresolve-testset testsetfile loglevel)
   (run-testset testsetfile loglevel testset-one-test-with-typeresolve)
)

(define (run-expsynta-testset testsetfile loglevel)
   (run-testset testsetfile loglevel testset-one-test-with-expsynta)
)


(define (run-testset testsetfile loglevel testset-fun)
  (call-with-current-continuation
    (lambda (exit-to-top)
      (let* ((fundefines (or ;; (list)
                             (read-library-files ;; If needed some day.
                                  (make-ewp 'warning-printer warn
                                            'error-printer error
                                  )
                                  *BREAM-LIB-MODULES*
                                  (list) ;; toplevel-defs
                             )
                         )
             )

             (toc (make-fresh-toc #f 101 loglevel exit-to-top fundefines))
            )

          (call-with-input-file testsetfile
             (lambda (inport)
                (let loop ((tsexp (read-typed-def inport))
                           (tests-read 0)
                           (tests-as-expected 0)
                           (tests-not-as-expected 0)
                          )
                  (cond ((eof-object? tsexp)
                           (format (current-output-port)
 "run-testset: ~s TESTS RUN, ~s AS EXPECTED, and ~s UNEXPECTED."
                                   tests-read
                                   tests-as-expected
                                   tests-not-as-expected
                           )
                        )
                        ((and (t-is-pair? tsexp) (= 3 (t-length tsexp)))
                           (let ((tr (testset-fun toc tsexp)))
                             (format (current-output-port) "\n\n\n")
                             (loop (read-typed-def inport)
                                    (+ 1 tests-read)
                                    (+ (if tr 1 0) tests-as-expected)
                                    (+ (if tr 0 1) tests-not-as-expected)
                             )
                           )
                        )
                        (else ;; Should be a list.
                            ((toc-warning-printer toc)
                                   (format #f
 "run-testset: Skipping a non-4-elem-list at file ~a ~s\n"
                                           testsetfile (texp-sprint tsexp)
                                   )
                            )
                            (loop (read-typed-def inport)
                                   tests-read
                                   tests-as-expected
                                   tests-not-as-expected
                            )
                        )
                  )
                )
             )
          )
      )
    )
  )
)


(define (testset-one-test-with-typeresolve toc test)
   (run-testset-one-test
           toc
           test
           (lambda (toc src)
             (let ((old-toc-exit (toc-toplevel-exit toc))
                   (res (call-with-current-continuation
                          (lambda (exit-here)
                              (set-toc-toplevel-exit! toc exit-here)
 ;; (expsynta toc src) ???
                              (typeresolve-toplevel-call! toc src (list))
                          )
                        )
                   )
                  )
                (set-toc-toplevel-exit! toc old-toc-exit)
                res
             )
           )
           "TYPERESOLVE-TESTSET"
           "typeresolve"
           "typeresolved"
   )
)


(define (testset-one-test-with-expsynta toc test)
   (run-testset-one-test
           toc
           test
           expsynta-for-testset!
           "EXPSYNTA-TESTSET"
           "rewrite"
           "rewrote"
   )
)


(define (run-testset-one-test toc test fun-to-run designation verb verb-past)
  (let* ((comment (t-u-first test))
         (before (expsynta toc (t-second test)))
         (expected-after-or-failure (t-third test)) ;; #f if expected to fail
         (real-after (texp-tree-copy before)) ;; Will be modified...
         (in-n-passes (fun-to-run toc real-after))
        )
     (begin
        (cond
          ((not in-n-passes)
            (if (not (t-elem expected-after-or-failure))
                (begin ;; Then...
                  ((toc-logging-printer2 toc) designation toc 1
                      (format #f
"[~a] SUCCEEDED. As expected, it failed to ~a THIS ~s, but instead only rewrote it to this: "
                              comment
                              verb
                              (texp-sprint before)
                      )
                      real-after
                  )
                  #t
                )
                (begin ;; Else...
                  ((toc-logging-printer2 toc) designation toc 1
                      (format #f
"[~a] FAILED!!!. Unexpectedly it failed to ~a ~s TO: "
                              comment
                              verb
                              (texp-sprint before)
                      )
                      expected-after-or-failure
                  )
                  #f
                )
            )
          )
          (else ;; fun-to-run succeeded,
;; but did it do it the way we expected?
            (cond ((not (t-elem expected-after-or-failure))
                     ((toc-logging-printer2 toc) designation toc 1
                         (format #f
"[~a] FAILED!!!. Although should have failed, ~a in ~s passes ~s TO: "
                                 comment
                                 verb-past
                                 in-n-passes
                                 (texp-sprint before)
                         )
                         real-after
                     )
                     #f
                  )

                  ((texp-trees-equal? real-after expected-after-or-failure)
                     ((toc-logging-printer2 toc) designation toc 1
                        (format #f
"[~a] SUCCEEDED. As expected, ~a in ~s passes ~s TO: "
                                 comment
                                 verb-past
                                 in-n-passes
                                 (texp-sprint before)
                        )
                        real-after
                     )
                     #t
                  )

                  (else
                     ((toc-logging-printer2 toc) designation toc 1
                        (format #f
"[~a] FAILED!!!. Should have ~a ~s to ~s, but ~a instead in ~s passes TO: "
                                 comment
                                 verb-past
                                 (texp-sprint before)
                                 (texp-sprint expected-after-or-failure)
                                 verb-past
                                 in-n-passes
                        )
                        real-after
                     )
                     #f
                  )

            )
          )
        )
     )
  )
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

