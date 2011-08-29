
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                 ;;
;; BREAM / toplevel.scm                                            ;;
;;   --  The toplevel module, for invoking all the necessary other ;;
;;       modules for performing all the phases of the compilation, ;;
;;       i.e. among other things:                                  ;;
;;         Reading the source module in + (all the library funs,   ;;
;;         syntax rewriting (cond, let*, etc.),                    ;;
;;         type resolving,                                         ;;
;;         compilation to level0-code,                             ;;
;;         and Verilog-output to files.                            ;;
;;                                                                 ;;
;; Try compiling with (compile-topmodule "test1out" ATLYS 1 1010)  ;;
;; or  (compile-topmodule "test1out" SP3 1 1001)                   ;;
;; for example!                                                    ;;
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
;; Edited    Aug 28 2011 by karttu.
;;   Changed toc-create-toplevel-define-wrapper by adding new I/O signals
;;   to the list of signals made available to the programmer.
;;   (e.g. in_buttons, un_uart_rxd and out_leds).
;;   Renamed also "switch" to "in_switches" and "txd" to "out_uart_txd"
;;   so as to conform a new general naming scheme for I/O signals.
;;
;;   Added another new argument,  platform, to compile-topmodule.
;;   Now the produced Verilog-code will be written under the
;;   subdirectory Compiled_for_<platform>, instead of being
;;   created under the same level as the Bream-source files themselves.
;;   Also, we copy with the new function toplevel-copy-platform-specific-files
;;   an appropriate master.ucf to that directory, as well as some other files.
;;

(define *BREAM-BASE-DIR* "~/bream")

(cd *BREAM-BASE-DIR*)

(load "./src/utilits1.scm") ;; Some common utility functions.

(load "./src/combfscm.scm") ;; Combinational bream functions for Scheme.

(load "./src/typesch1.scm") ;; The Typing Scheme #1.

(load "./src/srcstrct.scm") ;; For accessing type-annotated S-expr trees.

(load "./src/funinfos.scm") ;; Funinfo-structure.

(load "./src/dispatch.scm") ;; Must come before lev0veri.scm

(load "./src/expsynta.scm") ;; For cond, or, etc.

(load "./src/expwirms.scm") ;; For expanding wirm-macros.

(load "./src/compile1.scm") ;; The compilation module itself.

(load "./src/typreslv.scm") ;; Type (i.e. width) resolver.

(load "./src/combopti.scm") ;; For optimizing level-0 combinational exprs.

(load "./src/readdefs.scm") ;; Functions for reading in Bream function
;;                                 definitions


(load "./src/lev0veri.scm") ;; The Level0->Verilog backend.




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


(define (make-fresh-toc platform projectdir modulename max-passes loglevel exit-to-top fundefines)
              (make-toc 'outdir-name
                            (format #f "~a/Compiled_for_~a"
                                    projectdir
                                    platform
                            )
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
                          '(relative "src" "breamlib")
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


;; XXX -- To do: filter off those declarations of I/O signals (e.g. out_leds)
;; that are not used by the programmer's code, as to avoid a myriad
;; of warning signals at the later stages of compilation process
;; by the third party tools. (Comment them also out from the master.ucf
;; file while it is copied to <modulename>.ucf)

;; XXX -- Todo 2: allow making platform specific additional changes,
;; e.g. on ATLYS we could use the reset-button (signal T15) as a start,
;; if we inverted it before the use.
;; Also, we should finally implement the debounced start correctly.

(define (toc-create-toplevel-define-wrapper toc top-call-fis)
  (let* ((name_et_fargs
           (untyped (list
                       (untyped '<toplevel-call>)
                       (type-et-elem (type-i-of-definite-width 8) 'in_switches)
                       (type-et-elem (type-i-of-definite-width 3) 'in_buttons)
                       (type-et-elem type-1bit-input  'in_uart_rxd)
                       (type-et-elem type-1bit-output 'out_uart_txd)
                       (type-et-elem (type-i-of-definite-width 7) 'out_leds)
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


;; "SPARTAN3-200-SB" stands for Digilent's Spartan 3 XC3S200 Starter Board
;; and now, purely ad hoc, SP3 is short-hand for 'SPARTAN3-200-SB which I'm
;; too lazy to write every time and remember correctly, when starting
;; compile-topmodule:
(define SP3 "SPARTAN3-200-SB")
(define ATLYS "ATLYS") ;; This one just for avoiding typing two doublequotes


;; XXX -- Our quick-and-dirty implementation for copying certain files.
;; Many of those are not even needed, (like e.g. uart2defines.h if the
;; user's program doesn't refer to out_uart_txd).

(define (toplevel-copy-platform-specific-files ewp platform bream-base-dir destdir modulename)

   (let ((src-dir (format #f "~a/src/breamlib/Verilog/Board_specific/~a"
                              bream-base-dir
                              platform
                  )
         )
        )
     (cond ((not (file-directory? src-dir))
                     ((ewp-error-printer ewp)
                         (format #f
 "toplevel-copy-platform-specific-files: Could not find directory ~s ! Are you sure you entered the platform (second arg to compile-topmodule, ~s) correctly?"
                                 src-dir
                                 platform
                         )
                     )
           )
           (else
             (let* ((all-files (directory-read (string-append src-dir "/")))
                    (ucf-files
                      (keep-matching-items
                        all-files
                        (lambda (pn)
                           (string-suffix-ci? ".ucf" (->namestring pn))
                        )
                      )
                    )
                    (verilog-files
                      (keep-matching-items
                        all-files
                        (lambda (pn)
                           (string-suffix-ci? ".v" (->namestring pn))
                        )
                      )
                    )
                   )
               (begin
                (cond ((not (= 1 (length ucf-files)))
                        ((ewp-error-printer ewp)
                           (format #f
 "toplevel-copy-platform-specific-files: There should be exactly ONE ucf-file in  ~s ! Found ~s."
                                 src-dir
                                 (length ucf-files)
                           )
                        )
                      )
                      (else ;; Copy master.ucf to destdir as <modulename>.ucf
                          (copy-file (first ucf-files)
                                     (format #f "~a/~a.ucf" destdir modulename)
                          )
                      )
                )
;; Copy the rest of files (currently only those with extension .v)
;, with the same name to the destination directory:
                (for-each (lambda (src-file-path)
                            (copy-file
                                src-file-path
                                (pathname-new-directory
                                    src-file-path
                                    (pathname-directory
                                       (->pathname (string-append destdir "/"))
                                    )
                                )
                            )
                          )
                          verilog-files
                )
               )
             )
           )
     )
   )
)


;; the toplevel-file-name should the name (without .brm.scm extension)
;; of the file, where the toplevel-call is located, together
;; with some additional calling convention definitions, etc.
;; The file should be situated in directory of the same name,
;; i.e. <modulename>/<modulename>.brm.scm

(define (compile-topmodule modulename platform loglevel max-passes)
  (call-with-current-continuation
    (lambda (exit-to-top)
       (let* ((projectdir (format #f "~a/testprojects/~a" ;; XXX - will change!
                                  *BREAM-BASE-DIR*
                                  modulename
                          )
              )

              (destdir (format #f "~a/Compiled_for_~a" projectdir platform))
              (dummy1 (make-directory-unless-it-already-exists destdir))

              (module-file-name (format #f "~a/~a.brm.scm" projectdir modulename))
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

          (toplevel-copy-platform-specific-files
                   ewp platform *BREAM-BASE-DIR* destdir modulename
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
                             platform
                             projectdir
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

(define (compile-toplevel-call-to-directory top-call-fis
                                            platform
                                            projectdir
                                            modulename
                                            max-passes
                                            toplevel-defs
                                            loglevel
                                            exit-to-top
        )

  (let* ((fundefines (read-library-files
                                  (make-ewp 'warning-printer warn
                                            'error-printer error
                                  )
                                  *BREAM-LIB-MODULES*
                                  toplevel-defs
                     )
         )
         (toc (make-fresh-toc platform projectdir modulename max-passes loglevel exit-to-top fundefines))
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



;; Run e.g. as (run-typeresolve-testset "./src/breamlib/restests.lst.scm" 1)

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

             (toc (make-fresh-toc "no-platform" "no-projectdir" "no-module"
                                  101 loglevel exit-to-top fundefines
                  )
             )
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

