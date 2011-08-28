;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                  ;;
;; BREAM / readdefs.scm                                             ;;
;;   --  Code for reading in Bream function definitions             ;;
;;                                                                  ;;
;; The entry point is (...)                                         ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Unless otherwise mentioned, all the files in this code tree are
;; Copyright (C) 2010-2011 Antti Karttunen, subject to the terms of the GPL v2.
;; See the file COPYING for more information.  Contact the author for
;; further developments at the address: <his-firstname>.<his-surname>@gmail.com
;;

;;
;; Antti Karttunen ("karttu") started this module Oct 10 2010,
;; by transferring read-source-file! from module compile1.scm

;;
;; Edited    Oct 10 2010 by karttu.
;;   Added a new structure funsrc-info
;;
;; Edited    Oct 28 2010 by karttu.
;;   Added read-typed-def
;;
;; Edited    Nov 04 2010 by karttu.
;;    Accessors expr-* renamed to t-* and accessors dt-* renamed to t-u-*
;;
;; Edited    Nov 21 2010 by karttu.
;;    Changed funsrc-info structure to contain multiple fields containing
;;    the function body in various stages of expansion/type-annotation.
;;    Added argument coc to read-library-files and read-source-file!
;;    Renamed the structure funsrc-info as funinfo.
;;
;; Edited    Nov 27 2010 by karttu.
;;    Moved funinfo-structure and related functions to
;;    a new module funinfos.scm
;;
;; Edited    May 15 2011 by karttu
;;    Changed all (format (notification-output-port) ...) calls
;;    to ((coc-warning-printer coc) (format #f ...)) calls,
;;    and added the check for the uniqueness of defined funname.
;;
;; Edited    May 20 2011 by karttu
;;    Changed all coc's to ewp's.
;;    Added define-wirm (in addition to ordinary define's) to the
;;    list of definition-keywords to be recognized and read in.
;;
;; Edited    May 21 2011 by karttu
;;    Corrected a small bug in read-source-file! and changed the
;;    warning printings to use texp-sprint instead of detyped.
;;
;; Edited    Jun 02 2011 by karttu
;;    Added declare-extfun to the list of toplevel keywords 
;;    to be recognized and read in.
;;    The syntax is like
;;    (declare-extfun 1'(outbyte1200 output'outchan 8'byte_to_output))
;;    i.e. like ordinary define, but without the body, and the return
;;    type must be specified at the front of name-fargs list.
;;    (Because the body has been already written, e.g. manually
;;     with Verilog).
;;
;; Edited    Aug 19 2011 by karttu
;;    Now also toplevel-calls are wrapped as funinfo-structures
;;    with a special deftype <toplevel-call>
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Produces a list of two elements, of which the first element
;; is an assoc-list of (funname . (define (fuuname arg1 ... argn) body))
;; pairs.
;; The second element is a list containing toplevel-invocations ("calls").

;; TODO - XXX - Should contain more info:
;; (funname filename starting-line-number ending-line-number function-definition-or-invocation)
;;
;; Detect (-MODULE-INFO-BEGIN-) and (-MODULE-INFO-END-) sentinels
;; and copy the text between into a string which can be referenced
;; with filename (and later copied to the compiled module).
;; (Note: Guile is case-sensitive. Beware of introducing case-related bugs!)


(define-structure (module-info (keyword-constructor))
    filename

;;  definitions ;; No more here.
;;  toplevel-invocations ;; No more here.

    text
)


;; (define ekat (read-source-file! "bream/src/intbasic.brm.scm" (list) (list)))
;; (define test-defs
;;        (read-source-file! "bream/src/simpleio.brm.scm"
;;                            (module-info-definitions ekat)
;;                            (module-info-toplevel-invocations ekat)
;;        )
;; )

(define (read-library-files ewp filenames previous-definitions)
  (let loop ((filenames filenames)
             (definitions previous-definitions) ;; From toplevel-module.
             (toplevel-invocations #f)
            )
     (cond ((not (pair? filenames)) (reverse! definitions))
           (else
              (let ((defs-and-calls
                       (read-source-file! (first filenames)
                                          ewp
                                          definitions
                                          toplevel-invocations
                       )
                   ))
                 (loop (cdr filenames)
                       (car defs-and-calls)
                       (cdr defs-and-calls)
                 )
              )
           )
     )
  )
)


(define (read-source-file! infile ewp definitions toplevel-invocations)
 (let ((moi (make-module-info 'filename infile)))
   (call-with-input-file infile
     (lambda (inport)
        (let loop ((tsexp (read-typed-def inport))
                   (definitions definitions)
                   (toplevel-invocations toplevel-invocations)
                  )
             (cond
                ((eof-object? tsexp)
                   (cons definitions toplevel-invocations)
;; XXX - was like this, remove:
;;                 (set-module-info-definitions! moi
;;                                               (reverse! definitions)
;;                 )
;;                 (set-module-info-toplevel-invocations! moi
;;                                              (reverse! toplevel-invocations)
;;                 )
;;                 moi
                )
                ((not (t-is-pair? tsexp))
                    ((ewp-warning-printer ewp)
                       (format #f
   "**read-source-file! Skipping an atomic form at the toplevel of file ~s: ~a\n"
                            infile (texp-sprint tsexp)
                       )
                    )
                    (loop (read-typed-def inport) definitions toplevel-invocations)
                )
                ((case (t-u-first tsexp)
                   ((-MODULE-INFO-BEGIN-)
  ;; XXX - Read the text up to next (-MODULE-INFO-END-)
                     (let inloop ((lines (list (read-line inport))))
                          (cond ((eof-object? (car lines))
                                   (error
  "read-source-file! Missing (-MODULE-INFO-END-) sentinel in source file: "
                                          infile
                                   )
                                )
                                ((string-search-forward-ci "(-MODULE-INFO-END-)"
                                                           (car lines)
                                 ) => (lambda (offset) ;; Do some surgery...
                                         (set-car! lines
                                                   (string-head (car lines)
                                                                offset
                                                   )
                                         )
                                         (set-module-info-text! moi
                                                               (reverse! lines)
                                         )
;; And then proceed in the outer loop as usual:
                                         (loop (read-typed-def inport)
                                               definitions
                                               toplevel-invocations
                                         )
                                      )
                                )
                                (else (inloop (cons (read-line inport) lines)))
                          )
                     )
                   )
  
                   ((define define-wirm declare-extfun)
                      (let ((what (t-second tsexp)))
                         (cond ((not (t-is-pair? what))
                                 ((ewp-warning-printer ewp)
                                    (format #f
 "**read-source-file! Skipping an atomic define at the toplevel of file ~s: ~a\n"
                                            infile (texp-sprint tsexp)
                                    )
                                 )
                                 (loop (read-typed-def inport)
                                       definitions
                                       toplevel-invocations
                                 )
                               )

;; In future versions, we could/should append the alternative versions
;; into typed-definitions or some such slot of funinfo structure:
;; (When the type-resolved can handle such scenarios in some well-defined
;; manner.)
                               ((assoc (t-u-first what) ;; Not unique?
                                       definitions
                                )
                                 => (lambda (funname.funinfo)
                                      ((ewp-warning-printer ewp)
                                        (format #f
 "**read-source-file! Multiple definitions with varying type signatures not implemented yet: Skipping an alternative definition of ~s at the file ~s: ~a\n"
                                            (t-u-first what)
                                            infile (texp-sprint tsexp)
                                        )
                                      )
                                      (loop (read-typed-def inport)
                                            definitions
                                            toplevel-invocations
                                      )
                                    )
                               )
                               (else ;; It's OK: (define (foo ...) something)
                                 (loop (read-typed-def inport)
                                       (cons (cons (t-u-first what)
                                                   (make-funinfo
                                                     'deftype (t-u-first tsexp)
                                                     'funname (t-u-first what)
                                                     'org-definition tsexp
                                                     'moi moi
                                                   )
                                             )
                                             definitions
                                       )
                                       toplevel-invocations
                                 )
                               )
                          )
                      )
                   )

                   (else ;; Otherwise, check if we are collecting toplevel
;;                          calls, and if not, then give a warning:
                       (cond ((pair? toplevel-invocations) ;; Collecting them?
                                (loop (read-typed-def inport)
                                      definitions
                                      (cons (cons '<toplevel-call>
                                                   (make-funinfo
                                                     'deftype
                                                       '<define-toplevel-call>
                                                     'funname '<toplevel-call>
                                                     'org-definition tsexp
                                                     'moi moi
                                                   )
                                             )
                                             toplevel-invocations
                                      )
                                )
                             )
                             (else
                               ((ewp-warning-printer ewp)
                                   (format #f
 "**read-source-file! Skipping a toplevel invocation at the library file ~s: ~a\n"
                                           infile (texp-sprint tsexp)
                                   )
                               )
                               (loop (read-typed-def inport)
                                     definitions
                                     toplevel-invocations
                               )
                             )
                       )
                   )
                ))
             )
        ) ;; let loop
     )
   )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (read-typed-def inport)
   (let ((sexp (read inport)))
      (cond ((eof-object? sexp) sexp)
            (else (typify sexp type-default-unresolved 'quote))
      )
   )
)

