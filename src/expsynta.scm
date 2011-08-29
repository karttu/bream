
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                 ;;
;; BREAM / expsynta.scm                                            ;;
;;   --  Functions for expanding various Bream special forms.      ;;
;;       These are (mostly) purely syntactic rewritings,           ;;
;;       which would most make sense also in Scheme.               ;;
;;                                                                 ;;
;; NOTE: This module is now invoked almost directly after the      ;;
;;       function definitions have been read in from the source    ;;
;;       file(s). Care must be taken, that none of the forms       ;;
;;       will refer to any type-annotations (width-information).   ;;
;;       The entry point is (expsynta toc src)                     ;;
;;       which will return an expanded copy of src.                ;;
;;                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Unless otherwise mentioned, all the files in this code tree are
;; Copyright (C) 2010-2011 Antti Karttunen, subject to the terms of the GPL v2.
;; See the file COPYING for more information.  Contact the author for
;; further developments at the address: <his-firstname>.<his-surname>@gmail.com
;;

;;
;; Antti Karttunen ("karttu") transferred this code from compile1.scm
;; module at Sep 18 2010.
;;
;; This module implements the standard (I hope so!) Scheme syntactic
;; rewritings, from cond to if, etc.
;; I could have taken most of these from the Appendix of Revised Report,
;; but I didn't.
;;
;; XXX - Todo: case, at least.
;;

;;
;; Edited    Oct 27 2010 by karttu.
;;   Rewrote all expanding forms (except \\) so that they assume
;;   that all source-expressions are now type-annotated.
;;   Some of the type-values used are so far very ad hoc,
;;   and furthermore, some of these (like expansion for cond)
;;   are also very ugly. Invent something cleaner!
;;
;; Edited    Oct 28 2010 by karttu.
;;   Added one more kludgous argument for generic-dispatch.
;;
;;
;; Edited    Nov 04 2010 by karttu.
;;    Accessors expr-* renamed to t-* and accessors dt-* renamed to t-u-*
;;    All cc stuff renamed coc (= compiling context, not current continuation!)
;;
;; Edited    Nov 21 2010 by karttu.
;;    Changed expansion of forms like 'cond' and 'or' to be complete,
;;    instead of incremental expansion like it was before.
;;    (No more interleaved with the Bream->Level0 compilation.)
;;    Added some clarifying notes to the beginning.
;;    Added test-expsynta-rewrite for testing.
;;    Changed let-named to <let-named>
;;
;; Edited    May 15 2011 by karttu.
;;    Created a special form for define, for rewriting tail-recursive calls
;;    to named let. (Not anymore done in compile1.scm, because
;;    needed also by the type-resolver.)
;;
;; Edited    May 19 2011 by karttu.
;;    All coc's renamed rwc's, as this should be independent of compiling mod.
;;
;; Edited    May 20 2011 by karttu.
;;    Slight changes to rewriting form of define.
;;
;; Edited    May 29 2011 by karttu.
;;    Added "conc -> conc2" conversion.
;;
;; Edited    Jul 20 2011 by karttu.
;;    Revamped the whole dispatching system, to use the same
;;    dispatch-to-generic-form2 function that typreslv.scm calls,
;;    instead of an old generic-dispatch.
;;    For this, added an extra argument "extra" to all rewriting-forms.
;;    (Don't know yet, what to do it in vanilla rewriting.)
;;    The first version of let-unrolled testable!
;;
;; Edited    Jul 27 2011 by karttu.
;;    Added toc argument to call to expsynta and the first few additions
;;    for wirm-expansion.
;;    Most of the wirm-related code transferred to a new module expwirms.scm
;;
;; Edited    Aug 23 2011 by karttu.
;;    Now the expander for (lambda) will optimize any lambda-forms 
;;    left by wirm-expansion (currently only those!)
;;    whose body is constant integer, with just that integer,
;;    instead of ((lambda (name farg1 ... fargn) integer) carg1 ... cargn)
;;
;; Edited    Aug 23 2011 by karttu.
;;    Added rwc-find-varnames-def for the needs of new (experimental) code
;;    in expwirms.scm
;;
;; Edited    Aug 26 2011 by karttu.
;;    Added *c -> *c2 rewriting.
;;
;; Edited    Aug 28 2011 by karttu.
;;    A few extra arguments for make-fresh-toc
;;

;; New items are attached with attach! to the front, so they can be found
;; even when coming as fall-back for (wirm ...) forms.
(define *regular-rewriting-forms* (list (list (list))))

(define *rewriting-forms*
  (list
      (list '(regular 0.1 draft) *regular-rewriting-forms*)
      (list '(wirm 0.1 draft) (list (cons #t *regular-rewriting-forms*)))
  )
)




;; Later, if there comes bugfixes or other improvements
;; (which will change the compiler's output!)
;; to any of compiled-form's, they will be assigned to a new level
;; of calling-conventions, so that we will not break any existing
;; code out there, that used to compile OK!
;; (Essentially, we freeze the earlier compiling definitions
;; to their own levels. This for those that have been raised to
;; the status "final". Those marked "draft" can be still edited.

;;
;;


(define-syntax define-rewritten-form
   (syntax-rules ()
     ((define-rewritten-form (CACO formname src rwc extra) body)
        (attach!
             (cons (quote formname) (lambda (src rwc extra) body))
             (list-of-forms-by-dic (quote CACO) *rewriting-forms*)
        )
     )
   )
)


(define-structure (rwc (keyword-constructor) (copier))
    parent-toc
;;  trc-from-typeresolver ;; If called from typreslv.scm

    warning-printer
    error-printer

    logging-printer
    logging-printer2

    rewritten-forms
    rewriting-convention-in-use

    names2deflocs_et_inits

    labels2loops ;; assoc list of (label . loop-info)

    unrolling-depth ;; Through how many "unroll a loop" expanded lambdas
;;                     we have already come by?
    loop-unrolling-level ;; Expansion level, for recursive macros & like.

    closest-et ;; Latest enclosing expected width of wirm-expanded lambda

    wirm-depth ;; Through how many wirm-expanded lambdas we have already come?

    max-wirm-depth ;; Max. for above.
)


(define (toc-make-fresh-rwc toc which-rewriting-convention wirm-depth etype)
  (let ((caller-module
             (cond ((equal? which-rewriting-convention
                            (toc-rewriting-convention-in-use toc)
                    ) "EXPSYNTA"
                   )
                   ((equal? which-rewriting-convention
                            (toc-wirm-rewriting-convention-in-use toc)
                    ) "EXPWIRMS"
                   )
                   (else "EXP-UNKNOWN?")
             )
       ))

     (make-rwc
            'parent-toc toc
            'warning-printer warn
            'error-printer error
            'logging-printer
               (lambda (rwc . rest)
                  (apply (toc-logging-printer (rwc-parent-toc rwc))
                         (cons* caller-module (rwc-parent-toc rwc) rest)
                  )
               )

            'logging-printer2
               (lambda (rwc . rest)
                  (apply (toc-logging-printer2 (rwc-parent-toc rwc))
                         (cons* caller-module (rwc-parent-toc rwc) rest)
                  )
               )


            'rewritten-forms *rewriting-forms*
            'rewriting-convention-in-use which-rewriting-convention
            'names2deflocs_et_inits (list)
            'labels2loops (list)
            'unrolling-depth 0
            'loop-unrolling-level 0
            'wirm-depth wirm-depth
            'max-wirm-depth 65
            'closest-et etype
     )
  )
)


(define (rwc-list-of-rewritten-forms-in-use rwc)
  (list-of-forms-by-dic (rwc-rewriting-convention-in-use rwc)
                        (rwc-rewritten-forms rwc)
  )
)


(define (test-expsynta-rewrite src loglevel)
 (call-with-current-continuation
  (lambda (exit-to-top)
    (let* ((max-passes 1) ;; Not relevant here.
           (fundefines (list))
           (toc (make-fresh-toc "no-platform"
                                "no-projectdir"
                                "no-module"
                                max-passes
                                loglevel
                                exit-to-top
                                fundefines
                )
           )
           (rwc (toc-make-fresh-rwc toc 
                                    (toc-rewriting-convention-in-use toc)
                                    0
                                    type-default-unresolved
                )
           )
           (result (expsynta-rewrite
                       (typify src type-default-unresolved 'quote)
                       rwc
                       #f ;; Our extra.
                   )
           )
          )
       (texp-print-with-nl result (current-output-port))
       result
    )
  )
 )
)


(define (expsynta-for-testset! toc src)
  (set-toc-rewriting-changes-made! toc 0)
  (call-with-current-continuation
    (lambda (exitfun)
       (expsynta-in-place! toc src)
       (toc-rewriting-changes-made toc)
    )
  )
)


(define (expsynta toc src)
 (expsynta-rewrite
      src
      (toc-make-fresh-rwc toc 
                          (toc-rewriting-convention-in-use toc)
                          0
                          (t-type src)
      )
      #f ;; C'est extra.
 )
)

(define (expsynta-in-place! toc old-src)
   (texp-overwrite-in-place! old-src (expsynta toc old-src))
)


;; The old version commented out Jul 20 2011:
;;
;; (define (expsynta-rewrite src rwc) ;; rwc = rewriting context.
;;    (generic-dispatch (t-elem src)
;; 
;;                      src
;; 
;;                      (rwc-list-of-rewritten-forms-in-use rwc)
;; 
;;                      (lambda (s) (t-elem (car s)))
;; 
;; ;; Invoke this as long as the source doesn't change anymore:
;; ;; (Until we find a fixed point. Beware of infinitely recursive definitions!)
;;                      (lambda (new-src)
;;                         (if (equal? new-src src)
;;                             new-src
;;                             (expsynta-rewrite new-src rwc)
;;                         )
;;                      )
;; 
;;                      (lambda (x . rest) x) ;; If not a pair, return intact.
;; 
;; ;; If a t-expr which by itself is not an expanded macro,
;; ;; then we shall expand all the rest. (This is called when src is a pair,
;; ;; but nothing is found with its first element).
;; ;; Note: if the first element changes due to expansion, we should call
;; ;; this yet another time... (Now commented out for a moment).
;;                      (lambda (src rwc)
;; ;;                     (expsynta-rewrite
;;                          (type-et-elem
;;                            (t-type src)
;;                            (map (lambda (subsrc) (expsynta-rewrite subsrc rwc))
;;                                 (t-elem src)
;;                            )
;;                          )
;; ;;                       rwc
;; ;;                     )
;;                      )
;; 
;;                      rwc
;;    )
;; )
;; 


;; The new version:
(define (expsynta-rewrite src rwc extra)
   (let loop ((src src) (i 1))
     (let* ((dummy1 ((rwc-logging-printer2 rwc) rwc 5
                       (format #f
 "expsynta-rewrite, before expansion ~s, wirm-depth=~s, SRC="
                             i
                             (rwc-wirm-depth rwc)
                       )
                       src
                    )
            )

            (exp-src (dispatch-to-generic-form2
                         #f
                         src
                         rwc
                         (rwc-list-of-rewritten-forms-in-use rwc)
                         extra
                     )
            )

            (dummy2 ((rwc-logging-printer2 rwc) rwc 5
                       (format #f
 "expsynta-rewrite, after expansion ~s, wirm-depth=~s, ~a SRC=~a, exp-src="
                            i
                            (rwc-wirm-depth rwc)
                            (if (texp-trees-equal? exp-src src)
                                "RETURNING, as found a fix-point:"
                                "STILL CHANGING, REWRITING AGAIN:"
                            )
                            (texp-sprint src)
                       )
                       exp-src
                    )
            )
           )
         (cond ((texp-trees-equal? exp-src src) exp-src) ;; Found a fix point?
               (else
                     (set-toc-rewriting-changes-made!
                         (rwc-parent-toc rwc)
                         (1+ (toc-rewriting-changes-made (rwc-parent-toc rwc)))
                     )
                     (loop exp-src (1+ i)) ;; Rewrite once again.
               )
         )
     )
   )
)


;; A few wrappers:



(define (rwc-call-with-new-unrolling-depth rwc new-unrolling-depth lambda_rwc)
   (let ((new-rwc (copy-rwc rwc)))
     (set-rwc-unrolling-depth! new-rwc new-unrolling-depth)
     (lambda_rwc new-rwc)
   )
)


(define (expsynta-with-new-unrolling-depth src rwc extra new-unrolling-depth)
   (rwc-call-with-new-unrolling-depth rwc new-unrolling-depth
      (lambda (rwc) (expsynta-rewrite src rwc extra))
   )
)


(define (rwc-call-with-new-loopinfo-and-loop-unrolling-level rwc new-loop-label new-loop-info
             loop-unrolling-level lambda_rwc)
   (let ((new-rwc (copy-rwc rwc)))
     (set-rwc-labels2loops!
             new-rwc ;; Apply side-effects only to the new copy!
             (cons (cons new-loop-label new-loop-info)
                   (rwc-labels2loops rwc)
             )
     )
     (set-rwc-loop-unrolling-level! new-rwc loop-unrolling-level)
     (lambda_rwc new-rwc)
   )
)



(define (expsynta-with-new-loopinfo-and-loop-unrolling-level src rwc extra new-loop-label new-loop-info loop-unrolling-level)
   (rwc-call-with-new-loopinfo-and-loop-unrolling-level
      rwc new-loop-label new-loop-info loop-unrolling-level
      (lambda (rwc) (expsynta-rewrite src rwc extra))
   )
)


(define (rwc-call-with-new-et-and-wirm-depth
             rwc new-closest-et new-wirm-depth lambda_rwc
        )
   (let ((new-rwc (copy-rwc rwc)))
     (set-rwc-closest-et! new-rwc new-closest-et)
     (set-rwc-wirm-depth! new-rwc new-wirm-depth)
     (lambda_rwc new-rwc)
   )
)

(define (expsynta-with-new-et-and-wirm-depth
               src rwc extra new-closest-et new-wirm-depth)
   (rwc-call-with-new-et-and-wirm-depth
      rwc new-closest-et new-wirm-depth
      (lambda (rwc) (expsynta-rewrite src rwc extra))
   )
)

(define (expsynta-with-new-et-and-wirm-depth-and-more-bindings
        src rwc extra new-closest-et new-wirm-depth more-names2deflocs_et_inits
        )
   (rwc-call-with-additional-bindings rwc more-names2deflocs_et_inits
      (lambda (rwc)
         (rwc-call-with-new-et-and-wirm-depth
            rwc new-closest-et new-wirm-depth
            (lambda (rwc) (expsynta-rewrite src rwc extra))
         )
      )
   )
)




(define (rwc-call-with-additional-bindings rwc more-names2deflocs_et_inits
                                           lambda_rwc)
 (let ((new-rwc (copy-rwc rwc)))
   (set-rwc-names2deflocs_et_inits!
      new-rwc ;; Apply side-effects only to the new copy!
      (append more-names2deflocs_et_inits (rwc-names2deflocs_et_inits new-rwc))
   )
   (lambda_rwc new-rwc)
 )
)


(define (expsynta-with-more-bindings src rwc extra more-names2deflocs_et_inits)
   (rwc-call-with-additional-bindings rwc more-names2deflocs_et_inits
      (lambda (rwc) (expsynta-rewrite src rwc extra))
   )
)



(define (expsynta-rewrite-functor-and-cargs-of-funcall src rwc extra)
   (type-et-elem-et-definfo
         (t-type src)
         (map (lambda (subsrc) (expsynta-rewrite subsrc rwc extra))
              (t-elem src)
         )
         (t-definfo src)
   )
)


(define (expsynta-list-of-texps! list-of-texprs rwc extra)
   (for-each (lambda (carg)
              (texp-overwrite-in-place! carg (expsynta-rewrite carg rwc extra))
             )
             list-of-texprs
   )
   list-of-texprs
)

(define (expsynta-only-cargs! src rwc extra)
   (expsynta-list-of-texps! (t-carglist src) rwc extra)
   src
)



(define (rwc-find-varnames-def rwc src-name)
   (cond ((assq (t-elem src-name) (rwc-names2deflocs_et_inits rwc))
              => third ;; i.e. third of (list (t-elem farg) farg carg)
         )
         (else #f) ;; Didn't found it.
;;          ((rwc-error-printer rwc)
;;              (format #f
;; "rwc-find-varnames-definition: Unmapped variable referenced: ~a not in: "
;;                      (t-elem src-name)
;;              )
;;              (rwc-names2deflocs_et_inits rwc)
;;          )
   )
)


;; Here <terminal-argument> acts just as wildcard mark for all non-matched
;; atomic items. Return them as they are.
(define-rewritten-form ((regular 0.1 draft) <terminal-argument> src rwc extra)
   src
)


;; src is (lambda (farg1 farg2 ... fargn) body)
;; This SHOULD NOT be encountered, as the next form takes care of all
;; lambdas. (Unless in some anomalous positions?)
(define-rewritten-form ((regular 0.1 draft) lambda src rwc extra)
    (let ((lev (get-lambda-expansion-level (t-type (t-first src)))))
;;    (format #t "Calling rewriting-form for lambda\n")
      (error "Rewriting handler for lambda should not be called!")
    )
)


;; src is ((lambda (farg1 farg2 ... fargn) body) carg1 carg2 ... cargn)
;; First cargs are expanded recursively, and together with fargs,
;; the expanded cargs are collected to additional-bindings assoc list
;; (which should be in similar format as used by typeresolver),
;; Then the body is rewritten recursively, with both the additional bindings
;; and any expansion-depth information (if present as "lambda's special type")
;; temporarily passed in rwc
;;
;; In ordinary expansion mode we don't (currently) need additional-bindings,
;; but who knows what is needed in future. For now it is only for wirms.
;;

(define-rewritten-form ((regular 0.1 draft) (lambda) src rwc extra)
 (begin
  (let* ((lambda-keyword (t-first (t-first src)))
         (lambdas-type (t-type lambda-keyword))
         (fargs (t-second (t-first src)))
         (cargs (t-carglist src))
         (cargs-expanded (expsynta-list-of-texps! cargs rwc extra))
         (org-body (body-with-no-implicit-progn
                           (cdr (t-rest (t-first src)))
                           rwc
                           "lambda"
                   )
         )
         (additional-bindings
                (just-bind-named-exprs (t-elem fargs) cargs-expanded)
         )
         (dummy1 ((rwc-logging-printer2 rwc) rwc 5
                    (format #f
  "In (lambda)-handler: wirm-depth=~s, lambda's wirmexp level=~s, more-bindings=~a, SRC="
                            (rwc-wirm-depth rwc)
                            (get-lambda-wirm-expansion-level lambdas-type)
                            (texp-sprint additional-bindings)
                    )
                    src
                 )
         )

         (exp-body (cond
                     ((lambda-not-special? lambdas-type)
;;                       (format #t "Lambda not special, additional-bindings=")
;;                       (tp additional-bindings)

                          (expsynta-with-more-bindings
                              org-body
                              rwc
                              extra
                              additional-bindings
                          )
                     )
                     ((lambda-left-by-loop-unrolling? lambdas-type)
;;                       (format #t "Lambda's unrolling level=~s, additional-bindings="
;;                               (get-lambda-loop-unrolling-level lambdas-type)
;;                       )
;;                       (tp additional-bindings)

                         (expsynta-with-new-unrolling-depth
                              org-body
                              rwc
                              extra
                              (get-lambda-loop-unrolling-level lambdas-type)
                         )
                     )
                     ((lambda-left-by-wirm-expansion? lambdas-type)
;;                       (format #t "Lambda's wirmexp level=~s, additional-bindings="
;;                               (get-lambda-wirm-expansion-level lambdas-type)
;;                       )
;;                       (tp additional-bindings)
                         (expsynta-with-new-et-and-wirm-depth-and-more-bindings
                              org-body
                              rwc
                              extra
                              (t-type src) ;; The new closest enclosing type
                              (get-lambda-wirm-expansion-level lambdas-type)
                              additional-bindings
                         )
                     )
                     (else
                         (error "lambda has invalid type: " lambdas-type src)
                     )
                   )
         )
        )


    (if (and (lambda-left-by-wirm-expansion? lambdas-type)
             (resolved-int? exp-body)
        )

        exp-body ;; Our optimization.

        (type-et-elem (t-type src)
                      (cons (type-et-elem (t-type (t-first src))
                                          (list lambda-keyword fargs exp-body)
                            )
                            cargs-expanded
                      )
        )
    )
  )
 )
)


;; Here <generic-funcall> acts just as wildcard mark for all non-matched
;; functions. We just call expsynta-rewrite recursively for all arguments.
(define-rewritten-form ((regular 0.1 draft) <generic-funcall> src rwc extra)
  (begin
    ((rwc-logging-printer2 rwc) rwc 5
                    (format #f
 "In <generic-funcall>-handler: wirm-depth=~s, SRC="
                            (rwc-wirm-depth rwc)
                    )
                    src
    )
    (expsynta-rewrite-functor-and-cargs-of-funcall src rwc extra)
  )
)



;; Let somebody else handle it, if it is an error:
;; (Now, in our case it is actually a lambda's farg-ist)
(define-rewritten-form ((regular 0.1 draft) (<generic-funcall>) src rwc extra)
  (begin
;;  (format #t "Calling rewriting-form for (<generic-funcall>) with src=\n")
;;  (texp-print-with-nl src (current-output-port))
    (expsynta-rewrite-functor-and-cargs-of-funcall src rwc extra)
;;  ((rwc-error-printer rwc)
;; "compile: First-class functions not implemented yet. Don't know what to do with form: " src)
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (fold-multiple-arg-form src)
  (let* ((orgtype (t-type src))
         (dyadic-name (symbol-append (t-u-first src) (string->symbol "2")))
         (dyadic-call (type-et-elem (t-type (t-first src)) dyadic-name))
        )
    (cond ((= (t-length src) 2) (t-second src)) ;; Here we drop orgtype!
          ((= (t-length src) 3)
             (type-et-elem orgtype (cons dyadic-call (t-carglist src)))
          )
          (else
            (reduce-right (lambda (x y)
                              (type-et-elem orgtype (list dyadic-call x y))
                          )
                          '()
                          (t-carglist src)
            )
          )
    )
  )
)

;; Use as:
;; (fold-multiple-arg-form '(AND expr1)) --> expr1
;; (fold-multiple-arg-form '(AND expr1 expr2)) --> (and2 expr1 expr2)
;; (fold-multiple-arg-form '(AND expr1 expr2 expr3 expr4 expr5))
;; --> (and2 expr1 (and2 expr2 (and2 expr3 (and2 expr4 expr5))))


(define-rewritten-form ((regular 0.1 draft) conc src rwc extra)     (fold-multiple-arg-form src))

(define-rewritten-form ((regular 0.1 draft) *c src rwc extra)       (fold-multiple-arg-form src))

(define-rewritten-form ((regular 0.1 draft) seq src rwc extra)     (fold-multiple-arg-form src))
(define-rewritten-form ((regular 0.1 draft) seq-and src rwc extra) (fold-multiple-arg-form src))
(define-rewritten-form ((regular 0.1 draft) seq-or src rwc extra)  (fold-multiple-arg-form src))
(define-rewritten-form ((regular 0.1 draft) par src rwc extra)     (fold-multiple-arg-form src))
(define-rewritten-form ((regular 0.1 draft) fastest src rwc extra) (fold-multiple-arg-form src))
(define-rewritten-form ((regular 0.1 draft) slowest src rwc extra) (fold-multiple-arg-form src))
(define-rewritten-form ((regular 0.1 draft) and src rwc extra)     (fold-multiple-arg-form src))
(define-rewritten-form ((regular 0.1 draft) and-s src rwc extra)   (fold-multiple-arg-form src))
(define-rewritten-form ((regular 0.1 draft) or-f src rwc extra)    (fold-multiple-arg-form src))



;; Checks whether the given function name occurs anywhere in src
;; as in calling position:
;; Doesn't check for named lets. Nor calls like (map fun ...)
;; or (apply fun ...)
;; But detects them in calls like (map (lambda (x) (fun ...)))

;; There are a lots of possibilities for insidious bugs here,
;; when we have different functions scanning the source in
;; subtly different ways. E.g. both of these will think
;; that (define (foo x) (let* ((foo 7) (bar 8) (let bar)) ...))
;; contains a kind of thing they are searching for.

(define (calls? name t-src)
   (cond ;; ((not (pair? t-src)) #f)
         ((not (t-is-pair? t-src)) #f)
         ((eq? (t-u-first t-src) name) t-src) ;; Yes, found it!
         ((eq? (t-u-first t-src) 'quote) #f) ;; Not in this branch!
         ((there-exists? (t-elem t-src) (lambda (elem) (calls? name elem))))
         (else #f) ;; No, it's not recursive.
   )
)


(define (contains-named-let? t-src)
   (cond ;; ((not (pair? t-src)) #f)
         ((not (t-is-pair? t-src)) #f)
         ((and (> (t-length t-src) 3) ;; To avoid the case depicted above.
               (eq? (t-u-first t-src) 'let)
               (symbol? (t-u-second t-src))
          ) (t-u-second t-src) ;; Yes, found one, return the label used!
         )
         ((eq? (t-u-first t-src) 'quote) #f) ;; Not in this branch!
         ((there-exists? (t-elem t-src) contains-named-let?))
         (else #f) ;; No, it's not recursive.
   )
)



;; Currently just convert a tail-recursive call to a named let loop:
;; Note, the conversion should be idempotent, lest accidentally done twice.
(define-rewritten-form ((regular 0.1 draft) define src rwc extra)
  (let* ((name_et_fargs (t-second src))
         (tname (t-first name_et_fargs))
         (name (t-elem tname))
         (body (body-with-no-implicit-progn (cdr (t-rest src))
                                            rwc
                                            "define"
               )
         )
         (fargs (t-rest name_et_fargs))
         (copinits (map (lambda (s) (untyped (list s s))) fargs))
         (calls-itself? (calls? name body))
         (let-label-used (contains-named-let? body))
        )
     (cond ((eq? let-label-used name)
              ((rwc-error-printer rwc)
 "when expanding define: conflicting use of function's name as let label, please use another name: "
                      let-label-used
              )
           )
           (else
             (inherit-types-from-old src
               `(<define-expanded> ,name_et_fargs
                                  ,(if calls-itself?
                                      (inherit-types-from-old body
                                              `(<let-named> ,(untyped name)
                                                            ,(untyped copinits)
                                                            ,body
                                               )
                                      )
                                      body
                                   )
                )
             )
           )
     )
  )
)


;; It's essential that any type-annotations of both the variable-names
;; and the corresponding init-forms are copied to their respective places
;; in lambda-call's formal args and call-args. Accessors t-first and t-second
;; will do exactly that.

(define-rewritten-form ((regular 0.1 draft) let src rwc extra)
  (let ((vars_et_inits (t-elem (t-second src)))
        (lets-type (t-type (t-first src)))
       )
    (cond ((pair? vars_et_inits)
            (let ((bodyexpr
                    (body-with-no-implicit-progn (cdr (t-rest src)) rwc "let")
                  )
                  (fargs (untyped (map t-first vars_et_inits)))
                  (cargs (map t-second vars_et_inits))
;;                (_lambda (type-et-elem lets-type 'lambda)) ;; Umm...
                  (_lambda (untyped 'lambda)) ;; better...
                 )

;; Ordinary let is just a syntactic sugar for lambda-call:
              (inherit-types-from-old src
                  `((,_lambda ,fargs ,bodyexpr) ,@cargs)
              )
            )
          )
          (else ;; It's a named let, another beast altogether.
             (inherit-types-from-old src `(<let-named> ,@(t-rest src)))
          )
     )
  )
)


(define (create-lambda-wrapper what-kind-of-lambda fargs bodyexpr
                               lambdas-expansion-level)
   (type-et-elem type-default-unresolved
                 (list (type-et-elem lambdas-expansion-level                                                        what-kind-of-lambda
                       )
                       fargs
                       bodyexpr
                 )
   )
)


;; Experimental, telescoping named let (or unrolled loop, that is)
;; (i.e. a partial time-to-space conversion of tail-recursive loops,
;; or just partial loop unrolling, if you wish)
;; (sequential vs. combinational logic, some tradeoffs.)
(define-rewritten-form ((regular 0.1 draft) let-unrolled src rwc extra)
  (let* ((tel-factor (t-elem (arg-first src))) ;; telescoping, unrolling factor
         (looplabel (arg-u-second src))
         (vars_et_inits (arg-u-third src))
         (org-bodyexpr (body-with-no-implicit-progn (cdddr (t-rest src))
                                                    rwc "let-unrolled"
                       )
         )
         (fargs (untyped (map t-first vars_et_inits)))
        )
    (cond ((not (pair? vars_et_inits))
            ((rwc-error-printer rwc)
 "let-unrolled: vars & inits not a list: " src
            )
          )
          ((not (integer? tel-factor))
            ((rwc-error-printer rwc)
 "let-unrolled: currently the unrolling factor must be an integer: " src
            )
          )
;; This clause not needed, expansionloop below takes care of that also.
;;        ((zero? tel-factor) ;; Metamorphoses to ordinary named let,
                              ;; just drop the zero! For the completeness!
;;          (inherit-types-from-old src `(<let-named> ,@(cdr (t-rest src))))
;;        )
          (else
            (let expansionloop ((expcnt 1)
                                (body-expanded-n-times org-bodyexpr)
                               )
                (cond
                   ((> expcnt tel-factor)
                     (inherit-types-from-old src
                             `(<let-named>
                                  ,(t-third src)  ;; label
                                  ,(t-fourth src) ;; vars et inits
                                  ,body-expanded-n-times
                              )
                     )
                   )
                   (else (expansionloop
                           (1+ expcnt)
                           (expsynta-with-new-loopinfo-and-loop-unrolling-level
                                  body-expanded-n-times
                                  rwc
                                  extra
                                  looplabel
                                  (create-lambda-wrapper
                                       'lambda fargs org-bodyexpr
                                       (set-lambda-loop-unrolling-level expcnt)
                                  )
                                  expcnt
                           )
                         )
                   )
                )
            )
          )
     )
  )
)


;; Note! We MUST NOT blindly run the expansion a second time,
;; before the expansion level (rwc-loop-unrolling-level rwc) has been incremented
;; above the loop-unrolling-level-value found from the innermost enclosing expanded
;; lambda form (kept in extra):
;; (If extra is #f, then this is the topmost expansion of loop-call).
(define-rewritten-form ((regular 0.1 draft) <loop-tail-call> src rwc extra)
   (let* ((destlooplabel (t-u-first src))
          (loopinfo (cdr (assq destlooplabel (rwc-labels2loops rwc))))
;;        (fargs (t-u-second loopinfo)) ;; Later... in wirm-version.
          (cargs (t-carglist src))
         )

     (format #t
 "\n<loop-tail-call>, (rwc-unrolling-depth rwc)=~s, (rwc-loop-unrolling-level rwc)=~s, src="
             (rwc-unrolling-depth rwc) (rwc-loop-unrolling-level rwc)
     )
     (texp-print-with-nl src (current-output-port))
     (format #t "loopinfo=")
     (texp-print-with-nl loopinfo (current-output-port))
     (format #t "(rwc-names2deflocs_et_inits rwc)=")
     (texp-print-with-nl (rwc-names2deflocs_et_inits rwc) (current-output-port))

     (if (> (rwc-loop-unrolling-level rwc) (rwc-unrolling-depth rwc))
         (type-et-elem
            (t-type src)
            (cons loopinfo ;; i.e. lambda-wrapper constructed in above form.
                  cargs
            )
         )
         src ;; Otherwise, keep the src same so far.
     )
   )
)

;; (let* ((var1 <init1expr>)
;;        (var2 <init2expr>)
;;        (var3 <init3expr>)
;;       )
;;   body
;; )
;;
;; -->
;;
;; (let ((var1 <init1expr>))
;;    (let* ((var2 <init2expr>)
;;           (var3 <init3expr>)
;;          )
;;       body
;;    )
;; )
;;


(define-rewritten-form ((regular 0.1 draft) let* src rwc extra)
  (cond
        (else
          (let* ((vars_et_inits_typed (t-second src))
                 (vars_et_inits (t-elem vars_et_inits_typed))
                 (bodyexpr (body-with-no-implicit-progn (cdr (t-rest src))
                                                        rwc
                                                        "let*"
                           )
                 )
                )
            (cond ((symbol? vars_et_inits) ;; Oops, labeled let*?
                     ((rwc-error-printer rwc)
 "expand-let*: let* does not allow labels: " src
                     )
                  )
                  (else
                     (if (or (null? vars_et_inits)
                             (null? (cdr vars_et_inits))
                         )
;; vars_et_inits of length 1 or ():
                         (inherit-types-from-old
                                src
                               `(let ,vars_et_inits_typed ,bodyexpr)
                         )

;; Else more than one initialization, so let's expand once:
                         (inherit-types-from-old
                              src
                              `(let ,(untyped `(,(first vars_et_inits)))
                                    ,(inherit-types-from-old
                                        src
                                        `(let* ,(t-rest-with-same-type
                                                          vars_et_inits_typed
                                                )
                                               ,bodyexpr
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
)



;;
;; (IFZ a b c) --> (if (zero? a) b c)

(define-rewritten-form ((regular 0.1 draft) ifz src rwc extra)
  (cond ((not (= 4 (t-length src)))
           ((rwc-error-printer rwc)
 "expand-ifz: Form must have exactly three subforms: " src
           )
        )
        (else
           (let ((_zero? (untyped 'zero?)))
              (inherit-types-from-old src
                 `(if ,(type-et-elem type-boolean `(,_zero? ,(t-second src)))
                      ,(t-third src)
                      ,(t-fourth src)
                  )
              )
           )
        )
  )
)

;; (IFNZ a b c) --> (if (nonzero? a) b c)

(define-rewritten-form ((regular 0.1 draft) ifnz src rwc extra)
  (cond ((not (= 4 (t-length src)))
           ((rwc-error-printer rwc)
 "expand-ifnz: Form must have exactly three subforms: " src
           )
        )
        (else
           (let ((_nonzero? (untyped 'nonzero?)))
              (inherit-types-from-old src
                `(if ,(type-et-elem type-boolean `(,_nonzero? ,(t-second src)))
                     ,(t-third src)
                     ,(t-fourth src)
                 )
              )
           )
        )
  )
)


;; With this kind of rewriting, we can write code where the last
;; branch of or-form is a tail-recursive call:
(define-rewritten-form ((regular 0.1 draft) or src rwc extra)
  (cond
        ((t-length1? src) ;; XXX - Think! (or) in Scheme returns #f.
           ((rwc-error-printer rwc)
 "expand-or: Form must have at least one argument: " src
           )
        )
        (else
           (let ((first_expr (t-second src))
                 (rest (cdr (t-rest src)))
                )
             (cond ((null? rest) first_expr) ;; Rewrite (OR x) --> x

;; Otherwise rewrite (or expr1 ...) as (cond (expr1) (else (or ...)))

                   (else
                     (let ((_else (untyped 'else))
                           (_or (untyped 'or))
                          )

                       (inherit-types-from-old src
                          `(cond ,(untyped `(,first_expr))
                                 ,(untyped
                                     `(,_else ,(untyped `(,_or ,@rest)))
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




;; (cond (cond1 res1) (cond2 res2) (cond3 res3) (else elseres))
;; --> (if cond1 res1 (cond (cond2 res2) (cond3 res3) (else elseres)))
;; (cond (cond1 res1) (else elseres))
;; --> (if cond1 res1 (cond (else elseres)))
;; --> (if cond1 res1 elseres)
;;
;; (cond (cond1 res1)
;; --> (if cond1 res1) ;; Not allowed, muddy semantics!
;; (cond (else elseres))
;; --> elseres

;; (cond (cond1expr) rest)
;; --> (let ((cond1v cond1expr) (if cond1v cond1v (cond rest))))

;; (cond (cond1expr => fun) rest)
;; --> (let ((cond1v cond1expr) (if cond1v (fun cond1v) (cond rest))))

;; Note: fun can be also a (lambda (x) ...)

(define-rewritten-form ((regular 0.1 draft) cond src rwc extra)
  (cond ((t-length1? src)
           ((rwc-error-printer rwc)
 "expand-cond: Form must have at least one clause: " src
           )
        )
        (else
          (let ((first-clause (t-second src))
                (rest-of-clauses (cdr (t-rest src)))
               )
             (cond
                   ((null? rest-of-clauses) ;; Last clause?
                      (cond ((eq? (t-u-first first-clause) 'else)
                              (let ((else-expr (body-with-no-implicit-progn
                                                      (t-rest first-clause)
                                                      rwc
                                                      "cond"
                                               )
                                   ))
;; Rewriting (cond (else elseres)) --> elseres

                                  else-expr
                              )
                            )
                            (else
                              ((rwc-error-printer rwc)
 "expand-cond: The last clause of cond must be else!: " src
                              )
                            )
                      )
                   )
  
;; Rewriting (cond (cond1expr) rest)
;; --> (let ((cond1v cond1expr) (if cond1v cond1v (cond rest))))

;; XXX -- We inherit the type of cond1expr to cond1v.
;; XXX -- It should have been resolved already here as a boolean,
;; XXX -- and if not, an error should be generated:
                   ((t-length1? first-clause)
                      (let ((new-var-name
                               (type-et-elem
                                  (t-type (t-first first-clause))
                                  (generate-new-var-name 'condexpr)
                               )
                           ))
                        (inherit-types-from-old src
                          `(let
                             ,(untyped
                               `(,(untyped
                                   `(,new-var-name ,(t-first first-clause))
                                  )
                                )
                              )
                              ,(inherit-types-from-old src
                                   `(if ,new-var-name
                                        ,new-var-name
                                        ,(inherit-types-from-old src
                                                  `(cond ,@rest-of-clauses)
                                         )
                                    )
                               )
                           )
                        )
                      )
                   )
 
;; Rewriting (cond (cond1expr => fun) rest)
;; --> (let ((cond1v cond1expr) (if cond1v (fun cond1v) (cond rest))))

                   ((and (= 3 (t-length first-clause))
                         (eq? '=> (t-u-second first-clause))
                    )
                      (let ((testexpr (t-first first-clause))
                            (fun-or-lambda (body-with-no-implicit-progn
                                              (cdr (t-rest first-clause))
                                              rwc
                                              "cond"
                                           )
                            )
                            (new-var-name
                               (type-et-elem
                                  (t-type (t-first first-clause))
                                  (generate-new-var-name 'condexpr)
                               )
                            )
                           )

                        (inherit-types-from-old src
                          `(let
                             ,(untyped
                               `(,(untyped `(,new-var-name ,testexpr)))
                              )
                              ,(inherit-types-from-old src
                                   `(if ,new-var-name
                                        ,(untyped
                                            `(,fun-or-lambda ,new-var-name)
                                         )
                                        ,(inherit-types-from-old src
                                                  `(cond ,@rest-of-clauses)
                                         )
                                    )
                               )
                           )
                        )
                      )
                   )
  
;; Rewriting (cond (cond1 res1) rest-of-clauses)
;; --> (if cond1 res1 (cond rest-of-clauses))

                   (else ;; An ordinary cond-clause.
                     (let ((test-expr (t-first first-clause))
                           (then-expr (body-with-no-implicit-progn
                                             (t-rest first-clause)
                                             rwc
                                             "cond"
                                      )
                           )
                          )
                        (inherit-types-from-old src
                          `(if ,test-expr
                               ,then-expr
                               ,(inherit-types-from-old src
                                                 `(cond ,@rest-of-clauses)
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

