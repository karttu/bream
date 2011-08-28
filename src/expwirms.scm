
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;; BREAM / expwirms.scm                                                 ;;
;;   --  Functions for expanding various Bream-forms                    ;;
;;       as used in wirm-macros.                                        ;;
;;                                                                      ;;
;;       This module is invoked with expwirms-in-place! call            ;;
;;       from function typeresolve-until-no-unexpanded-wirms-remain!    ;;
;;       (in module typreslv.scm) for each yet to be expanded call      ;;
;;       to a wirm-macro still present in source-code.                  ;;
;;       The entry point is (expwirms-in-place! toc src)                ;;
;;       which will return an expanded copy of src, physically          ;;
;;       substituted into the same texp-structure.                      ;;
;;                                                                      ;;
;; NOTE: Currently this shares the rwc-structure and dispatching        ;;
;;       machinery (expsynta-rewrite) with expsynta.scm module.         ;;
;;       Also, forms which are not given here, are effectively          ;;
;;       inherited from the ordinary rewriting-forms specified in       ;;
;;       expsynta.scm                                                   ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Unless otherwise mentioned, all the files in this code tree are
;; Copyright (C) 2010-2011 Antti Karttunen, subject to the terms of the GPL v2.
;; See the file COPYING for more information.  Contact the author for
;; further developments at the address: <his-firstname>.<his-surname>@gmail.com
;;

;;
;; Antti Karttunen ("karttu") spawned this module from expsynta.scm
;; module at Jul 27 2011.
;;
;;

;; Edited    Aug 23 2011 by karttu.
;;   Tried to change a call to wex-expwirm-and-typeresolve! to
;;   wex-expwirm-and-typeresolve-only-if-needed!
;;   in handlers for w, w-1 and w+1.
;;   This will (most probably) break the code which calls
;;   these forms via wirm-macros (like ww, ww-1, ww+1)
;;   which are situated as the second (or third) args
;;   of drop and bits. (See the change comment for the same date
;;   in typreslv.scm).
;;   (Yes it does, so I undid that change.)
;;
;;   In any case, the current interplay between typreslv.scm and
;;   expwirms.scm is currently a result of just code-and-see-what-comes.
;;   This needs a clear-headed redesign.
;;
;;
;; Edited    Aug 24 2011 by karttu.
;;   Added to all-int-args? a call to new function
;;   t-is-integer-or-has-been-bound-to-one?
;;   for testing some new experimental wirm-macros
;;   with compile-time variable resolving.
;;



(define (expwirms-in-place! toc old-src wirm-depth)
   (texp-overwrite-in-place! old-src (expwirms toc old-src wirm-depth))
)


(define (expwirms toc src wirm-depth)
  (let* ((rwc (toc-make-fresh-rwc toc 
                          (toc-wirm-rewriting-convention-in-use toc)
                          wirm-depth
                          (t-type src)
             )
         )
         (dummy1 ((rwc-logging-printer2 rwc) rwc 2
                    (format #f
 "Entering expwirms, with wirm-depth=~s, SRC="
                            (rwc-wirm-depth rwc)
                    )
                    src
                 )
         )
         (result (expsynta-rewrite src rwc #f))
         (dummy2 ((rwc-logging-printer2 rwc) rwc 2
                    (format #f
 "Returning from expwirms, with wirm-depth=~s, SRC=~a, RESULT="
                            (rwc-wirm-depth rwc)
                            (texp-sprint src)
                    )
                    result
                 )
         )
        )
     result
  )
)



;; For wirms, the processing is more involved:
(define-rewritten-form ((wirm 0.1 draft) <generic-funcall> src rwc extra)
  (let ((cached-fundef (t-callees-fundef src)))
    (begin
      (if (and cached-fundef ;; A wirm-macro needing an expansion?
               (eq? 'define-wirm (get-deftype-from-fundef cached-fundef))
          )
          (let* ((fargs (untyped (t-rest (t-second cached-fundef))))
                 (fundef-body (body-with-no-implicit-progn
                                                  (cdr (t-rest cached-fundef))
                                                  rwc
                                                  "wirm body"
                              )
                 )
                 (expcnt (1+ (rwc-wirm-depth rwc)))
                 (dummy1 (if (> expcnt (rwc-max-wirm-depth rwc))
                             (begin
                               ((rwc-logging-printer2 rwc) rwc 0
                                 (format #f
 "Runaway macro expansion? (wirm-depth=~s) Check the terminating condition of the recursion for the SRC="
                                         (rwc-wirm-depth rwc)
                                 )
                                 src
                               )
                               ((toc-toplevel-exit (rwc-parent-toc rwc)) #f)
                             )
                         )
                 )
;; Note that we discard silently the old cached-fundef here, by replacing
;; (with cons) the head of the src-list with the lambda-form.
;; However, we shouldn't lose the original resolved type of this calling
;; form (t-type src), as it will be stored into (rwc-closest-et rwc)
;; by the rewriting-handler for (lambda) (in expsynta.scm):
                 (result (expsynta-only-cargs!
                            (type-et-elem
                               (t-type src) ;; Don't lose this one!
                               (cons (create-lambda-wrapper
                                       'lambda fargs fundef-body
                                       (set-lambda-wirm-expansion-level expcnt)
                                     )
                                     (t-carglist src)
                               )
                            )
                            rwc
                            extra
                         )
                 )
                 (dummy2 ((rwc-logging-printer2 rwc) rwc 3
                             (format #f
 "In <generic-funcall> wirm-depth=~s, SRC=~a, RESULT="
                                     (rwc-wirm-depth rwc)
                                     (texp-sprint src)
                             )
                             result
                        )
                 )
                )
             result
          )
;; Else:
          (expsynta-rewrite-functor-and-cargs-of-funcall src rwc extra)
      )
;;    (format #t "Calling rewriting-form for <generic-funcall> with src=\n")
;;    (texp-print-with-nl src (current-output-port))
    )
  )
)

;; Calls to typeresolve! in typreslv.scm through this local function:
(define (wex-typeresolve! rwc src etype)
   (typeresolve! (rwc-parent-toc rwc)
                 src
                 (rwc-names2deflocs_et_inits rwc)
                 (rwc-wirm-depth rwc)
                 etype
   )
)

(define (wex-typeresolve-and-expwirm! src rwc extra etype) ;; XXX - Not called.
  (let ((src-typeresolved (wex-typeresolve! rwc src etype)))
     (expsynta-rewrite src-typeresolved rwc extra)
  )
)


(define (wex-expwirm-and-typeresolve! src rwc extra etype)
  (let ((src-expanded (expsynta-rewrite src rwc extra)))
     (wex-typeresolve! rwc src-expanded etype)
  )
)


(define (wex-expwirm-and-typeresolve-only-if-needed! src rwc extra etype)
  (let ((src-expanded (expsynta-rewrite src rwc extra)))
     (if (type-completely-unresolved? src-expanded)
         (wex-typeresolve! rwc src-expanded etype)
         src-expanded
     )
  )
)

(define (wex-integer-literal wex n)
    (type-et-elem
         (type-i-of-atleast-width (nonnegatives_least_binwidth n))
         n
    )
)

;; Do some error checking, that b indeed is either #f or #t
(define (wex-boolean-literal wex b)
    (type-et-elem
         type-boolean
         b
    )
)



;; Concatenation:
;;          conc ;; { } (cat in Verilisp) One or more arguments.
;;          conc2

;; Subrange of bits:
;;          bits ;; ternary: (bits binstr high_inclusive low_inclusive)
;; both high_ and low_inclusive must resolve to integers at compile-time.

;; One bit:
;;          bit ;; Dyadic. The second arg does not need to be constant in Verilog

;; Reference a vector.
;;          ref ;; Dyadic. The second arg does not need to be constant in Verilog
;; Note: both (bit x n) and (ref x n) compile to Verilog-form x[n].

;;        \\      ;; From Handel-C: (\\ x n) = (bits x (-1+ (WIDTH x)) n)
;;                   so it's not exactly same as (>> x n). Used in recursive
;;                   macros. We should have WIDTH-1 for idiom (-1+ (WIDTH ..))



;; Returns the integer texp is or the one it is bound to, and #f in other cases
(define (t-is-integer-or-has-been-bound-to-one? rwc texp)
   (cond ((t-is-integer? texp) texp)
         ((and (t-is-symbol? texp) (rwc-find-varnames-def rwc texp))
           =>
           (lambda (vardef) (and (t-is-integer? vardef) vardef))
         )
         (else #f)
   )
)


;; If all args of src are integer literals, return a non-typed list
;; of those integers, otherwise return #f (if there was even one
;; non-integer argument (e.g. a non-optimizable function call)).

(define (all-int-args? rwc src)
   (let loop ((rev-args (list)) (args (t-carglist src)))
       (cond ((null? args) (reverse! rev-args))
             ((t-is-integer-or-has-been-bound-to-one? rwc (car args))
               =>
                (lambda (in)
                  (loop (cons (t-elem in) rev-args)
                        (cdr args)
                  )
                )
             )
             (else #f)
      )
  )
)


(define (all-boolean-args? rwc src)
   (let loop ((rev-args (list)) (args (t-carglist src)))
       (cond ((null? args) (reverse! rev-args))
             ((t-is-boolean? (car args))
                (loop (cons (t-elem (car args)) rev-args)
                      (cdr args)
                )
             )
             (else #f)
      )
  )
)


(define (apply-ifun-to-iargs fun src rwc extra)
   (let ((srcexp (expsynta-only-cargs! src rwc extra)))
      (cond ((all-int-args? rwc srcexp)
              =>
              (lambda (uia) (wex-integer-literal rwc (apply fun uia)))
            )
            (else srcexp) ;; Cannot simplify, return as it is.
      )
   )
)


(define (apply-bfun-to-iargs fun src rwc extra)
   (let ((srcexp (expsynta-only-cargs! src rwc extra)))
      (cond ((all-int-args? rwc srcexp)
              =>
              (lambda (uia) (wex-boolean-literal rwc (apply fun uia)))
            )
            (else srcexp) ;; Cannot simplify, return as it is.
      )
   )
)

(define (apply-bfun-to-bargs fun src rwc extra)
   (let ((srcexp (expsynta-only-cargs! src rwc extra)))
      (cond ((all-boolean-args? rwc srcexp)
              =>
              (lambda (uia) (wex-boolean-literal rwc (apply fun uia)))
            )
            (else srcexp) ;; Cannot simplify, return as it is.
      )
   )
)


(define apply-ifun-to-1-iarg  apply-ifun-to-iargs) ;; XXX -- Add check later!
(define apply-ifun-to-2-iargs apply-ifun-to-iargs) ;; XXX -- Add check later!

(define apply-bfun-to-1-iarg  apply-bfun-to-iargs) ;; XXX -- Add check later!
(define apply-bfun-to-2-iargs apply-bfun-to-iargs) ;; XXX -- Add check later!

(define apply-bfun-to-1-barg  apply-bfun-to-bargs) ;; XXX -- Add check later!


(define-rewritten-form ((wirm 0.1 draft) + src rwc extra) (apply-ifun-to-iargs + src rwc extra))
(define-rewritten-form ((wirm 0.1 draft) * src rwc extra) (apply-ifun-to-iargs * src rwc extra))
(define-rewritten-form ((wirm 0.1 draft) - src rwc extra) (apply-ifun-to-iargs - src rwc extra))

(define-rewritten-form ((wirm 0.1 draft) 1+ src rwc extra) (apply-ifun-to-1-iarg 1+ src rwc extra))

(define-rewritten-form ((wirm 0.1 draft) -1+ src rwc extra) (apply-ifun-to-1-iarg -1+ src rwc extra))


(define-rewritten-form ((wirm 0.1 draft) asl src rwc extra) (apply-ifun-to-2-iargs <<< src rwc extra))

(define-rewritten-form ((wirm 0.1 draft) <<< src rwc extra) (apply-ifun-to-2-iargs <<< src rwc extra))

(define-rewritten-form ((wirm 0.1 draft) asr src rwc extra) (apply-ifun-to-2-iargs >>> src rwc extra))

(define-rewritten-form ((wirm 0.1 draft) >>> src rwc extra) (apply-ifun-to-2-iargs >>> src rwc extra))

(define-rewritten-form ((wirm 0.1 draft) shl src rwc extra) (apply-ifun-to-2-iargs << src rwc extra))

(define-rewritten-form ((wirm 0.1 draft) << src rwc extra) (apply-ifun-to-2-iargs << src rwc extra))

(define-rewritten-form ((wirm 0.1 draft) shr src rwc extra) (apply-ifun-to-2-iargs >> src rwc extra))

(define-rewritten-form ((wirm 0.1 draft) >> src rwc extra) (apply-ifun-to-2-iargs >> src rwc extra))

(define-rewritten-form ((wirm 0.1 draft) bit src rwc extra) (apply-ifun-to-2-iargs bit src rwc extra))

(define-rewritten-form ((wirm 0.1 draft) bitnot src rwc extra) (apply-ifun-to-1-iarg bitnot src rwc extra))

(define-rewritten-form ((wirm 0.1 draft) bitand src rwc extra) (apply-ifun-to-iargs bitand src rwc extra))
(define-rewritten-form ((wirm 0.1 draft) bitor src rwc extra) (apply-ifun-to-iargs bitor src rwc extra))
(define-rewritten-form ((wirm 0.1 draft) bitxor src rwc extra) (apply-ifun-to-iargs bitxor src rwc extra))
(define-rewritten-form ((wirm 0.1 draft) bitxnor src rwc extra) (apply-ifun-to-iargs bitxnor src rwc extra))

(define-rewritten-form ((wirm 0.1 draft) redand src rwc extra) (apply-ifun-to-1-iarg redand src rwc extra))
(define-rewritten-form ((wirm 0.1 draft) rednand src rwc extra) (apply-ifun-to-1-iarg rednand src rwc extra))
(define-rewritten-form ((wirm 0.1 draft) redor src rwc extra) (apply-ifun-to-1-iarg redor src rwc extra))
(define-rewritten-form ((wirm 0.1 draft) rednor src rwc extra) (apply-ifun-to-1-iarg rednor src rwc extra))
(define-rewritten-form ((wirm 0.1 draft) redxor src rwc extra) (apply-ifun-to-1-iarg redxor src rwc extra))
(define-rewritten-form ((wirm 0.1 draft) redxnor src rwc extra) (apply-ifun-to-1-iarg redxnor src rwc extra))

(define-rewritten-form ((wirm 0.1 draft) < src rwc extra) (apply-bfun-to-iargs < src rwc extra))
(define-rewritten-form ((wirm 0.1 draft) > src rwc extra) (apply-bfun-to-iargs > src rwc extra))
(define-rewritten-form ((wirm 0.1 draft) <= src rwc extra) (apply-bfun-to-iargs <= src rwc extra))
(define-rewritten-form ((wirm 0.1 draft) >= src rwc extra) (apply-bfun-to-iargs >= src rwc extra))

(define-rewritten-form ((wirm 0.1 draft) = src rwc extra) (apply-bfun-to-iargs = src rwc extra))

(define-rewritten-form ((wirm 0.1 draft) != src rwc extra)
   (apply-bfun-to-2-iargs (lambda (x y) (not (= x y))) src rwc extra)
)

(define-rewritten-form ((wirm 0.1 draft) zero? src rwc extra) (apply-bfun-to-1-iarg zero? src rwc extra))
(define-rewritten-form ((wirm 0.1 draft) nonzero? src rwc extra) (apply-bfun-to-1-iarg nonzero? src rwc extra))

(define-rewritten-form ((wirm 0.1 draft) even? src rwc extra) (apply-bfun-to-1-iarg even? src rwc extra))
(define-rewritten-form ((wirm 0.1 draft) odd? src rwc extra) (apply-bfun-to-1-iarg odd? src rwc extra))
(define-rewritten-form ((wirm 0.1 draft) pow2? src rwc extra) (apply-bfun-to-1-iarg pow2? src rwc extra))


(define-rewritten-form ((wirm 0.1 draft) logand src rwc extra) (apply-bfun-to-bargs logand src rwc extra))
(define-rewritten-form ((wirm 0.1 draft) logor src rwc extra) (apply-bfun-to-bargs logor src rwc extra))
(define-rewritten-form ((wirm 0.1 draft) lognot src rwc extra) (apply-bfun-to-1-barg lognot src rwc extra))



(define-rewritten-form ((wirm 0.1 draft) ew src rwc extra)
   (let* ((enclosing-width (type-width-of (rwc-closest-et rwc))))
     (wex-integer-literal rwc enclosing-width)
   )
)


(define-rewritten-form ((wirm 0.1 draft) w src rwc extra)
   (let* ((arg1resolved
               (wex-expwirm-and-typeresolve!
                                             (arg-first src) rwc extra
                                             type-default-unresolved
               )
          )
          (arg1width (t-width arg1resolved))
         )
     (wex-integer-literal rwc arg1width)
   )
)

;; XXX -- Implement these two w-1 and w+1 in more elegant manner: (rewrite!)
(define-rewritten-form ((wirm 0.1 draft) w-1 src rwc extra)
   (let* ((arg1resolved
               (wex-expwirm-and-typeresolve!
                                             (arg-first src) rwc extra
                                             type-default-unresolved
               )
          )
          (arg1width (t-width arg1resolved))
         )
     (wex-integer-literal rwc (-1+ arg1width))
   )
)


(define-rewritten-form ((wirm 0.1 draft) w+1 src rwc extra)
   (let* ((arg1resolved
               (wex-expwirm-and-typeresolve!
                                             (arg-first src) rwc extra
                                             type-default-unresolved
               )
          )
          (arg1width (t-width arg1resolved))
         )
     (wex-integer-literal rwc (1+ arg1width))
   )
)


;; "If" is very special in wirm-mode. For the starters, we can't run
;; the type-resolver indiscriminately to whole form,
;; as then we might accidentally generate out-of-bounds indices
;; in the else-branch, in case the test-branch evaluates to #t
;; and only the then-branch (usually the terminating branch for recursive
;; macros) is intended to be considered.
;; So we do type-resolving here piecewise, first for test-branch,
;; and then, depending on its result, handle ONLY the either one
;; of then or else-branches.
;; We should make sure that the etype of the whole if propagates
;; there, as here we are doing some of the work of typeresolver
;; as well. (And to the other direction?)

;; In what order we should do the typeresolving and expansion?
;; It would be natural to first typeresolve, and only after that
;; expand. The type-resolve will expand any further recursive
;; invocations of wirm-macros (usually in the else-branch)
;; in deepest first order.

;; XXX --- Now we just make an assumption that this is the topmost form in
;;         wirm-macro, so that we can pass (rwc-closest-et rwc)
;;         as the expected type of then and else-branches.
;;         On the other hand, we need to expand things like conc to conc2
;;         before type-resolver can recognize them correctly.
;;         On the other hand, we could ordinary expsynta for wirm-macro
;;         bodies at the time they are first encountered, like
;;         also the ordinary functions, as done by
;;         trc-splice-fundef-in-if-not-already! on behalf of
;;         typeresolver-handler of <generic-funcall>.
;;

(define-rewritten-form ((wirm 0.1 draft) if src rwc extra) ;; XXX - not ready
  (cond ((not (= 4 (t-length src)))
           ((rwc-error-printer rwc)
 "if must have exactly three subforms: " src
           )
        )
        (else
           (let* ((test_expr1 (wex-typeresolve! rwc (arg-first src)
                                                type-boolean
                              )
                  )
                  (test_expr (expsynta-rewrite test_expr1 rwc extra))
                 )
;; If test_expr didn't simplify to a literal boolean value,
;; we return the whole if-form back as it is:
             (cond ((not (t-is-boolean? test_expr)) src)
                   ((t-elem test_expr) ;; test_expr is #t, choose then-branch
                       (wex-expwirm-and-typeresolve! (arg-second src) rwc extra
                                                     (rwc-closest-et rwc)
                       )
                   )
                   (else
                       (wex-expwirm-and-typeresolve! (arg-third src) rwc extra
                                                     (rwc-closest-et rwc)
                       )
                   )
             )
           )
        )
  )
)

