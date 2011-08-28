
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                 ;;
;; BREAM / typreslv.scm                                            ;;
;;   --  Functions for resolving the type (i.e. currently: width)  ;;
;;       of combinational functions.                               ;;
;;                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Unless otherwise mentioned, all the files in this code tree are
;; Copyright (C) 2010-2011 Antti Karttunen, subject to the terms of the GPL v2.
;; See the file COPYING for more information.  Contact the author for
;; further developments at the address: <his-firstname>.<his-surname>@gmail.com
;;

;;
;; Antti Karttunen ("karttu") started writing this module at Nov 04 2010.
;;

;; Edited    Nov 20 2010 by karttu
;;   Renamed from typrcomb.scm to typreslv.scm.
;;
;; Edited Nov 21-22 2010 by karttu
;;   Starting to get some shape.
;;   A lots of thinking done concerning resolving of <generic-funcall>s
;;   and everything else.
;;
;; Edited    May 14 2011 by karttu.
;;   Back to the work, after six months!
;;   Started editing the handler of <generic-funcall> to use the new accessors
;;   t-callees-fundef and set-callees-fundef!
;;   (Discarding that "a lots of thinking done" last November...)
;;   to physically splice a fresh copy of the needed body
;;   to an appropriate "cache".
;;
;; Edited    May 16 2011 by karttu.
;;   Changing all coc's to trc's ("type resolving context"),
;;   so that this module would be (almost) wholly independent
;;   from the compilation module proper (compile1.scm)
;;   Created a new structure trc, with many of the same or similar
;;   elements as coc in compile1, but with some important differences.
;;
;; Edited May 19-22 2011 by karttu.
;;   Rudimentary implementations of type-resolvings for lambda forms,
;;   external function calls, named lets and loop-tail-calls.
;;   (Not tested at all yet.)
;;
;; Edited May 25-27 2011 by karttu.
;;   The first testable implementation of type-resolver.
;;

;;
;; Edited    May 29 2011 by karttu.
;;   Changed the type system so, that currently it doesn't use
;;   the looping-bit at all. (It's nowhere set, although there
;;   is some dormant code-remnants that check for its presence,
;;   or toggle it off). I.e. <loop-tail-call> now just gives back
;;   its context etype instead of type-certainly-loops.
;;   The compiling module will not do anything (at least currently)
;;   with the type/width-field of loop-tail-calls, so it doesn't
;;   cause any harm there, and here the sibling branches of any 'if'
;;   are happy with their looping sister, as they will think it is
;;   of the same type as they (and the if's result as well).
;;   Also created resolver-handler <generic-shift> for shifts.
;;
;;   Added also the type-resolver for conc2.
;;
;; Edited May 29-30 2011 by karttu.
;;   Added type-resolvers for bit and bits, and also an
;;   experimental type-resolver for the "drop n bits" form: (\\ x n)
;;

;; Edited    May 31 2011 by karttu.
;;   Removed the spurious extra definition for shl, which used
;;   generic type-resolver <in1bit-etype-with-one-anywidth-arg>
;;

;; Edited    Jun 02 2011 by karttu.
;;   Now funinfo-structure is also cached in addition to expsynta'ed
;;   copy of the fundef itself. Check for extfun declarations.
;;   Function trc-splice-fundef-in-if-not-already! and <generic-funcall>
;;   respectively changed.
;;
;;   Later that day:
;;   Added rudimentary type-resolvers for w, w-1 and w+1.
;;   Also for +c, par2 and seq2.

;; Edited Jul 22-27 2011 by karttu.
;;   Beginning to add functionality for wirm-expansion.
;;

;; Edited    Jul 29 2011 by karttu.
;;   Added the type-resolver <etype-atleast-as-wide-as-the-arg>
;;   for the wirm-macros SXT and ZXT.
;;

;; Edited    Aug 11 2011 by karttu.
;;   \\ renamed to drop. So (\\ x n) is now (drop x n)


;; Edited    Aug 20 2011 by karttu.
;;   Added make-list-of-toplevel-initial-type-bindings for the needs
;;   of new typeresolve-toplevel-call! (with an extra argument).

;; Edited    Aug 23 2011 by karttu.
;;   Added for comb. function '*' the type-resolver <equitype-etype-and-args> 
;;   (This might be a temporary solution.)
;;
;;   Now type-resolvers for bits and drop allow also wirm-calls
;;   as their second (and third) arguments, expecting that they
;;   will be resolved to integers. Not yet a final implementation.
;;   (See the comments at the end of resolver for drop).
;;   
;;

;; Edited    Aug 26 2011 by karttu.
;;   Added type-resolver <etype-one-narrower-than-arg> for the needs
;;   of new wirm-macro zeqw-1.
;;   Also an experimental type-resolver <etype-sum-of-args> for the needs
;;   of *c. (Well, it was buggy, now in limbo. *c2 uses now the same
;;   handler <etype-sum-of-2-args> as conc2.)
;;
;; 

;; Type resolving schemes should consist of two components,
;; the strictness, like: 'cavalier', 'latitudian', 'lenient' or 'retentive'
;; and effort, how hard we try to resolve, e.g. should
;; backtracking be used, etc.

(define *typeresolving-forms* (list (list '(regular 0.1 draft) (list (list (list))))))


(define (trc-list-of-typeresolving-forms-in-use trc)
  (list-of-forms-by-dic (trc-resolving-convention-in-use trc)
                        (trc-typeresolving-forms trc)
  )
)


(define-syntax define-typeresolving-form
   (syntax-rules ()
     ((define-typeresolving-form (CACO formname src trc etype) body ...)
        (attach!
             (cons (quote formname) (lambda (src trc etype) body ...))
             (list-of-forms-by-dic (quote CACO) *typeresolving-forms*)
        )
     )
   )
)




;; Not this:
;; (define (resolve-expr-type src trc etype)
;;    (generic-dispatch (t-elem src)
;; 
;;                      src
;; 
;;                      (trc-list-of-typeresolving-forms-in-use trc)
;; 
;;                      (lambda (s) (t-elem (car s)))
;; 
;;                      (lambda (x) (set-texp-type! src x))
;; 
;;                      (lambda (src . rest) #f)
;; 
;;                      (lambda (src . rest) #f)
;; 
;;                      trc etype
;;    )
;; )
;; 

;; Have to do this by indirection, so that the changes are not lost when we
;; return resolving from some of the binding constructs like
;; lambda or named let:


(define (incr-otherchanges-count! trc)
  (let ((ctrp (trc-n_otherchanges trc))) ;; Is a dotted pair.
    (set-car! ctrp (+ 1 (car ctrp))) ;; Whose car contains the count.
    (car ctrp)
  )
)

(define (incr-mismatches-count! trc)
  (let ((ctrp (trc-n_mismatches trc))) ;; Is a dotted pair.
    (set-car! ctrp (+ 1 (car ctrp))) ;; Whose car contains the count.
    (car ctrp)
  )
)

(define (incr-resolvings-count! trc)
  (let ((ctrp (trc-n_resolvings trc))) ;; Is a dotted pair.
    (set-car! ctrp (+ 1 (car ctrp))) ;; Whose car contains the count.
    (car ctrp)
  )
)


;; etype = expected type
;; Note that if etype is #f (i.e. we don't know the expected type)
;; and src doesn't resolve to any type either, then this returns #f as well.
;; This is different from a type mismatch, in which case we do
;; a non-local exit with (trc-error-printer):

;; We should check the possibly cached type from (t-type src)
;; (However, for named let registers and for lambda-form formal-arg defs,
;; that shouldn't bypass storing of the (var-name . type) pair to ...)

;; Also with generic-funcalls, even if the result type is given
;; explicitly, we still might/need to do type-resolving for that
;; function's body.

;; Only when both the function's (wheher combinational or non-combinational)
;; restype has been annotated (to (t-type src-of-call)) as well as
;; all of its call-arguments (really necessary?)
;; we can skip further resolving below that? (Really???)

;; But in any case, we probably need to do multiple resolvings
;; until a fixed point is reached. (We should also check when
;; there will be no more changes to a list of variables that have
;; been resolved. The variables have quirky, non-local effects...)

;; What might be the worst-case performance of this kind of naive
;; resolving algorithm? (Quadratic, exponential??? Should we care?
;; We can implement faster algorithm later.)
;; Does this converge? Has confluence?



(define (trc-too-many-passes-error trc src)
  (begin
     ((trc-logging-printer2 trc) trc 0
         (format #f
 "TOO MANY TYPE RESOLVING PASSES (~s) (max=~s)! Please check for circular (runaway) type dependencies in your code, or raise the maximum limit for passes."
                  (toc-typeresolving-passes-made (trc-parent-toc trc))
                  (toc-typeresolving-max-passes  (trc-parent-toc trc))
         )
         src
     )
     ((trc-typeresolve-exit trc) #f) ;; Throw back #f as we failed now.
  )
)



(define (trc-type-mismatch-error trc src etype)
  (begin
     ((trc-warning-printer trc)
         (format #f
"TYPE MISMATCH: expected type ~s (~s) doesn't match with type of src ~s (~s): "
                (type-bin->ext etype) etype
                (type-bin->ext (texp-type src)) (texp-type src)
         )
     )
     (texp-print-with-nl src (trc-logging-port trc))
     ((trc-typeresolve-exit trc) #f) ;; Throw back #f as we failed now.
  )
)


(define (trc-type-mismatching-sum-error trc src t1 t2 etype)
  (begin
     ((trc-logging-printer2 trc) trc 0
         (format #f
"TYPE MISMATCH: expected type ~s (~s) doesn't sum to the widths of types ~s (~s) and ~s (~s) of src: "
                (type-bin->ext etype) etype
                (type-bin->ext t1) t1
                (type-bin->ext t2) t2
         )
         src
     )
     ((trc-typeresolve-exit trc) #f) ;; Throw back #f as we failed now.
  )
)


(define (trc-type-nospace-for-carry-error trc src etype)
  (begin
     ((trc-logging-printer2 trc) trc 0
         (format #f
"TYPE MISMATCH: expected type ~s (~s) has no space for carry bit! src: "
                (type-bin->ext etype) etype
         )
         src
     )
     ((trc-typeresolve-exit trc) #f) ;; Throw back #f as we failed now.
  )
)


(define (trc-type-invalid-bits-uplim-and-lowlim trc src etype)
  (begin
     ((trc-logging-printer2 trc) trc 0
         (format #f
"TYPE MISMATCH: the uplimit and/or lowlimit invalid in call to bits. SRC: "
         )
         src
     )
     ((trc-typeresolve-exit trc) #f) ;; Throw back #f as we failed now.
  )
)



(define (trc-type-invalid-bits-uplim-error trc src t1 uplindex etype)
  (begin
     ((trc-logging-printer2 trc) trc 0
         (format #f
"TYPE MISMATCH: the upper bit index of bits greater than or equal to the definite width of its first arg: (~s >=~s). SRC: "
                uplindex
                (type-bin->ext t1)
         )
         src
     )
     ((trc-typeresolve-exit trc) #f) ;; Throw back #f as we failed now.
  )
)




(define (trc-type-mismatch-error-not-input-signal trc src)
  (begin
     ((trc-error-printer trc)
         (format #f
"TYPE MISMATCH: the resolved type (~s) is not input signal: "
                 (texp-type src)
         )
         src
     )
     ((trc-typeresolve-exit trc) #f)
  )
)


(define (trc-change-type-and-update-changes! trc src new-type)
  (let ((old-type (texp-type src)))
    (cond ((not (eq? new-type old-type)) ;; Do only if src's type changes.
            (begin
               (set-texp-type! src new-type) ;; Update the type-field of src

               ((trc-logging-printer2 trc) trc 6
                  (format #f
 "Replaced the src's old type ~s (~s) with new one ~s (~s): "
                      (type-bin->ext old-type) old-type
                      (type-bin->ext new-type) new-type
                  )
                  src
               )

               (if (or ;; (type-certainly-looping? new-type) ;; XXX - in limbo
                       (type-width-definite? new-type)
                   )
                   (incr-resolvings-count! trc)
                   (incr-otherchanges-count! trc)
               )
            )
          )
    )
    new-type
  )
)


;; Returns the refined type if the (texp-type src) matches with etype,
;; otherwise bails out with a type-mismatch error message:
;; (Also update the type-field of src automatically with the new refined type.)

;; (In some other version, we might use  (types-match-with-loops? t1 t2) )
(define (trc-types-match!? trc src etype)
  (let* ((t1 (texp-type src))
         (t2 etype)
;;       (reftype (types-match? t1 t2))
         (reftype (types-match-with-loops? t1 t2))
        )
    (begin
      (cond ((not reftype) (trc-type-mismatch-error trc src etype))

;;          ((not (and (eq? reftype t1) (eq? reftype t2))) ...) ;; Any change?
            ((not (eq? reftype t1)) ;; Only if src's type changes.
              (begin
                ((trc-logging-printer2 trc) trc 500
                    (format #f
 "trc-types-match!?: prevtype=~s (~s):etype=~s (~s):refined type=~s (~s) at src:"
                            (type-bin->ext t1)      t1
                            (type-bin->ext etype)   etype
                            (type-bin->ext reftype) reftype
                    )
                    src
                )
                (trc-change-type-and-update-changes! trc src reftype)
              )
            )
      )
      reftype
    )
  )
)


;; (resolve-expr-type src trc etype)

;;
;; This does at least two type-unifications/refinings:
;;
;;  1: unifies etype (supplied by caller) with src's (cached) type,
;;  2: resolves the given form with that refined type as its expected type,
;;     (with usually its own resolvings/refinings invoked recursively)
;;  3: writes the possibly further refined type returned by step 2
;;     into the type-field of src.
;;     (That is, it is "cached" there, waiting to be used in the next pass.
;;      This because there might be a "type barrier", when we go to the
;;      next higher call-level, e.g. if we have:
;;      (zero? (bitand a (bitxor b c) d))
;;      then zero? will not expect anything from its args.)
;;

;; So, when coming here, we should first unify etype and src's type-field,
;; then dispatch to typeresolving form, with that new refined type
;; given as an argument to it, and after having obtained the resulting
;; (possibly refined) type from that form, set it again to src's type-field
;; and return it also as a result of this whole function.

;; Save the resulting type to the type-field of this src, so that we
;; can on the next pass use it for refining the etype given to
;; dispatch-to-generic-form:
;; We must keep it somewhere, as there might be a "type barrier"
;; when we go to the next higher call-level, e.g. if we have:
;; (zero? (bitand a (bitxor b c) d))
;; then zero? will not expect anything from its args (except that they
;; are of type "input" (not boolean)), and the width of bitand's result
;; and arguments might take multiple passes, until they set to some
;; definite value.

(define (resolve-expr-type src trc etype)
    (resolve-expr-type-with-genform #f src trc etype)
)

(define (resolve-expr-type-with-genform formname src trc etype)
 (begin
    ((trc-logging-printer2 trc) trc 5
                    (format #f
 "Entering resolve-expr-type-with-genform: formname=~s, etype=~s (~s), wirm-depth=~s, SRC:"
                            formname (type-bin->ext etype) etype
                            (trc-wirm-depth trc)
                    )
                    src
    )

    (let* ((etype2 (trc-types-match!? trc src etype))
           (dummy ((trc-logging-printer2 trc) trc 5
                     (format #f
 "In resolve-expr-type-with-genform after first trc-types-match and before recursive dispatch: etype2=~s (~s), SRC:"
                            (type-bin->ext etype2) etype2
                    )
                    src
                  )
           )
           (restype (dispatch-to-generic-form2
                         formname
                         src
                         trc
                         (trc-list-of-typeresolving-forms-in-use trc)
                         etype2
                    )
           )
           (dummy2 ((trc-logging-printer2 trc) trc 5
                     (format #f
 "In resolve-expr-type-with-genform after recursive dispatch: formname=~s, etype=~s (~s), etype2=~s (~s), restype=~s (~s), wirm-depth=~s, SRC:"
                            formname
                            (type-bin->ext etype) etype
                            (type-bin->ext etype2) etype2
                            (type-bin->ext restype) restype
                            (trc-wirm-depth trc)
                     )
                    src
                  )
           )
          )
       (trc-types-match!? trc src restype) ;; Unify once again.
    )
 )
)


(define (dispatch-to-typeresolving-form form-name src trc etype)
  (resolve-expr-type-with-genform form-name src trc etype)
)



;; TRC stands for Type-Resolving Context.

(define-structure (trc (keyword-constructor) (copier))

    parent-toc ;; A pointer to "top-oc" structure, defined in toplevel.scm

;; varnames to their defining locations (including their type fields
;;  and initializing forms):
;; This is an assoc-list consisting of pairs of the form:
;; (varname . (ptr_to_defining_location ptr_to_initializing_location))
;; <i.e.  (varname ptr_to_defining_location ptr_to_initializing_location)>
;; ptr_to_defining_location is a dtpr to a position in the list
;; of formal args (of fundef or lambda form or an automacro),
;; such that its car points to texp containing both the variable name
;; and its type (width) field.
;; Similarly for ptr_to_initializing_location, either it points
;; to the cdr of the initialization list of the variable in the named let,
;; or then it points to the appropriate position of the call-arglist
;; of the lambda or external funcall (or an automacro invocation?)

    to-be-expanded-items

    n_otherchanges

    n_mismatches

    n_resolvings

    types-max-width

    invocation-stack

    wirm-depth ;; Through how many wirm-expanded lambdas we have already come?
               ;; Passed to expwirms

    names2deflocs_et_inits

    labels2loops ;; assoc list of (label . loop-info)
;;    where we can loop back. (currently #f or a list of one element)

    defined-functions ;; a list of defined functions to which we can refer to.

    error-printer

    warning-printer

    logging-printer

    logging-printer2

    logging-level ;; Integer from 0 to ...

    logging-port

    toplevel-exit

    typeresolve-exit

    typeresolving-forms

    resolving-convention-in-use
)



(define (toc-make-fresh-trc toc typeresolve-exit
                            inherited-name-bindings
                            yet-to-be-expanded-items
                            wirm-depth
        )
  (make-trc
            'parent-toc toc
            'to-be-expanded-items yet-to-be-expanded-items
            'n_otherchanges (list 0) ;; Doesn't include full resolvings.
            'n_resolvings (list 0)
            'n_mismatches (list 0)
            'types-max-width (toc-typeresolving-max-width toc)
            'invocation-stack (list)
            'wirm-depth wirm-depth
            'names2deflocs_et_inits inherited-name-bindings
            'labels2loops (list) ;; Empty.
            'defined-functions (toc-defined-functions toc)

            'error-printer (toc-error-printer toc)
            'warning-printer (toc-warning-printer toc)

;; Use as: ((trc-logging-printer trc) trc 1 (format #t "...") extras)
            'logging-printer
               (lambda (trc . rest)
                  (apply (toc-logging-printer (trc-parent-toc trc))
                         (cons* "TYPERESOLVER" (trc-parent-toc trc) rest)
                  )
               )

            'logging-printer2
               (lambda (trc . rest)
                  (apply (toc-logging-printer2 (trc-parent-toc trc))
                         (cons* "TYPERESOLVER" (trc-parent-toc trc) rest)
                  )
               )

            'logging-level (toc-logging-level toc)

            'logging-port (toc-logging-port toc)

            'toplevel-exit (toc-toplevel-exit toc)

            'typeresolve-exit typeresolve-exit

            'typeresolving-forms *typeresolving-forms* ;; XXX - ?

            'resolving-convention-in-use (toc-resolving-convention-in-use toc)
  )
)


;;
;; This takes as its arguments a copy of partially type-annotated source
;; (as src) and a list of function definitions, read in from external
;; library files.
;; As a result, returns #t if managed to succesfully to complete
;; the type-annotation of src, with src physically modified accordingly,
;; including also all the definitions of subsequent invocations of external
;; library functions completely annotated and spliced in, into their
;; respective positions.
;;
;; Return #f if all that is not possible.
;;

(define (typeresolve-toplevel-call! toc src names2deflocs)
  (set-toc-typeresolving-passes-made! toc 0)
  (call-with-current-continuation
    (lambda (exitfun)
      (let ((trc (toc-make-fresh-trc toc
                                     exitfun
                                     names2deflocs
                                     (list (list (list)))
                                     0
                 )
           ))
         (typeresolve-until-no-unexpanded-wirms-remain! toc trc src
                                                        type-default-unresolved
         )
         (toc-typeresolving-passes-made toc)
      )
    )
  )
)


;; This one called from expwirms.scm:
(define (typeresolve! toc src names2deflocs_et_inits wirm-depth etype)
  (call-with-current-continuation
    (lambda (exitfun)
      (let* ((trc (toc-make-fresh-trc toc
                      (toc-toplevel-exit toc) ;; Was: exitfun
                      names2deflocs_et_inits
                      (list (list (list)))
                      wirm-depth
                  )
             )
             (dummy1 ((trc-logging-printer2 trc) trc 2
                       (format #f
 "Entering typeresolve!, with wirm-depth=~s, etype=~s (~s), SRC="
                            (trc-wirm-depth trc)
                            (type-bin->ext etype) etype
                       )
                       src
                    )
             )
            )
         (typeresolve-until-no-unexpanded-wirms-remain! toc trc src etype)
         ((trc-logging-printer2 trc) trc 2
                       (format #f
 "Exiting typeresolve!, with wirm-depth=~s, etype=~s (~s), RESULT="
                            (trc-wirm-depth trc)
                            (type-bin->ext etype) etype
                       )
                       src
         )
         src ;; XXX -- Depending on what the above one returns...
      )
    )
  )
)


;; What we should do here:
;;     (0) Delete any out-of-date comments and thinking-out-aloud monologues!
;;
;; Run the typeresolving until all widths have set (to nonchanging values),
;; then see whether there were any calls to wirm-macros collected
;; (still unexpanded), and if there were,
;; then, for each unexpanded wirm-call, call the function
;;   wirm-expand
;;    which starts from the call point in source,
;;     and
;;     (1) transforms it from (foo (expr1 ...) (expr2 ...))
;;         to ((lambda (x y) <foos-body>)  (expr1 ...) (expr2 ...))
;;         (i.e. only foo replaced with lambda having the formal args
;;         as in (define-wirm (foo x y) <foos-body>)
;;         with expansion level of that enclosing lambda set to 
;;         (1+ (trc-last-lambda-expansion-level trc))
;;         with (set-lambda-expansion-level ...)
;;         or to zero, if first such a replacement.
;;
;;     (2) invokes the wirm-expander (as expsynta, but with special dictionary)
;;         replacing (tree-copied <foos-body>) in place.
;;         Currently, we suppose that the first (i.e. the topmost, the most
;;         enclosing) form encountered in body is 'cond', 'or' or something
;;         else that rewrites eventually to 'if'.
;;         For if's test-expression, we call recursively
;;         typeresolve-until-no-unexpanded-wirms-remain! (which could
;;         meanwhile insert and resolve further calls to wirm-macros)
;;         It will inherit the original trc from here, because it might
;;         need refer to widths of variables defined by let/lambda etc.
;;
;;         (XXX -- If the topmost form is not 'if', and it needs
;;          the width of some expression to expand correctly,
;;          and we haven't yet run the type-resolver?
;;          So, maybe we should also invoke type-resolver
;;          whenever we encounter W or W-1 with unresolved argument?
;;          Why we don't call it just for W and W-1 ?
;;          Because, theoretically, we might have something like:
;;          (if (= 3 (let ((z (conc x y))) (W-1 z))) ...)
;;          which needs typeresolving to evaluate.
;;
;;          Should we really care about monstrosities like that?
;;
;;          Decision: NO! We presuppose that the test-expressions
;;          of the top-ifs in wirm-macros are simple, and easily
;;          evaluable/rewritable to a constant boolean value.
;;          (I.e. a subset of ordinarily allowed test-expressions,
;;           that could be anything).
;;
;;     (3) Only after that, the "specially interpreted if", will try
;;         to expand its test-branch, which most probably will contain
;;         references to W or RW, and now it can replace those W's and W-1's
;;         with constant values, and a test like (zero? (W x))
;;         or (= (W x) (RW)) should be completely resolvable to a
;;         constant boolean value.
;;
;;     (4) Depending on whether the test-branch of if was rewritten
;;         to #f or #t, the then or else-branch of if is spliced
;;         in the place of whole if.
;;
;;     (5) In any case, whatever the <foos-body> was rewritten to,
;;         we will run typeresolve-until-no-unexpanded-wirms-remain!
;;         (or typeresolve-until-widths-set! ?)
;;         again for it, before returning here.
;;         If there was an active recursive macro call, e.g. in else-branch,
;;         it will be inserted into (trc-to-be-expanded-items trc),
;;         and the loop here continues. (BUT NOT ON THIS RECURSION LEVEL,
;;         BUT ON A DEEPER ONE!)
;;         We catch infinite recursion by checking the lambda-expansion-level
;;         against some predefined max-value.
;;         XXX -- (It is collected by rewriting form for lambda, and passed
;;          on in extra argument, and when type-resolving ...)
;;


;; XXX -- This should return the total number of passes made for
;;        the needs of informative messages of typeresolve-testset-one-test:

;; See comment at the end of type-resolver for drop, to see why we
;; loop to the beginning after expanding any wirm-macros here:

(define (typeresolve-until-no-unexpanded-wirms-remain! toc trc src etype)
   (let loop ()
      (begin
        (typeresolve-until-widths-set! toc trc src etype)
        (let ((yet-to-expand-items (trc-to-be-expanded-items trc)))
          ((trc-logging-printer2 trc) trc 2
             (format #f "typeresolve-until-no-unexpanded-wirms-remain!: yet-to-expand-items=~a, SRC="
                     (texp-sprint yet-to-expand-items)
             )
             src
          )
          (cond ((not (null? (caar yet-to-expand-items))) ;; (() . ()) is last
                   (let ((expitem (pop! yet-to-expand-items)))
;; XXX - Should we pass the appropriate binding-context to expwirms? Hygiene???
                      (expwirms-in-place! toc
                                          (first expitem) ;; The call-point
                                          (second expitem) ;; & its wirm-depth
                      )
                      (loop)
                   )
                )
                (else src)
          )
        )
      )
   )
)


(define (typeresolve-until-widths-set! toc trc src etype)
      (let loop ((acc_changes 0))
        (let ((changes (typeresolve-next-pass! toc trc src etype)))
           (if (zero? changes)
               (toc-typeresolving-passes-made toc) ;; Or maybe acc_changes
               (loop (+ acc_changes changes))
           )
        )
      )
)


;; Returns as result the sum of complete resolvings + other changes
;; made in the current pass.
(define (typeresolve-next-pass! toc old-trc src etype)
    (let* ((trc (toc-make-fresh-trc toc
                                    (trc-typeresolve-exit old-trc)
                                    (trc-names2deflocs_et_inits old-trc)
                                    (trc-to-be-expanded-items old-trc)
                                    (trc-wirm-depth old-trc)
                )
          ))
          (cond ((> (toc-typeresolving-passes-made toc)
                    (toc-typeresolving-max-passes toc)
                 )
                   (trc-too-many-passes-error trc src)
                )
                (else
                  (begin
                     (if (zero? (toc-typeresolving-passes-made toc))
                         ((trc-logging-printer2 trc) trc 2
                           "typeresolve-next-pass! src BEFORE THE FIRST PASS: "
                           src
                         )
                     )
                     (let ((newtype (resolve-expr-type src trc etype)))
                       (begin
                         (set-toc-typeresolving-passes-made!
                              toc
                              (+ 1 (toc-typeresolving-passes-made toc))
                         )
                         ((trc-logging-printer2 trc) trc 2
                            (format #f
"typeresolve-next-pass! AFTER PASS ~s: Complete resolvings: ~s Other changes: ~s "
                                    (toc-typeresolving-passes-made toc)
                                    (trc-n_resolvings trc)
                                    (trc-n_otherchanges trc)
                            )
                            src
                         )
;; Return thesum of changes made:
                         (+ (first (trc-n_resolvings trc))
                            (first (trc-n_otherchanges trc))
                         )
                       )
                     )
                  )
                )
          )
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (trc-find-funinfo-struct-by-funname trc funname) ;; Used in typreslv
   (cond ((assq funname (trc-defined-functions trc)) => cdr)
         (else
            (error
 "trc-find-funinfo-struct-by-funname: Code is referring to a non-combinational function not available: " funname
            )
         )
   )
)

(define (trc-defined-functions-fargs trc funname) ;; Not used anymore.
  (t-rest (t-second
                (funinfo-org-definition ;; This can be used.
                                        (trc-find-funinfo-struct-by-funname
                                                trc funname
                                        )
                )
          )
  )
)

(define (trc-defined-functions-body trc funname) ;; Not actually used now.
   (t-third (funinfo-sexpanded-source
                     (trc-parent-toc trc)
                     (trc-find-funinfo-struct-by-funname trc funname)
            )
   )
)



(define (rescall-with-funname-pushed trc funname lambda_trc)
   (let ((new-trc (copy-trc trc)))
     (set-trc-invocation-stack! new-trc ;; Side-effects only to the new copy!
                              (if funname
                                  (cons funname (trc-invocation-stack new-trc))
                                  (trc-invocation-stack new-trc)
                              )
     )
     (lambda_trc new-trc)
   )
)


(define (resolve-with-funname-pushed src trc etype funname)
   (rescall-with-funname-pushed trc funname
      (lambda (trc) (resolve-expr-type src trc etype))
   )
)


(define (rescall-with-additional-bindings trc more-names2deflocs_et_inits
                                       lambda_trc)
 (let ((new-trc (copy-trc trc)))
   (set-trc-names2deflocs_et_inits!
      new-trc ;; Apply side-effects only to the new copy!
      (append more-names2deflocs_et_inits (trc-names2deflocs_et_inits new-trc))
   )
   (lambda_trc new-trc)
 )
)


(define (rescall-with-wirm-depth-set trc wirm-depth lambda_trc)
   (let ((new-trc (copy-trc trc)))
     (set-trc-wirm-depth! new-trc wirm-depth)
     (lambda_trc new-trc)
   )
)


(define (resolve-with-more-bindings src trc etype more-names2deflocs_et_inits)
   (rescall-with-additional-bindings trc more-names2deflocs_et_inits
      (lambda (trc) (resolve-expr-type src trc etype))
   )
)


(define (resolve-with-more-bindings-and-wirm-depth src trc etype more-names2deflocs_et_inits wirm-depth)
   (rescall-with-additional-bindings trc more-names2deflocs_et_inits
      (lambda (trc)
         (rescall-with-wirm-depth-set trc wirm-depth
            (lambda (trc) (resolve-expr-type src trc etype))
         )
      )
   )
)


(define (rescall-with-new-loop-info trc new-loop-label new-loop-info
              lambda_trc)
   (let ((new-trc (copy-trc trc)))
     (set-trc-labels2loops!
             new-trc ;; Apply side-effects only to the new copy!
             (cons (cons new-loop-label new-loop-info)
                   (trc-labels2loops trc)
             )
     )
     (lambda_trc new-trc)
   )
)


(define (resolve-and-match-farg-and-carg! trc farg carg) 
  (trc-types-match!? trc farg (resolve-expr-type carg trc (texp-type farg)))
)

;; Used in both typreslv.scm and expsynta.scm for constructing
;; names2deflocs_et_inits list, which is being transferred
;; from the latter module to the former, when (parts of) wirm-macro expansions
;; are type-resolved. So the syntax in both modules have to be exactly same!

(define (make-name2defloc-binding farg carg) (list (t-elem farg) farg carg))

(define (resolve-and-bind-named-exprs! trc fargs cargs)
   (map (lambda (farg carg)
           (begin
              (resolve-and-match-farg-and-carg! trc farg carg)
              (make-name2defloc-binding farg carg)
           )
        )
        fargs
        cargs
   )
)


;; Like above, but leaves the resolving part away. Otherwise creates a
;; similarly constructed association list. For the needs of expwirms
(define (just-bind-named-exprs fargs cargs)
   (map make-name2defloc-binding
        fargs
        cargs
   )
)

;; This is needed when type-resolving the toplevel-call, where we need
;; only the fargs and their types, but they don't have any "init-expressions":
(define (make-list-of-toplevel-initial-type-bindings fargs)
   (map (lambda (farg) (make-name2defloc-binding farg #f))
        fargs
   )
)


;; Like above, but doesn't return a pair list. Just for side-effects:
;; Currently used by <loop-tail-call>
(define (resolve-farg-carg-pairs! trc fargs cargs)
   (for-each (lambda (farg carg)
                (resolve-and-match-farg-and-carg! trc farg carg)
             )
             fargs
             cargs
   )
)



;; When encountering variable name, we should unify its width with etype,
;; (and set that width to the corresponding wire in trc-wiredefs,
;; if not already there.)
;; However, the name is not available in trc-names2deflocs_et_inits, unless
;; the enclosing let/named let/lambda or function definition
;; has been already traversed by compile.
;; IMPORTANT: we should match the type (width) to the same wire,
;; as to which the variable name is assigned to by compile
;; (That is, do NOT mix the scopes.)


(define (trc-find-varnames-definition trc src-name)
   (cond ((assq (t-elem src-name) (trc-names2deflocs_et_inits trc))
              => cdr ;; i.e. cdr of (list (t-elem farg) farg carg)
         )
         (else
            ((trc-error-printer trc)
                (format #f
 "trc-find-varnames-definition: Unmapped variable referenced: ~a not in: "
                        (t-elem src-name)
                )
                (trc-names2deflocs_et_inits trc)
            )
         )
   )
)

(define (trc-find-varnames-type trc src-name) ;; XXX - Not used, remove.
   (texp-type (car (trc-find-varnames-definition trc src-name)))
)



;; XXX - Here we might do a shortcut, and start type-resolving
;; (also) the carg (i.e. call-argument or init-form) of varname
;; (available as (second varnames-defloc-et-init) )
;; with resolve-expr-type with new refined etype, and so on,
;; but we will do that in any case in the next pass.
;; (Note that although the assoc list trc-names2deflocs_et_inits
;; is dissolved between separate type resolving passes,
;; (THIS IS NOT NECESSARILY TRUE ANYMORE, SEE typeresolve-next-pass!)
;; the refined types (of both fargs and cargs), will be kept
;; in the type-fields of the respective parts of the source-structure.)
;;

;; Here we clear the possible "looping-bit" of etype, because formal args
;; of named let could inherit that type only from their init-forms:
;; (Currently no effect, as looping type information is not used at all...)
(define (trc-match-with-varnames-defined-type trc src-name etype)
  (let ((varnames-defloc-et-init (trc-find-varnames-definition trc src-name)))
     (trc-types-match!? trc
                        (first varnames-defloc-et-init)
                        (type-cast-nonlooping etype)
     )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dig the partially type-annotated copy of the called function's definition,
;; type-resolve it as usual (i.e. complete the annotation, is it guaranteed?)
;; and return its restype. Update the typed-definitions list in funinfo-struct
;; if a distinct new type-signature was generated for this function.

;; Complete annotation not guaranteed in one pass. Consider:
;;
;; (define (foo x y z) (= (+ x y) (W z)))
;; called as (foo a b c)
;; Now, it might be so, that c's width is not resolved before
;; b's width is known, and that is known only after the first-type-resolving
;; pass to foo's body (provided the width of sub-expr 'a' is already known).

;; So we have to save a partially annotated definition of foo
;; (with type-signature having "holes", i.e. #f's in it)
;; to the typed-definitions, which must then be fetched second
;; time and annotated more thoroughly.
;; (We should take care that it is exactly this calling-instance
;; that is fetched second time... The search-key should include
;; the "position of call" somehow encoded. How?)
;; Or do we just take a copy of each partially resolved fundef,
;; gradually resolving it fully.
;; (Leaving the older ones for other possible call-instances?)
;; These form a kind of tree then.

;; The starting point is not necessarily the fundef with type-signature
;; having all #f's, as the exp-definition in funinfo-structure
;; might have some types already annotated, because they were fixed
;; in the source. So with first resolving pass, at least those
;; call-args will be resolved.

;; The funname and partially resolved type-signature form the key,
;; with which typed-definitions is searched with (assoc).

;; - Where is this "search-key" (funname + tsig with possible "holes") kept
;;   between possibly multiple passes?
;; - In the type-fields of call-args, I reckon.
;; 

;; (Note: we might get the same type-signature by two different "routes".
;; Should we unify the resulting fully annotated fundefs?
;; Here we assume that similarly partially annotated versions
;; of the fundef (i.e. type-signatures are equal, with holes in same positions)
;; will eventually lead to valid fully annotated versions in all cases,
;; even if diverging later.
;; (Is this assumption valid? What if in the other call there
;; are literals like 0 or 1, which result some heuristic optimizations?)

;;
;; Important: we might have in source file special definitions like:
;; (somewhat artificial example, but in general, up to some bit width,
;; some function implemented with LUT's or even FIFO lookup tables).
;;
;; (define (gcd,1 a b) (bitor a b))
;; (define (lcm,1 a b) (bitand a b))
;;
;; which should override the standard definitions without width-annotations.

;; Would this make the type-resolving nondeterministic, requiring
;; back-tracking? (or amb?) (Cf. in Prolog, which instance of the rule
;; we should try to satisfy first?)

;; This kind of implementation of "width-specialized functions"
;; (instead of just, say, defining with separate names as gcd1, lcm1, etc.)
;; would require that the indexing of partially annotated fundefs
;; were organized differently. The "root" of each search-tree
;; would be sexpanded version of each type-signature defined
;; in the source file, and then the resolving would continue
;; from each of those, matching the more restricted first.
;; (gcd,1 before gcd, if possible, etc.)

;; Note: Even with identical arg-types we might have different result-types.
;; And vice versa, with different arg-types, the result-types might still
;; be identical. (E.g. any test: (divisible-by-3? x) )

;; XXX -- Yes, really not ready.
;; This is only for noncombinational, external functions.
;; (Type-resolving forms for all the combinational functions should
;; be defined in this module separately).


(define-typeresolving-form ((regular 0.1 draft) (lambda) src trc etype)
  (let* ((lambda-keyword (t-first (t-first src)))
         (lambdas-type (t-type lambda-keyword))
         (u-fargs (t-u-second (t-first src))) ;; (cadar src)
         (cargs (t-carglist src))
         (more-bindings (resolve-and-bind-named-exprs! trc u-fargs cargs))
         (bodyexpr (body-with-no-implicit-progn
                          (cdr (t-rest (t-first src)))
                          trc
                          "lambda"
                   )
         )
        )

     (cond ((and (not (lambda-not-special? lambdas-type))
                 (lambda-left-by-wirm-expansion? lambdas-type)
            )
                 ((trc-logging-printer2 trc) trc 3
                    (format #f
  "In (lambda)-handler: wirm-depth now=~s, lambda's wirmexp level=~s, more-bindings=~a, SRC="
                            (trc-wirm-depth trc)
                            (get-lambda-wirm-expansion-level lambdas-type)
                            (texp-sprint more-bindings)
                    )
                    src
                 )

                 (resolve-with-more-bindings-and-wirm-depth
                                 bodyexpr
                                 trc
                                 etype
                                 more-bindings
                                 (get-lambda-wirm-expansion-level lambdas-type)
                 )
           )
           (else
                 ((trc-logging-printer2 trc) trc 3
                    (format #f
 "In (lambda)-handler: nothing special (lambda's type=~s), wirm-depth now=~s, more-bindings=~s, SRC="
                            lambdas-type
                            (trc-wirm-depth trc)
                            (texp-sprint more-bindings)
                    )
                    src
                 )

                 (resolve-with-more-bindings
                                 bodyexpr
                                 trc
                                 etype
                                 more-bindings
                 )
           )
     )
  )
)


;; Note that callpoint is a typed expression that will be physically
;; modified, both the elem-part which will be rewritten from:
;;  (foo (expr1 ...) (expr2 ...))
;; to:
;;  ((lambda (x y) <foos_body>) (expr1 ...) (expr2 ...))
;; (Actually, we can replace just foo, and leave cargs of the expression
;; as they are.)
;; and the type-part, which will eventually be set to the expected type
;; for the whole call-expression. Note how the most current estimate of
;; etype (as resolved from upwards) has been already written to the
;; type-field of src, before we come to <generic-funcall> handler and here.
;; Note that it could be refined further after the splicing is done,
;; but as it is a physical slot in callpoint (which is "typed source"),
;; it will be automatically updated here as well.
;;
;; Also, callpoint is used as a key for del-assq! which tests the equality
;; with physical (= pointer-wise) eq? test. This is so, that the handler
;; for <generic-funcall> can delete the wirm-call from this list,
;; after it has been once lambda-wrapper-expanded:

(define (trc-add-wirmcall-to-be-expanded-list! trc funname callpoint fundef fis)
  (let ((still-unexpanded (trc-to-be-expanded-items trc)))
     (attach! (list callpoint (trc-wirm-depth trc) funname fundef fis)
              still-unexpanded
     )
  )
)


(define (trc-splice-fundef-in-if-not-already! trc src funname)
 (begin
   (let ((cached-fundef (t-callees-fundef src)))
      (cond ((not cached-fundef) ;; First time here? Has to read it in.
              (let* (
;; XXX - In some future version (not (regular 0.1 draft) !) we might
;; use callers-tsig to fetch an alternatively typed definitions from
;; funinfo-typed-definitions list. But not now, because not designed yet.
;;                   (fun-cargs (t-carglist src))
;;                   (callers-tsig (cons (t-calls-type src)
;;                                       (map t-type fun-cargs)))
                     (fis (trc-find-funinfo-struct-by-funname trc funname))
                     (fundef (if (eq? 'define-wirm (funinfo-deftype fis))
;; XXX -- Should this be done for wirms here??? Or later?
;;                               (funinfo-sexpanded-source (trc-parent-toc trc)
;;                                                         fis
;;                               )
;; We do it later, in expwirms:
                                 (funinfo-org-definition fis)
                                 (funinfo-sexpanded-source (trc-parent-toc trc)
                                                           fis
                                 )
                             )
                     )
                    )
                 (begin
                    ((trc-logging-printer2 trc) trc 2
                       (format #f
 "Splicing in the definition of function ~s with definition:"
                               funname
                       )
                       fundef
                    )

                    (set-callees-fundef-and-fis! src
                                                 (texp-tree-copy fundef)
                                                 fis
                    )

                    (if (eq? (funinfo-deftype fis) 'define-wirm)
                        (begin
                           ((trc-logging-printer trc) trc 3
                               (format #f
 "Adding the wirm definition of ~s to a list of wirm-macros to be expanded. wirm-depth=~s.\n"
                                       funname
                                       (trc-wirm-depth trc)
                               )
                           )

                           (trc-add-wirmcall-to-be-expanded-list!
                                         trc
                                         funname
                                         src
                                         (t-callees-fundef src)
                                         (t-callees-fis src)
                           )
                        )
                    )
                 )
              )
            )
      )
   )
   (t-callees-fundef src)
 )
)


;; Almost same as lambda call (see above), but here we have to first
;; splice the definition of the loaded in library function to our
;; "hidden secret place", if not already stashed there:

;; If we are calling a wirm-macro instead, then we mark this call-point
;; to (trc-to-be-expanded-items trc), and will return to expand it,
;; once the type-widths have set in all the surrounding code.

;; A wirm-macro might have an optional type-resolving-form keyword present, as:
;;
;; (define-wirm (revbits b) :type-resolving-form <equitype-etype-and-args>
;;   (if (< (W b) 2) b
;;       (conc (bit b 0) (revbits (bits b (W-1 b) 1)))
;;   )
;; )
;;

(define (def-keyword? t)
  (let ((elem (t-elem t)))
     (and (symbol? elem) (char=? (string-ref (symbol->string elem) 0) #\:))
  )
)


;; XXX -- This should be in toplevel.scm or somewhere else, referring
;; only to toc, and not anything below that.
;; Note: this checks that the body has exactly one sub-expressions,
;; and then returns that sub-expression. (Together with its type).
(define (body-with-no-implicit-progn wholebody rwc where)
    (cond ((def-keyword? (car wholebody))
               (body-with-no-implicit-progn (cddr wholebody) rwc where)
          )
          ((> (length wholebody) 1)
            ((rwc-error-printer rwc)
              (format #f
"compile: No implicit progn allowed in the body of ~a! Use par or seq: " 
                      where
              )
              (detyped* wholebody)
            )
          )
          (else (car wholebody))
     )
)


;; In this case, we call <equitype-etype-and-args> immediately,
;; although the macro itself is not expanded until later.

(define-typeresolving-form ((regular 0.1 draft) <generic-funcall> src trc etype)

  (let* ((funname (t-u-first src))
         (fundef (trc-splice-fundef-in-if-not-already! trc src funname))
         (deftype (get-deftype-from-fundef fundef))
         (wirm-resolving-form
                         (and (> (t-length fundef) 2)
;; (def-keyword? (t-third fundef))
                              (eq? (t-u-third fundef) ':type-resolving-form)
                              (t-u-fourth fundef)
                         )
         )
         (fargs (t-rest (t-second fundef)))
         (cargs (t-carglist src))
;; This is done for all, ordinary define's, declare-extfun's and define-wirm's:
         (fargs-cargs-bound (resolve-and-bind-named-exprs! trc fargs cargs))
        )

      (case deftype
           ((define <define-expanded>) ;; Not here: <define-toplevel-call> !
             (begin
                ((trc-logging-printer2 trc) trc 3
                         (format #f
 "Starting to resolve the body of function ~s with definition:"
                                 funname
                         )
                         fundef
                )

                (resolve-with-more-bindings
                      (body-with-no-implicit-progn (cdr (t-rest fundef))
                                                   trc
                                                   "function/wirm body"
                      )
                      trc
                      etype
                      fargs-cargs-bound
                )
             )
           )

;; E.g. as: (declare-extfun 1'(outbyte1200 output'outchan 8'byte_to_output))
           ((declare-extfun) ;; Just return the declared ret-type -- Enough?
               (t-type (t-second fundef))
           )

           ((define-wirm)
              ((trc-logging-printer trc) trc 3
                         (format #f
 "typeresolving <generic-funcall>: Encountered again a call to wirm ~s, wirm-depth now=~s.\n"
                                 funname (trc-wirm-depth trc)
                         )
              )
              (if wirm-resolving-form
                  (dispatch-to-typeresolving-form wirm-resolving-form
                                                  src trc etype
                  )
                  etype
              )
           )

           (else
              (begin
                ((trc-logging-printer2 trc) trc 0
                         (format #f
 "Handler for <generic-funcall>: Cannot handle function ~s with definition:"
                                 funname
                         )
                         fundef
                )
                ((trc-typeresolve-exit trc) #f)
              )
           )

      )
  )
)




(define-typeresolving-form ((regular 0.1 draft) <let-named> src trc etype)
  (let* ((looplabel (t-u-second src))
         (vars_et_inits_typed (t-third src))
         (vars_et_inits (t-elem vars_et_inits_typed))
         (bodyexpr (body-with-no-implicit-progn (cddr (t-rest src))
                                                trc
                                                "named let"
                   )
         )
         (fargs (map t-first vars_et_inits))
         (cargs (map t-second vars_et_inits)) ;; I.e. initializing forms.
         (more-bindings (resolve-and-bind-named-exprs! trc fargs cargs))
        )
     (rescall-with-new-loop-info trc looplabel more-bindings
         (lambda (trc-with-loopinfo-added)
            (resolve-with-more-bindings
                bodyexpr
                trc-with-loopinfo-added
                etype
;; XXX - Somewhat a kludge: We don't expect every loop to be an eternal one!
;; Also, if the programmer wants a named let to return boolean in the end,
;; then he has to specifically annotate it as its return type:
;; (Looping-type-information not used now. Needs careful thinking.)
;;              (if (type-completely-unresolved? etype)
;;                  (type-cast-allow-looping type-unresolved-input)
;;                  (type-cast-allow-looping etype)
;;              )
                more-bindings
            )
         )
     )
  )
)


;; XXX - Until I invent something more sophisticated
;; We should probably try here also to unify the original cargs (init-exprs)
;; of fargs with these tail call cargs, and maybe also the corresponding
;; cargs between different tail calls.

;; One should never allow width-changing expressions as <loop-tail-call>
;; arguments!  (Unless they are of constant width?)
;; To avoid pathological cases like:
;; (let loop ((i x) (a 1,1))
;;      (if (zero? i)
;;          a
;;          (loop (- i 1) (conc 1,1 a))
;;      )
;; )
;;
;; or:
;;
;; (let loop ((i x) (a 1,1) (b 2,2))
;;      (if (zero? i)
;;          a
;;          (loop (- i 1) (conc 1,1 b) (conc a a))
;;      )
;; )

;; But something like:
;; (let loop ((i x) (a 0))
;;      (if (zero? i)
;;          a
;;          (loop (- i 1) (conc i i))
;;      )
;; )
;; should be valid.
;;
;; Well, conc and drop should give an error when all their expected and
;; argument widths have set to definite widths, and if they do not match!
;; What if they are ATLEAST-widths? Then with
;; (let loop ((i x) (a 1))
;;      (if (zero? i)
;;          a
;;          (loop (- i 1) (conc 1,1 a))
;;      )
;; )
;;
;; the expected type of a is first (ATLEAST 1), then
;; when typeresolving (conc 1,1 a) (ATLEAST 2) is matched with (ATLEAST 1),
;; (ATLEAST 3) with (ATLEAST 2), etc.

;; Drastic solution? Do not allow ATLEAST-widths as expected type
;; or argument types of forms like conc or drop ?
;; Other solution, would be to set some (compiler option-settable)
;; upper limit for ATLEAST widths (depending on FPGA platform and Verilog
;; version we are using, what is the max. width of bit-vectors,
;; 64?, 128?, 1024?, 65536?) and then just wait until the recursive
;; inferring process hits that ceiling (or maybe the stack overflows
;; before that...)

(define-typeresolving-form ((regular 0.1 draft) <loop-tail-call> src trc etype)
   (let* ((destlooplabel (t-u-first src))

          (loopinfo (cdr (assq destlooplabel (trc-labels2loops trc))))

          (fargs (map second loopinfo))

          (cargs (t-carglist src))
         )


      (resolve-farg-carg-pairs! trc fargs cargs)

;;    type-certainly-loops ;; XXX -- Until all the implications mulled over...
      etype
   )
)


;; XXX -- Check the logic...
;; Probably almost same as with or-f2 and fastest2, if they allow sub-exprs
;; that call non-returning (looping) functions, even though the latter ones
;; do not allow explicit tail-recursive-calls (checked in compile-module).
;;

;; Both branches, then and else, can be either type-looping (i.e. "bottom",
;; non-returning), or returning some value. In principle, the result
;; of whole if is the "union" of the respective types.
;; (E.g. if the other branch gives as (ATLEAST 4) its type,
;; and the other branch 36, then the result should be 36.)
;; However, if the other branch gives type-looping, then
;; we should optimistically discard it, and give the other branch's
;; type as a type of whole if.
;; However, if BOTH branches return type-looping, then the whole if's
;; type is also type-looping.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              | #f           | w | q (!=w) | ATLEAST(y)          | looping
;; ----------------------------------------------------------------
;; #f           | #f           | w | q       | ATLEAST(y)          | ???
;; w            | w            | w | MIS!    | w if w>=y,else MIS! | w
;; ATLEAST(x)   | ATLEAST(x)   |               ATLEAST(max(x,y))   | ATLEAST(x)
;; looping      | ???????      | w | q       | ATLEAST(y)          | looping
;;
;; In if, then & else-branches looping x looping = looping.
;; If the other branch does not resolve (yet?), then the resulting
;; type of if cannot be determined yet.
;;


;; 

(define-typeresolving-form ((regular 0.1 draft) <generic-if> src trc etype)
  (let ((test-expr (arg-first src))
        (then-expr (arg-second src))
        (else-expr (arg-third src))
       )
     (let* ((test-type (resolve-expr-type test-expr trc type-boolean))
            (then-type (resolve-expr-type then-expr trc etype))
            (else-type (resolve-expr-type else-expr trc then-type))
;; Not this:
;;                                         (type-inherit-loopiness-from
;;                                            (type-cast-nonlooping then-type)
;;                                            etype
;;                                         )
           )
        else-type
     )
  )
)


(define-typeresolving-form ((regular 0.1 draft) if src trc etype)
  (dispatch-to-typeresolving-form '<generic-if> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) seq-if src trc etype)
  (dispatch-to-typeresolving-form '<generic-if> src trc etype)
)


;; let's (whether named or unnamed) type should likewise match
;; its context. If it is the outermost form in the function,
;; then it should match with the return type of that function.


;; Boolean (zero?, <, >, =, etc.) and 1-bit reductive functions (rednor, etc.)
;; do not propagate any expected type downwards, as whatever their
;; argument's width is, they will always produce 1-bit wide result.
;; But neither do functions like zero?, rednor, redxor, etc.
;; ASSUME anything about their argument widths,
;; so a combinational expression like (rednor (* a b)) (~|(a*b) in Verilog)
;; is perfectly valid, even if we don't know what is the width of the
;; expression (* a b) (as long as we DON'T TRY to allocate a wire for it!)

;; However, if we create a separately compiled function
;; (define (delayed-zero? x) (zero? (delay1 x)))
;; or (define (delayed-zero? x) (delay1 (zero? x)))
;; then when calling it as (delayed-zero? (* a b))
;; The expression (* a b) MUST resolve to a known width.

;; If we have an expression like (bitxor a b c),
;; then at least one of etype, or widths of a, b or c
;; must resolve, and the other's widths are set/should unify
;; to the equal value.

;; If we have a separately compiled definition:
;; (define (delayed-bitxor3 x y z) (delay1 (bitxor x y z)))
;; then, when type-resolving a call (delayed-bitxor3 a b c)
;; we should be type-resolving an expression (delay1 (bitxor a b c))
;; (delay1 resolves as a pass-thru type, with its arg-type == etype)
;; which SHOULD ALSO help to resolve the expressions a, b, c and etype
;; in the caller. (Also, the same resolved widths should be set to the
;; formal args of delayed-bitxor3)

;; A more realistic example:
;; (define (bitclr b mask) (bitand b (bitnot mask)))
;; (This would be compiled as compile-time function for Verilog,
;; which also requires knowing the widths of both arguments and the result).

;; Now, the resolving for bitxor, bitand, etc. operations
;; proceeds as, _if_ none of etype or other widths are yet known,
;; by recursively resolving each argument-expression in turn,
;; and when a first one is encountered, which resolves to a known value,
;; then all the other branches are called (possible SECOND time),
;; with that value as their etype. (And the same type/width is also
;; returned as the type-result of this form, to be used upwards.)

;; When encountering variables at the leaves of sub-expressions,
;; whose type is resolved from the above, we should annotate them,
;; but should we also have an assoc-list of (variable . type) bindings,
;; managed according to scope, to speed up the resolving process?
;; (And have a more powerful resolving ability?)

;; Actually, <loop-tail-call> is almost only form, that SHOULDN'T
;; type-match to the enclosing form. (Usually some sort of if?)
;; (However, if both 'then' and 'else'-branch of 'if' are of type-looping,
;; it should propagate that type upwards.)

;; if, seq-if, seq2, etc will type-match to the enclosing context,
;; but if in some branch there is <loop-tail-call>, then that branch
;; resolves to a special type type-looping.
;; (In seq2 that must be the last branch, whose type will be in any case
;; returned as the type of the whole expression.)

;; If the other branch of if is not a <loop-tail-call> but instead
;; a final branch, then it should type-match with etype, which will
;; eventually match with the etype of the innermost named let (loop).
;; The actual compilation module compile1.scm checks that we don't
;; accept pathological forms like (foo (if (test1 ...) (bal ...) (loop ...)))
;;
;; Both branches of 'if' might be <loop-tail-call>s, if it is a part
;; of a larger rewritten cond. However, if the top-most 'if' resolves
;; to type-looping, we definitely have an error, because then we don't
;; have a terminating branch in the loop.
;; (But what if the programmer _wants_ an endless loop?)

;; Under and2 and par2 there should not be any <loop-tail-call>'s.
;; (Checked by compile1.scm)

;; XXX -- Note a potential future issue here:
;; we don't currently distinguish here between the "e-mode" and "i-mode",
;; like in compilation-module, thus all named let's are assumed
;; to define a tail-recursive construct, that is, the name defined
;; should be called only in genuinely tail-recursive positions.
;; Here we will not currently detect it, but compile1.scm will
;; catch those kind of bugs later. (I.e. if loop-call occurs under
;; the test-branch of if, for example).

;; For now, the sepcial type type-looping (i.e. "bottom")
;; can catch some of those kind of errors. I.e. if loop tail call
;; occurs as an argument to any other function/form than
;; as then/else branch of if, or the last branch of seq2
;; or in lambda-body, then it will be caught.
;; (E.g. if given for example as one of the arguments to bitand)

;; However, if in some distant future we want to implement genuinely
;; recursive forms, then we need a much more sophisticated analysis
;; of the control flow.


;; Here either etype might refine the type in "master repository"
;; (i.e. in (trc-names2deflocs_et_inits trc))
;; or other way, the refined type in master repository might
;; refine the context's type.
;; (The latter case handled automatically by calling resolve-expr-type)
;;
(define-typeresolving-form ((regular 0.1 draft) <terminal-argument> src trc etype)

   (cond ((t-is-symbol? src)
             (trc-match-with-varnames-defined-type trc src etype)
         )

;; Should contain ATLEAST qualifier:
         ((t-is-integer? src)
            (type-i-of-atleast-width
                (if (not (negative? (t-elem src)))
                    (nonnegatives_least_binwidth (t-elem src))
                    (negatives_least_binwidth (t-elem src))
                )
            )
         )

         ((boolean? (t-elem src)) type-boolean)
         
         (else ;; It's some other literal. XXX - What?
              ((trc-error-printer trc)
                 (format #f
"Don't know (yet) how to type-resolve a non-integer literal:"
                 )
                 (t-elem src)
              )
         )
   )
)




(define-typeresolving-form ((regular 0.1 draft) zero? src trc etype)
  (dispatch-to-typeresolving-form '<boolean-etype-with-one-anywidth-arg>
                                  src trc etype
  )
)

(define-typeresolving-form ((regular 0.1 draft) nonzero? src trc etype)
  (dispatch-to-typeresolving-form '<boolean-etype-with-one-anywidth-arg>
                                  src trc etype
  )
)


(define-typeresolving-form ((regular 0.1 draft) even? src trc etype)
  (dispatch-to-typeresolving-form '<boolean-etype-with-one-anywidth-arg>
                                  src trc etype
  )
)

(define-typeresolving-form ((regular 0.1 draft) odd? src trc etype)
  (dispatch-to-typeresolving-form '<boolean-etype-with-one-anywidth-arg>
                                  src trc etype
  )
)


(define-typeresolving-form ((regular 0.1 draft) pow2? src trc etype)
  (dispatch-to-typeresolving-form '<boolean-etype-with-one-anywidth-arg>
                                  src trc etype
  )
)


(define-typeresolving-form ((regular 0.1 draft) = src trc etype)
  (dispatch-to-typeresolving-form '<boolean-etype-and-equitype-args> src trc
                                                                     etype
  )
)

(define-typeresolving-form ((regular 0.1 draft) == src trc etype)
  (dispatch-to-typeresolving-form '<boolean-etype-and-equitype-args> src trc
                                                                     etype
  )
)

(define-typeresolving-form ((regular 0.1 draft) != src trc etype)
  (dispatch-to-typeresolving-form '<boolean-etype-and-equitype-args> src trc
                                                                     etype
  )
)


(define-typeresolving-form ((regular 0.1 draft) < src trc etype)
  (dispatch-to-typeresolving-form '<boolean-etype-and-equitype-args> src trc
                                                                     etype
  )
)

(define-typeresolving-form ((regular 0.1 draft) <= src trc etype)
  (dispatch-to-typeresolving-form '<boolean-etype-and-equitype-args> src trc
                                                                     etype
  )
)


(define-typeresolving-form ((regular 0.1 draft) > src trc etype)
  (dispatch-to-typeresolving-form '<boolean-etype-and-equitype-args> src trc
                                                                     etype
  )
)

(define-typeresolving-form ((regular 0.1 draft) >= src trc etype)
  (dispatch-to-typeresolving-form '<boolean-etype-and-equitype-args> src trc
                                                                     etype
  )
)



;; Note: the aliases should probably handler somewhere else than here!
;; (Unnecessary duplication).

(define-typeresolving-form ((regular 0.1 draft) asl src trc etype)
     (dispatch-to-typeresolving-form '<generic-shift> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) <<< src trc etype)
     (dispatch-to-typeresolving-form '<generic-shift> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) asr src trc etype)
     (dispatch-to-typeresolving-form '<generic-shift> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) >>> src trc etype)
     (dispatch-to-typeresolving-form '<generic-shift> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) shl src trc etype)
     (dispatch-to-typeresolving-form '<generic-shift> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) << src trc etype)
     (dispatch-to-typeresolving-form '<generic-shift> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) shr src trc etype)
     (dispatch-to-typeresolving-form '<generic-shift> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) >> src trc etype)
     (dispatch-to-typeresolving-form '<generic-shift> src trc etype)
)



(define-typeresolving-form ((regular 0.1 draft) redand src trc etype) 
  (dispatch-to-typeresolving-form '<in1bit-etype-with-one-anywidth-arg> src trc
                                                                        etype
  )
)

(define-typeresolving-form ((regular 0.1 draft) rednand src trc etype) 
  (dispatch-to-typeresolving-form '<in1bit-etype-with-one-anywidth-arg> src trc
                                                                        etype
  )
)

(define-typeresolving-form ((regular 0.1 draft) redor src trc etype) 
  (dispatch-to-typeresolving-form '<in1bit-etype-with-one-anywidth-arg> src trc
                                                                        etype
  )
)

(define-typeresolving-form ((regular 0.1 draft) rednor src trc etype) 
  (dispatch-to-typeresolving-form '<in1bit-etype-with-one-anywidth-arg> src trc
                                                                        etype
  )
)

(define-typeresolving-form ((regular 0.1 draft) redxor src trc etype) 
  (dispatch-to-typeresolving-form '<in1bit-etype-with-one-anywidth-arg> src trc
                                                                        etype
  )
)

(define-typeresolving-form ((regular 0.1 draft) redxnor src trc etype) 
  (dispatch-to-typeresolving-form '<in1bit-etype-with-one-anywidth-arg> src trc
                                                                        etype
  )
)


;; These should check that their arguments are also of type-boolean:
(define-typeresolving-form ((regular 0.1 draft) lognot src trc etype)
  (dispatch-to-typeresolving-form '<boolean-etype-and-args> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) logand src trc etype)
  (dispatch-to-typeresolving-form '<boolean-etype-and-args> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) logor src trc etype)
  (dispatch-to-typeresolving-form '<boolean-etype-and-args> src trc etype)
)


;; Should we require that the argument of bitnot and other bit-forms
;; are NOT of type boolean? Or do we assume automatic #f -> 0, #t -> 1
;; casting when a boolean function occurs as an argument of
;; a function expecting bit-string? (Yes, the latter.)

;; Also, this should handle the case where an output signal midiout1
;; is bound, _inverted_, to called function's output argument,
;; (uart_out byte (bitnot midiout1))
;; with probably an intermediate wire bitnot_1 = ~midiout1;
;; generated, which in turn is bound with instantiate to farg of uart_out

;; Similarly with any width-preserving unary functions or wirms, like revbits,
;; it should be possible to use them also with output arguments/signals.

(define-typeresolving-form ((regular 0.1 draft) bitnot src trc etype)
  (dispatch-to-typeresolving-form '<equitype-etype-and-args> src trc etype)
)


(define-typeresolving-form ((regular 0.1 draft) bitand src trc etype)
  (dispatch-to-typeresolving-form '<equitype-etype-and-args> src trc etype)
)


(define-typeresolving-form ((regular 0.1 draft) bitor src trc etype)
  (dispatch-to-typeresolving-form '<equitype-etype-and-args> src trc etype)
)


(define-typeresolving-form ((regular 0.1 draft) bitxor src trc etype)
  (dispatch-to-typeresolving-form '<equitype-etype-and-args> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) bitxnor src trc etype)
  (dispatch-to-typeresolving-form '<equitype-etype-and-args> src trc etype)
)


(define-typeresolving-form ((regular 0.1 draft) + src trc etype)
  (dispatch-to-typeresolving-form '<equitype-etype-and-args> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) +w src trc etype)
  (dispatch-to-typeresolving-form '<equitype-etype-and-args> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) +c src trc etype)
  (dispatch-to-typeresolving-form '<etype-one-wider-than-args> src trc etype)
)


(define-typeresolving-form ((regular 0.1 draft) - src trc etype)
  (dispatch-to-typeresolving-form '<equitype-etype-and-args> src trc etype)
)


(define-typeresolving-form ((regular 0.1 draft) -1+ src trc etype)
  (dispatch-to-typeresolving-form '<equitype-etype-and-args> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) 1+ src trc etype)
  (dispatch-to-typeresolving-form '<equitype-etype-and-args> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) * src trc etype)
  (dispatch-to-typeresolving-form '<equitype-etype-and-args> src trc etype)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In these forms it might be a good idea to allow also sub-expressions
;; of the type-looping:
;; (For launching watch-dogs and things like that.)
;; Well, doesn't make much sense with slowest2 or and-s2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typeresolving-form ((regular 0.1 draft) and2 src trc etype)
  (dispatch-to-typeresolving-form '<equitype-etype-and-args> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) and-s2 src trc etype)
  (dispatch-to-typeresolving-form '<equitype-etype-and-args> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) or-f2 src trc etype)
  (dispatch-to-typeresolving-form '<equitype-etype-and-args> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) fastest2 src trc etype)
  (dispatch-to-typeresolving-form '<equitype-etype-and-args> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) slowest2 src trc etype)
  (dispatch-to-typeresolving-form '<equitype-etype-and-args> src trc etype)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-typeresolving-form ((regular 0.1 draft) par2 src trc etype)
  (dispatch-to-typeresolving-form '<generic-progn> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) seq2 src trc etype)
  (dispatch-to-typeresolving-form '<generic-progn> src trc etype)
)


;; For par2 and seq2. Would work with more than two args as well.
;; Only the last expression is matched against, although ones before
;; that (e.g/i.e. the first one) are run through resolver also.

(define-typeresolving-form ((regular 0.1 draft) <generic-progn> src trc etype)
    (let loop ((subexprs (t-carglist src)))
         (cond ((null? (cdr subexprs)) ;; Last one?
                   (resolve-expr-type (car subexprs) trc etype)
               )
               (else
                  (begin
                     (resolve-expr-type (car subexprs)
                                        trc
                                        (texp-type (car subexprs))
                     )
                     (loop (cdr subexprs))
                  )
               )
         )
    )
)



;; Note that the form like (bitxor a b) might occur as a sub-expression
;; of, say, (bitand (bitxor a b) c), (with etype=#f), and thus, when we
;; try to resolve it first time, it might be left unresolved, giving #f,
;; after which, on the higher level, we try to resolve c, and if that resolves,
;; then we resolve (bitxor a b) again, this time with known etype.
;; (Which will set/unify both a and b to that type.)

;; The calling resolve-expr-type will set the refined type returned
;; by fold-left into the type-field of that src, and the next time
;; this is called, it will be etype.

(define-typeresolving-form ((regular 0.1 draft) <equitype-etype-and-args> src trc etype)
    (fold-left (lambda (reftype subexpr)
                    (resolve-expr-type subexpr trc reftype)
               )
               etype ;; Start refining from the etype given to us,
               (t-carglist src) ;; with the types of subexpressions
    )
)


;; This is for +c: XXX - First cut. We might need to do explicit
;; (conc 1'0 ...)'s for the subexpressions.
(define-typeresolving-form ((regular 0.1 draft) <etype-one-wider-than-args> src trc etype)
  (let tryagain ((etype etype))
     (cond ((type-completely-unresolved? etype)
              (tryagain (type-i-of-atleast-width 2))
           )
           ((< (type-width-of etype) 2)
              (if (type-width-definite? etype) ;; if definite, then error.
                  (trc-type-nospace-for-carry-error trc src etype)
                  (tryagain (type-with-inc1-width etype))
              )
           )
           (else ;; There is space in etype for carry, its width is at least 2.
             (type-with-inc1-width
                (fold-left (lambda (reftype subexpr)
                               (resolve-expr-type subexpr trc reftype)
                           )
                           (type-with-dec1-width etype)
                           (t-carglist src) ;; with the types of subexpressions
                )
             )
           )
     )
  )
)

;; Our first cut for the needs to wirm-macro zeqw-1 which creates zeros of
;; one bit narrower width. So far we do inferring only from arg to etype
;; direction, not the other way... -- XXX to do?
(define-typeresolving-form ((regular 0.1 draft) <etype-one-narrower-than-arg> src trc etype)
   (let ((args-type (resolve-expr-type (arg-first src)
                                         trc
                                        (t-type (arg-first src))
                    )
        ))
     (if (> (type-width-of args-type) 1) ;; Don't create zero-widths!
            (type-with-dec1-width args-type)
            args-type
     )
   )
)



;; XXX -- For the needs of *c. A simple-minded version, with no
;; back-inferences from etype to any of args. Should be really
;; implemented in similar way as the typeresolver of conc2 is.
;; (Well, I changed *c2 to also use that.)
(define-typeresolving-form ((regular 0.1 draft) <etype-sum-of-args-buggy> src trc etype)
 (let ((sum-of-widths
        (fold-left (lambda (prevsum subexpr)
                     (+ prevsum
                        (type-width-of (resolve-expr-type subexpr
                                                          trc
                                                          (t-type subexpr)
                                       )
                        )
                     )
                   )
                   0
                   (t-carglist src)
        )
      ))
;; XXX -- Shouldn't do this! def -> nondef causes idiot loops?
    (type-i-of-atleast-width sum-of-widths)
 )
)



(define-typeresolving-form ((regular 0.1 draft) <boolean-etype-and-equitype-args> src trc etype)
  (let ((reftype
          (fold-left (lambda (reftype subexpr)
                        (resolve-expr-type subexpr trc reftype)
                     )
                     (t-type (arg-first src)) ;; Start refining from this.
                     (t-carglist src)
          )
       ))
;; Cache the refined type of call-arguments to the type-field of the first
;; one of them (for the next pass):
     (begin
            ((trc-logging-printer2 trc) trc 5
                (format #f
 "<boolean-etype-and-equitype-args>: replacing the first subexprs type from ~s (~s) to ~s (~s) at src:"
                        (type-bin->ext (t-type (arg-first src)))
                        (t-type (arg-first src))
                        reftype
                        (type-bin->ext reftype)
                )
                src
            )
            (trc-change-type-and-update-changes! trc (arg-first src) reftype)
;; And then return the type the caller expects, that is, a simple boolean:
            type-boolean
     )
  )
)


(define-typeresolving-form ((regular 0.1 draft) <boolean-etype-and-args> src trc etype)
    (fold-left (lambda (reftype subexpr)
                    (resolve-expr-type subexpr trc reftype)
               )
               type-boolean ;; (instead of etype) ;; Forces all to be booleans.
               (t-carglist src) ;; with the types of subexpressions
    )
)


;; Here the only requirement from unary arg is that it is of type input:

(define-typeresolving-form ((regular 0.1 draft) <boolean-etype-with-one-anywidth-arg> src trc etype)
  (begin (resolve-expr-type (arg-first src) trc (type-i-of-atleast-width 0))
         type-boolean
  )
)


(define-typeresolving-form ((regular 0.1 draft) <in1bit-etype-with-one-anywidth-arg> src trc etype)
  (begin (resolve-expr-type (arg-first src) trc (type-i-of-atleast-width 0))
         (type-i-of-definite-width 1)
  )
)


(define-typeresolving-form ((regular 0.1 draft) <etype-atleast-as-wide-as-the-arg> src trc etype) ;; Experimental for SXT and ZXT wirm-macros.
   (type-cast-nondefinite (resolve-expr-type (arg-first src)
                                             trc
                                             (t-type (arg-first src))
                          )
   )
)

;; XXX - Here the problem is that the type-resolver fixes the etype width
;; in the first pass, although it could otherwise still grow in later
;; passes:
(define-typeresolving-form ((regular 0.1 draft) <force-etype-definite> src trc etype) ;; For SXT and ZXT wirm-macros. Also experimental, can't do it this way.
   (type-cast-definite etype)
)


(define-typeresolving-form ((regular 0.1 draft) <etype-atleast-as-wide-as-binwidth-of-nonneg-arg> src trc etype) ;; For ???
  (let ((t1 (resolve-expr-type (arg-first src) trc (t-type (arg-first src)))))
     (type-i-of-atleast-width
              (nonnegatives_least_binwidth (type-width-of t1))
     )
  )
)



(define-typeresolving-form ((regular 0.1 draft) ew src trc etype) etype)
;; Or?:   type-default-unresolved ;; At this point we know nothing about it!


;; XXX - To do: We have to collect w's, w-1's and w+1's to to-be-expanded-items
;; list, in similar way as the wirm-macro calls are collected.
;; (So that expsynta or expwirms can replace them with integer literals,
;;  later, when the widths have set.)
;; This because not all calls to w's are inside wirm-macros!
;; (What if we are expanding a wirm-macro? Beware of dangling references!)
;;
(define-typeresolving-form ((regular 0.1 draft) w src trc etype)
  (let ((t1 (resolve-expr-type (arg-first src) trc type-default-unresolved)))
     (type-i-of-atleast-width
              (nonnegatives_least_binwidth (type-width-of t1))
     )
  )
)


(define-typeresolving-form ((regular 0.1 draft) w-1 src trc etype)
  (let ((t1 (resolve-expr-type (arg-first src) trc type-default-unresolved)))
     (type-i-of-atleast-width
          (nonnegatives_least_binwidth (if (zero? (type-width-of t1))
                                           0
                                           (- (type-width-of t1) 1)
                                       )
          )
     )
  )
)


(define-typeresolving-form ((regular 0.1 draft) w+1 src trc etype)
  (let ((t1 (resolve-expr-type (arg-first src) trc type-default-unresolved)))
     (type-i-of-atleast-width
              (nonnegatives_least_binwidth (+ (type-width-of t1) 1))
     )
  )
)



;; 
;; ;; This is just for testing with run-typeresolve-testset and should not
;; ;; actually be encountered or run in "production code":
;; ;; (Note how we try to bind fargs to themselves, which really doesn't work.)
;; (define-typeresolving-form ((regular 0.1 draft) <define-expanded> src trc etype)
;;   (let* ((fundef src)
;;          (fargs (t-rest (t-second fundef)))
;;          (cargs fargs)
;;          (fundef-body (body-with-no-implicit-progn (cdr (t-rest fundef))
;;                                                    trc
;;                                                  "function body"
;;                       )
;;          )
;;         )
;;      (resolve-with-more-bindings fundef-body
;;                                  trc
;;                                  etype
;;                                  (resolve-and-bind-named-exprs! trc fargs cargs)
;;      )
;;    )
;; )
;; 

;; 
;; ;; This was the original idea when the type-system was simpler, with
;; ;; just "undefined type" (#f) and types of definite widths:
;; (define-typeresolving-form ((historical 0.1 unused) <equitype-etype-and-args> src trc etype)
;;   (let recurse ((etype etype))
;;      (cond ((type-defined? etype) ;; If etype is defined (i.e. not #f),
;; ;;             then expect everything else to have the same type too:
;;               (for-all? (t-carglist src)
;;                         (lambda (sub) (resolve-expr-type sub trc etype))
;;               )
;;            )
;; 
;; ;; Otherwise at least one of the subexpressions should resolve:
;; ;; (If one of them does, then we shall call this again with a set etype).
;;            ((there-exists? (t-carglist src)
;;                            (lambda (sub) (resolve-expr-type sub trc etype))
;;             ) => recurse ;; The beauty of Scheme!
;;            )
;; 
;;            (else #f) ;; Return #f, as we couldn't resolve it at this time!
;;      )
;;   )
;; )
;; 
;; 
;; ;; Here the situation is different, as there is no extra help coming from
;; ;; the next higher level. So, it's probably an error if we can't resolve
;; ;; the types of arguments, but for now, let's give just a warning message:
;; ;; (We could have something like (if (= (* a b) (* c d)) ...) )
;; 
;; (define-typeresolving-form ((historical 0.1 unused) <boolean-etype-and-equitype-args> src trc etype)
;;   (begin
;;     (type-assert-is-one-of-listed etype (list type-boolean) src trc)
;;     (let ((atype (there-exists? (t-carglist src)
;;                                 (lambda (s) (resolve-expr-type s trc #f))
;;                  )
;;          ))
;;       (cond ((not (type-defined? atype)) ;; Should we raise an error?
;;                 ((trc-warning-printer trc)
;;                     (format #f
;;  "none of the args resolves to any type/width: "
;;                     )
;;                     src
;;                 )
;;             )
;; 
;;             (else (for-all? (t-carglist src)
;;                             (lambda (sub) (resolve-expr-type sub trc atype))
;;                   )
;;             )
;;       )
;;     )
;; 
;;     (or etype type-boolean)
;;   )
;; )
;; 


;; With shifts we should unify the etype and the type of first arg.
;; The type of second arg is of no importance. (Ok if it is a boolean function,
;; as we get automatically for (shr x (zero? y)) an interpretation:
;; (Shift x right (one bit) if y is zero.)

;; For +, -, bitand, bitor, bitxor, bitxnor we should check that the arguments
;; are of the same width. Also, the expected type flows downward.
;; That is, we should unify the expected type and the types of all
;; arguments.

;; If etype is defined, then it's easy, we propagate it as
;; etype of all the argument functions also.
;; Otherwise, at least one of the argument functions should
;; be resolved to a known type (width), which is then unified
;; with all the rest, and also passed upwards as a type of
;; this function.

;; With boolean forms like ==, !=, <, >, etc. at least the other
;; argument should be resolved (from downwards) to a known type (width),
;; and that can then be propagated as an expected type to the other
;; argument.

;; With conc, if etype is not defined, the all arg-exprs
;; should resolve from downwards.
;; If etype _is_ defined, then at max. one can be unresolved,
;; which is then computed as expected_width - sum_of_all_other_arg_widths

;; With (SXT a) and (ZXT a), the width of a is essentially ignored
;; and etype MUST be defined (if the width of a resolves from downwards,
;; then it must be <= etype. If width of a _doesn't resolve_, then
;; the expected width of it is expected width of SXT or ZXT ???)
;;

;; With tail-recursive loop calls, either the registers have
;; already known widths (e.g. have been declared explicitly),
;; and thus we have a definite expected width, or then the
;; argument-forms must resolve to known widths/types, and are thus
;; passed "upwards" as widths for those registers.

;; (And non-combinational function calls are handled about the same way?
;; If an argument of the called function has no explicitly declared
;; type/width, then it must be resolved from the corresponding
;; call-arg expression's width/type.)
;; (Note: the type of an undeclared argument of a function
;; could also be resolved by other means, by determining it with
;; information solely available in that function definition.)


;; What ref references is a kind of array. How to implement?
;; (define-typeresolving-form ((regular 0.1 draft) ref src trc etype)
;;     (resolve-expr-type (arg-first src) trc etype)
;; )

;;
;;    etype           t1             t2
;;     DEF            DEF            DEF   Should hold: etype = t1+t2
;;
;;     DEF0           DEF1          INDEF   types-match(DEF0,DEF1+INDEF)
;;                                         i.e. INDEF must be less than
;;                                                DEF0-DEF1, and DEF1<=DEF0
;;                                         we can infer that INDEF=DEF0-DEF1
;;     DEF0          INDEF          DEF2   (as above, mutatis mutandis).
;;
;;     DEF           INDEF          INDEF   types-match(DEF,INDEF+INDEF)
;;     (Note that this case should match only when the sum of t1's and t2'
;;      atleast-widths is <= etype's definite width.
;;      However, if their sum would be exactly etype's definite width,
;;      and if we used this occassion for fixing either or both of them
;;      (to definite ones), then it would probably introduce subtle
;;      call-tree search-order dependencies
;;      to our type-resolving process, or the overall success might
;;      require backtracking, which we don't want to do now.)
;;
;;    INDEF           DEF            DEF    types-match(INDEF,t1+t2)
;;
;;    INDEF           DEF1          INDEF2  types-match(INDEF,DEF1+INDEF2)
;;                                          (we cannot infere any definite
;;                                           widths from this).
;;    INDEF          INDEF1          DEF2  (as above, mutatis mutandis)
;;
;;
;;    INDEF          INDEF          INDEF   should match always, because
;;                                          all values can still grow.
;;
;;

;; XXX - Clean up. Should be completely independent of the sigtype
;; of the type-mask, and etype should inherit that in some appropriate
;; way.

(define-typeresolving-form ((regular 0.1 draft) conc2 src trc etype)
  (dispatch-to-typeresolving-form '<etype-sum-of-2-args> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) *c2 src trc etype)
  (dispatch-to-typeresolving-form '<etype-sum-of-2-args> src trc etype)
)

(define-typeresolving-form ((regular 0.1 draft) <etype-sum-of-2-args> src trc etype)
  (let* ((expr1 (arg-first src))
         (expr2 (arg-second src))
         (t1 (resolve-expr-type expr1 trc (texp-type expr1)))
         (t2 (resolve-expr-type expr2 trc (texp-type expr2)))
         (width-of-t1+t2 (+ (type-width-of t1) (type-width-of t2)))
         (ewidth (type-width-of etype))
        )
     (cond ((type-width-definite? etype) ;; The first four cases from above.
              (begin                     ;; we have a definite upper limit.
                (cond ((> width-of-t1+t2 ewidth)
                         (trc-type-mismatching-sum-error trc src t1 t2 etype)
                      )

                      ((and (type-width-definite? t1)
                            (type-width-definite? t2)
                       ) ;; if all definite, then the sum must match exactly:
                          (if (< width-of-t1+t2 ewidth)
                              (trc-type-mismatching-sum-error trc src t1 t2
                                                              etype
                              )
                          )
                      )

                      ((type-width-definite? t1) ;; with t2 indefinite.
                         (conc2-make-indefinite-arg-definite!
                                   trc src t1 t2 expr2 etype
                         )
                      )

                      ((type-width-definite? t2) ;; with t1 indefinite.
                         (conc2-make-indefinite-arg-definite!
                                   trc src t2 t1 expr1 etype
                         )
                      )

                      (else etype) ;; both t1 & t2 indefinite
                )
                etype ;; etype cannot be changed anymore, as already definite.
              )
           )
;; For the rest of clauses, etype is nondefinite, that is, we consider
;; the four latter cases from above:
;; With DEF+DEF, the result (etype) will come definite as well:
;; here, if width-of-t1+t2 > ewidth,
;; the second call to trc-types-match!? in resolve-expr-type-with-genform
;; (i.e. that  (trc-types-match!? trc src restype) )
;; (resolve-expr-type-with-genform which invoked this typeresolving form)
;; will catch cases where width-of-t1+t2 > old indefinite width of src.
;; 
           ((and (type-width-definite? t1) (type-width-definite? t2))
               (if (type-completely-unresolved? etype) ;; XXX - kludge
                   (type-i-of-definite-width width-of-t1+t2)
                   (type-overwrite-with-definite-width etype width-of-t1+t2)
               )
           )

           ((type-completely-unresolved? etype) ;; XXX - kludge
               (type-i-of-atleast-width width-of-t1+t2)
           )

           (else ;; Otherwise, keep it indefinite, and set its width
;;                  larger only in case the sum of t1 and t2 > its old width.
               (type-overwrite-width etype (max ewidth width-of-t1+t2))
           )
     )
  )
)

;; We must check that def<=etype, and indef<=(etype-def), otherwise we
;; we have mismatching sum error:
(define (conc2-make-indefinite-arg-definite! trc src def indef indef-texp etype)
 (let ((etype-def (- (type-width-of etype) (type-width-of def))))
   (cond ((or (> (type-width-of def) (type-width-of etype))
              (> (type-width-of indef) etype-def)
          )
             (trc-type-mismatching-sum-error trc src def indef etype)
         )
         (else ;; It's OK we can fix the indefinite arg's (indef's) width:
           (let ((new-type (type-overwrite-with-definite-width indef
                                                               etype-def
                           )
                ))
             (trc-change-type-and-update-changes! trc indef-texp new-type)
           )
         )
   )
 )
)


(define-typeresolving-form ((regular 0.1 draft) <generic-shift> src trc etype)
 (let ((t1 (resolve-expr-type (arg-first src) trc etype))
       (t2 (resolve-expr-type (arg-second src) trc (texp-type (arg-second src))))
      )
    t1
 )
)


;; (bit expr bit-index) The second arg does not need to be constant in Verilog.
;; We don't need to know either argument's width.
;; This should be applicable to signals of any type (also output and inout)?
;; So we inherit the sigtype of the result from arg1.
(define-typeresolving-form ((regular 0.1 draft) bit src trc etype)
 (let ((t1 (resolve-expr-type (arg-first src) trc (texp-type (arg-first src))))
       (t2 (resolve-expr-type (arg-second src) trc (texp-type (arg-second src))))
      )
    (type-overwrite-with-definite-width t1 1)
 )
)

;;
;; bits has inclusive limits.
;;

;;
;; XXX - Dilemma:
;;   When we have non-constant limits, like for example:
;;    (- r (bits divider (w-1 orgdiv) 0))
;;   then we cannot give any definite width for bits-form,
;;   UNLESS of course, etype (here r) already has a definite width
;;   Now, type-resolver might silently replace all width-related
;;   calls like (w ...) and (w-1 ...) with the corresponding
;;   atleast-widths (integer constants),
;;   but on the other hand, this should not be done, UNTIL
;;   enough passes have been made, until there are no further
;;   changes. (Definite widths can be replaced right away, though.)
;;   So, not until then we will know the limits of bits, which
;;   would then give a definite width for it, and thus also for 'r'
;;   in the above example.
;;
;;   One possible solution: allow indefinite sized indices / drop-n value
;;   in bits and drop. However, this would require adding
;;   "atmost widths" to the type-schema, which would make everything
;;   unnecessary complex. (That is, because if we drop from a definite
;;   width x atleast n bits, then the resulting type is at most x-n
;;   bits wide.)

;;   Another solution: type-resolve multiple passes, until no more
;;   changes occur, with (w ...) forms not "evaluated" before that,
;;   and when they are, then continue type-resolving again,
;;   multiple passes.


(define-typeresolving-form ((regular 0.1 draft) bits src trc etype)
  (cond ((not (= 4 (t-length src)))
           ((trc-error-printer trc)
 "bits must have exactly three arguments: " src
           )
        )
        (else
         (let try-again ()
          (let* ((arg1 (arg-first src))
                 (arg2 (arg-second src))
                 (arg3 (arg-third src))
                 (t1 (resolve-expr-type arg1 trc (texp-type arg1)))
                )
             (cond ((and (resolved-int? arg2)  (resolved-int? arg3))
                       (let ((upl (texp-elem arg2)) (lol (texp-elem arg3)))
                         (begin
;; Should we also allow here upl=lol-1 for "0-bit slices", for some special
;; use with WIRM-macros?
                          (cond ((or (< lol 0)
                                     (< upl lol) ;; (upl=lol for 1-bit slices.)
                                 )
                                   (trc-type-invalid-bits-uplim-and-lowlim
                                          trc src etype
                                   )
                                )
                                ((>= upl (type-width-of t1))
                                   (if (type-width-definite? t1)
                                       (trc-type-invalid-bits-uplim-error
                                           trc src t1 upl etype
                                       )
                                       (trc-change-type-and-update-changes!
                                            trc arg1
                                            (type-overwrite-width t1 (+ 1 upl))
                                       )
                                   )
                                )
                          )
                          (type-overwrite-with-definite-width t1 (+ 1 (- upl lol)))
                         )
                       )
                   )
                   (else ;; See the comments at the end of resolver for drop.
                     (resolve-expr-type arg2 trc (type-i-of-atleast-width 1))
                     (resolve-expr-type arg3 trc (type-i-of-atleast-width 1))
                     etype

;;                   (if (not (resolved-int? arg2))
;;                    (begin
;;                     (resolve-expr-type arg2 trc (type-i-of-atleast-width 1))
;;                     (expwirms-in-place! (trc-parent-toc trc)
;;                                         arg2
;;                                         (trc-wirm-depth trc)
;;                     )
;;                    )
;;                   )

;;                   (if (not (resolved-int? arg3))
;;                    (begin
;;                     (resolve-expr-type arg3 trc (type-i-of-atleast-width 1))
;;                     (expwirms-in-place! (trc-parent-toc trc)
;;                                         arg3
;;                                         (trc-wirm-depth trc)
;;                     )
;;                    )
;;                   )
;;
;;                   (if (or (not (resolved-int? arg2))
;;                           (not (resolved-int? arg3))
;;                       )
;;                      ((trc-error-printer trc)
;; "The second and third arg of bits must reduce to integers at compile time: "
;;                             src
;;                      )
;;                      (try-again) ;; Ok, got an integer, let's try again:
;;                   )

                   )
             )
          )
         )
        )
  )
)



(define (trc-type-invalid-drop-bits-args trc src etype)
  (begin
     ((trc-logging-printer2 trc) trc 0
         (format #f
"TYPE MISMATCH: the number of bits to drop invalid in call to drop-bits (drop). Expected type (width) of result=~s. SRC: "
                (type-bin->ext etype)
         )
         src
     )
     ((trc-typeresolve-exit trc) #f) ;; Throw back #f as we failed now.
  )
)

;; This is just for testing. The actual implementation (and name)
;; might be different.
;; In principle (drop x n) is equal to (bits x (-1+ (WIDTH x)) n)
;; (So we might implement this as a WIRM-macro later.)

;;
;; Four cases, regarding the definiteness of etype and type of arg1 (t1):
;; (Here we drop n bits from arg1).
;;
;;    etype           t1 
;;     DEF            DEF    Should hold: width(t1) = width(etype)+n
;;                           (i.e. width(etype) = width(t1)-n)
;;                           
;;
;;    INDEF           DEF    Should hold: width(t1) >= width(etype)+n
;;                           (i.e. width(etype) <= width(t1)-n)
;;                           (Implies that n >= width(t1) as etype cannot
;;                            have negative width).
;;                           (Fix etype to definite width(t1)-n.)
;;
;; In those first two cases, clearly we must have n>=width(t1).

;;
;;     DEF           INDEF   Should hold: width(t1) <= width(etype)+n
;;                           (i.e. n >= width(t1) - width(etype) )
;;                           in that case, fix arg1 to definite width(etype)+n 
;;                           otherwise, a type mismatch.
;;
;;    INDEF          INDEF
;;                           If width(t1) < width(etype)+n,
;;                           then raise it to width(etype)+n.
;;                           (i.e. if width(etype) is still say 1,
;;                           and width(t1) is 3, and n is 10,
;;                           then width(t1) should be made 11.
;;                           (Here we have case where n > width(t1)).
;;
;;                           If width(t1) > width(etype)+n
;;                           (i.e. width(etype) < width(t1)-n),
;;                           then raise width(etype) to width(t1)-n.
;;                           (If width(t1) == width(etype)+n, then OK already).
;;


(define-typeresolving-form ((regular 0.1 draft) drop src trc etype)
 (cond
   ((not (= 3 (t-length src)))
       ((trc-error-printer trc)
 "drop must have exactly two arguments: " src
       )
   )
   (else
    (let try-again ()
     (let* ((arg1 (arg-first src))
            (arg2 (arg-second src))
            (t1 (resolve-expr-type arg1 trc (texp-type arg1)))
            (t1width (type-width-of t1))
            (ewidth (type-width-of etype))
           )
       (cond ((resolved-int? arg2) =>
                (lambda (drop_n)
                   (cond ((type-width-definite? t1)
;; To be sure, catch cases where n > definite width(t1) here:
                           (cond ((> drop_n t1width)
                                       (trc-type-invalid-drop-bits-args
                                               trc src etype
                                       )
                                 )
;; If the width(etype) changes because of this, there will be type mismatch
;; raised by trc-types-match!? called by resolve-expr-type after this one:
                                 ((type-width-definite? etype)
                                    (type-overwrite-width etype
                                                          (- t1width drop_n)
                                    )
                                 )
                                 (else ;; The case INDEF DEF from above.
                                       (if (<=  ewidth (- t1width drop_n))
;; i.e. same as                            (>= t1width (+ ewidth drop_n))
;; Here we let etype (possibly completetely unresolved) inherit its
;; sigtype, etc. from t1:
                                           (type-overwrite-with-definite-width
                                                  t1 (- t1width drop_n)
                                           )
                                           (trc-type-invalid-drop-bits-args
                                               trc src etype
                                           )
                                       )
                                 )
                           )
                         )

                         (else ;; arg1 is of atleast width (indefinite)
                           (cond ((type-width-definite? etype)
                                    (begin
;; I.e. if  t1width <= (ewidth+drop_n), then t1width-drop_n <= ewidth
                                      (if (<= t1width (+ ewidth drop_n))
                                          (trc-change-type-and-update-changes!
                                                trc arg1
                                            (type-i-of-definite-width
                                                      (+ ewidth drop_n)
                                            )
                                          )
                                          (trc-type-invalid-drop-bits-args
                                                                trc src etype
                                          )
                                      )
                                      etype ;; is already of definite width.
                                    )
                                 )
                                 (else ;; They are both indefite, anything goes
                                   (cond ((< t1width (+ ewidth drop_n))
                                           (trc-change-type-and-update-changes!
                                                trc arg1
                                                (type-i-of-atleast-width
                                                        (+ ewidth drop_n)
                                                )
                                           )
;;                                         (type-i-of-atleast-width
                                              etype ;; no changes in width.
;;                                         )
                                         )
                                         ((> t1width (+ ewidth drop_n))
                                           (type-i-of-atleast-width
                                                      (- t1width drop_n)
                                           )
                                         )
                                         (else ;; width(t1) == width(etype)+n
;;                                         (type-i-of-atleast-width
                                              etype ;; no changes in width.
;;                                         )
                                         )
                                   )
                                 )
                           )
                         )
                   )
                )
             )
             (else ;; Non-constant 2nd argument?
;; Our only hope is that it is a call to a wirm-macro or other
;; expression which reduces to an integer:
                (begin
;;                (resolve-expr-type arg2 trc (type-i-of-atleast-width 1))
;;                (expwirms-in-place! (trc-parent-toc trc)
;;                                    arg2
;;                                    (trc-wirm-depth trc)
;;                )
;;
;;                (if (not (resolved-int? arg2))
;;                    ((trc-error-printer trc)
;; "The second argument of drop must reduce to an integer at compile time: "
;;                           src
;;                    )
;;                    (try-again) ;; Ok, got an integer, let's try again:
;;                )


;; XXX -- our first cut. Not necessarily a most appropriate time and place
;; to call expwirms: (Really, not a working approach. Commented out):
;;                (expwirms-in-place! (trc-parent-toc trc)
;;                                    arg2
;;                                    (trc-wirm-depth trc)
;;                )
;;
;;                (if (not (resolved-int? arg2))
;;                    ((trc-error-printer trc)
;; "The second argument of drop must reduce to an integer at compile time: " src
;;                    )
;;                    (try-again) ;; Ok, got an integer, let's try again:
;;                )
;;                (trc-type-invalid-drop-bits-args trc src etype)

;; Instead, try to resolve the type of arg2, which automatically adds
;; any wirm-macro call present to a queue of yet-to-be-expanded macros:
;; and for now, just return the etype we were supplied, and then let's hope
;; that the type-resolving will set to a fixed point (even with some missing
;; information), and after that, when the wirm-macro is eventually expanded,
;; it will reduce to an integer we need, and _then_, when we do additional
;; passes over the source, the etype from here will be right.
;; So it's mandatory that in typeresolve-until-no-unexpanded-wirms-remain!
;; we DO loop back, that the whole source will be typeresolved AGAIN,
;; after any wirm-expansions.
;; XXX -- Bug: this still works only if the outermost function of arg2
;; is a wirm-macro, but not if it is for example (>> ... 1)
;; or other arithmetic expression.
                  (resolve-expr-type arg2 trc (type-i-of-atleast-width 1))
                  etype

               )
             )
       )
     )
    )
   )
 )
)

