
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                 ;;
;; BREAM / dispatch.scm                                            ;;
;;   --  Generic functions for dispatching to an appropriate       ;;
;;       function, based on Scheme form and calling convention/    ;;
;;       dictionary in use.                                        ;;
;;                                                                 ;;
;; Used by many modules: compile1, typreslv, expsynta, lev0veri    ;;
;;                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Unless otherwise mentioned, all the files in this code tree are
;; Copyright (C) 2010-2011 Antti Karttunen, subject to the terms of the GPL v2.
;; See the file COPYING for more information.  Contact the author for
;; further developments at the address: <his-firstname>.<his-surname>@gmail.com
;;

;;
;; Antti Karttunen ("karttu") started writing this module at Sep 18 2010.
;; (Generalized a few functions already defined in compile1.scm and
;; lev0veri.scm)
;;
;; Edited    Oct 28 2010 by karttu.
;;   Added one more kludgous argument, get-search-symbol for generic-dispatch.
;;
;; Edited    Nov 20 2010 by karttu
;;   New functions dispatch-to-generic-form, dispatch-search-from-forms
;;   src->search-pat extracted and generalized from some ugly code in
;;   compile1.scm and transferred here.
;;
;; Edited    May 21 2011 by karttu
;;   Changed argument coc in above mentioned three functions to coc-or-trc
;;   and edited src->search-pat to access either coc-labels2loops
;;   or trc-labels2loops, depending on which type of structure it has.
;;   (i.e. whether called from the compiler or by the type resolver.)
;;
;; Edited    May 26 2011 by karttu
;;   New functions dispatch-to-generic-form2 and dispatch-search-from-forms2
;;   for the needs of typreslv.scm
;;
;; Edited    Jul 20 2011 by karttu
;;   Added labels2loops check for rwc's also (i.e. rewriting context,
;;   for expsynta.scm)
;;
;; Edited    Jul 22 2011 by karttu
;;   Modified  dispatch-to-generic-form to call dispatch-to-generic-form2
;;   (Because it was almost identical).
;;
;; Edited    Jul 22 2011 by karttu
;;   Now uses assoc-from-contlist (defined in utilits1.scm) instead of
;;   an ordinary assoc, as to implement a rudimentary form of fall-back
;;   inheritance between calling conventions. (See the definitions in
;;   the beginning of expsynta.scm).
;;

;; dic = dispatcher class/dictionary, whatever:


(define (list-of-forms-by-dic dic toplist-of-forms)
   (cond ((assoc dic toplist-of-forms) => second)
         (else
           (error "Unsupported calling convention/dispatcher class: " dic)
         )
   )
)


;; XXX --- This is starting to reek.

(define (generic-dispatch search-with
                          src
                          forms
                          get-search-symbol ;; Usually first, i.e. car
                          when-found-wrapper
                          when-not-pair-fun
                          when-not-found-fun . rst
        )
    (cond ((not (pair? search-with)) (apply when-not-pair-fun (cons src rst)))

          ((assoc-from-contlist (get-search-symbol search-with) forms)
            => (lambda (name.formfun)
                    (when-found-wrapper
                         (apply (cdr name.formfun) (cons src rst))
                    )
               )
          )

          (else (apply when-not-found-fun (cons src rst)))
    )
)

;; 
;; dispatch-to-generic-form is used both by compile1.scm and typreslv.scm:
;; (Duplicates partly the functionality of above kludgous generic-dispatch.)


(define (src->search-pat src coc-or-trc)
  (cond ((not (t-is-pair? src)) '<terminal-argument>)

;; Our polymorphism in poor man's way:
        ((and (coc? coc-or-trc)
              (assq (t-u-first src) (coc-labels2loops coc-or-trc))
         ) '<loop-tail-call>
        )

        ((and (trc? coc-or-trc)
              (assq (t-u-first src) (trc-labels2loops coc-or-trc))
         ) '<loop-tail-call>
        )

;; Added Jul 20 2011:
        ((and (rwc? coc-or-trc)
              (assq (t-u-first src) (rwc-labels2loops coc-or-trc))
         ) '<loop-tail-call>
        )

        ((pair? (t-u-first src)) ;; E.g. it's probably ((lambda (x y z) ..) ..)
          (if (equal? 'lambda (t-u-first (t-first src)))
                      (list (t-u-first (t-first src))) ;; (lambda)
                      '(<generic-funcall>)
          )
        )

        (else (t-u-first src))
  )
)






;; If (src->search-pat src coc-or-trc) is not found among compiled-forms
;; or type-resolving-forms, then one should search it once more
;; with '<generic-funcall>:


(define (dispatch-to-generic-form src coc-or-trc searched-forms extra-one)
    (dispatch-to-generic-form2 #f src coc-or-trc searched-forms extra-one)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If the extra-argument formname is given as something else than #f,
;; then it is used "as a functor name" instead of the original first
;; element of src.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (dispatch-search-from-forms2 formname src coc-or-trc searched-forms)
 (cond ((and formname (assoc-from-contlist formname searched-forms)))
       ((assoc-from-contlist (src->search-pat src coc-or-trc) searched-forms))
       ((assoc-from-contlist '<generic-funcall> searched-forms))
       (else (error "dispatch-search-from-forms2: Shouldn't happen! src=" src))
 )
)


(define (dispatch-to-generic-form2 formname src coc-or-trc searched-forms extra-one)
   (cond ((dispatch-search-from-forms2 formname src coc-or-trc searched-forms)
           => (lambda (name.compiler)
                   ((cdr name.compiler) src coc-or-trc extra-one)
              )
         )
         (else #f)
   )
)


