
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                  ;;
;; BREAM / combopti.scm                                             ;;
;;   --  Optimizing-forms for the Level0 Combinational Logic.       ;;
;;       Used by compile module to simplify some of the             ;;
;;       Level0 expressions it generates.                           ;;
;;                                                                  ;;
;; The entry point is (optimize-combinational-level0-code src coc)  ;;
;; which will optimize the src-expression, if it can, and otherwise ;;
;; return it back as it was.                                        ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Unless otherwise mentioned, all the files in this code tree are
;; Copyright (C) 2010-2011 Antti Karttunen, subject to the terms of the GPL v2.
;; See the file COPYING for more information.  Contact the author for
;; further developments at the address: <his-firstname>.<his-surname>@gmail.com
;;

;;
;; Antti Karttunen ("karttu") started this module Sep 18 2010,
;; by transferring optimize-ternary-if and optimize-combinational-level0-code 
;; from module compile1.scm and the dispatching code and macro definitions
;; from expsynta.scm

;;
;; Edited    Sep 19 2010 by karttu.
;;   Added reductions:
;;    (bitand ... 0 ...) -> 0
;;    (bitor ... non-zero-int ...) -> 1
;;
;; Edited    Oct 10 2010 by karttu.
;;   Realized that the latter reduction is wrong for bitor. Didn't fix it yet.
;;
;; Edited    Oct 28 2010 by karttu.
;;   Added one more kludgous argument for generic-dispatch.
;;
;; Edited    Nov 04 2010 by karttu.
;;    All cc stuff renamed coc (= compiling context, not current continuation!)
;;
;; Edited    May 25 2011 by karttu.
;;   Added the current year to the copyright notice.
;;
;; Edited    Aug 25 2011 by karttu.
;;   Edited bitand so that (bitand) (without args) gives back 1.
;;   (Needed when invoking functions with no arguments).
;;

;;
;; XXX - Todo: Any more optimizations needed?
;; E.g. we could have
;; ((0 ==  and2_left_17) ? and2_left_17 : (i_3 > 1));
;; --> ((0 ==  and2_left_17) ? 0 : (i_3 > 1));
;;
;; In general:
;;   (?: (== X <lit>) X Z) --> (?: (== X <lit>) <lit> Z)
;;   (?: (== <lit> X) X Z) --> (?: (== X <lit>) <lit> Z)
;;
;;   (?: (!= <lit> X) Z X) --> (?: (!= <lit> X) Z <lit>)
;;   (?: (!= X <lit>) Z X) --> (?: (!= X <lit>) Z <lit>)
;;


(define *optimized-lev0-forms* (list (list '(regular 0.1 draft) (list (list (list))))))



(define (coc-list-of-optimized-lev0-forms-in-use coc)
  (list-of-forms-by-dic (coc-calling-convention-in-use coc)
                        (coc-optimized-lev0-forms coc)
  )
)


;; Called as:
;; (optimize-combinational-level0-code src coc)

(define (optimize-combinational-level0-code src coc)
   (generic-dispatch src

                     src

                     (list-of-forms-by-dic
                             (coc-calling-convention-in-use coc)
                             *optimized-lev0-forms*
                     )

                     first

                     (lambda (x) x)

                     (lambda (x coc) x)

                     (lambda (x coc) x)

                     coc
   )
)




(define-syntax define-optimized-lev0-form
   (syntax-rules ()
     ((define-optimized-lev0-form (CACO formname src coc) body)
        (attach!
             (cons (quote formname) (lambda (src coc) body))
             (list-of-forms-by-dic (quote CACO) *optimized-lev0-forms*)
        )
     )
   )
)


(define-optimized-lev0-form ((regular 0.1 draft) ?: s coc)
  (cond ((not (= 4 (length s)))
           (error
 "?: (lev0-optimizer): Form must have exactly three subexpressions: " s
           )
        )

        (else
          (let ((test-expr (optimize-combinational-level0-code (second s) coc))
                (then-expr (optimize-combinational-level0-code (third s) coc))
                (else-expr (optimize-combinational-level0-code (fourth s) coc))
               )
            (cond ((equal? 1 test-expr) then-expr) ;; Then it's always then.
                  ((equal? 0 test-expr) else-expr)
;; Doesn't matter which one:
                  ((equal? then-expr else-expr) then-expr)
                  (else `(,(car s) ,test-expr ,then-expr ,else-expr))
            )
          )
        )
  )
)



(define-optimized-lev0-form ((regular 0.1 draft) bitand s coc)
  (cond ((null? (cdr s)) 1) ;; (bitand) without args is always true.
;;         (error
;; "bitand (lev0-optimizer): Form must have at least one argument: " s
;;           )

        (else
          (let* ((subexprs
                   (map (lambda (x) (optimize-combinational-level0-code x coc))
                        (cdr s)
                   )
                 )
                 (wod (delete 1 subexprs)) ;; wod = with-ones-deleted
                )
             (cond ((memq 0 wod) 0) ;; If any zero present, forces to zero.
                   ((null? wod) 1) ;; (bitand) -> 1
                   ((null? (cdr wod)) (first wod)) ;; (bitand any) --> any
                   (else (cons (first s) wod))
             )
          )
        )
  )
)


(define-optimized-lev0-form ((regular 0.1 draft) bitor s coc)
  (cond ((null? (cdr s))
           (error
 "bitor (lev0-optimizer): Form must have at least one argument: " s
           )
        )

        (else
          (let* ((subexprs
                   (map (lambda (x) (optimize-combinational-level0-code x coc))
                        (cdr s)
                   )
                 )
                 (wzd (delete 0 subexprs)) ;; wzd = with-zeros-deleted
                )
             (cond ((there-exists? wzd ;; XXX -- True for logor, not bitor?!
                          (lambda (x) (and (integer? x) (not (zero? x))))
                    ) 1) ;; if any non-zero integer present, forces true.
                   ((null? wzd) 0) ;; (bitor) -> 0
                   ((null? (cdr wzd)) (first wzd)) ;; (bitor any) --> any
                   (else (cons (first s) wzd))
             )
          )
        )
  )
)


