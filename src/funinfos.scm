;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                  ;;
;; BREAM / funinfos.scm                                             ;;
;;   --  Code for organizing function definitions & bodies          ;;
;;       at the various stages of compilation and type-resolving.   ;;
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
;; Antti Karttunen ("karttu") started this module Nov 27 2010,
;; by transferring (old) funinfo-structure from module readdefs.scm
;;

;; Edited    May 19 2011 by karttu.
;;    Removed coc argument from funinfo-sexpanded-source
;;
;; Edited    Jun 02 2011 by karttu.
;;    Added deftype, so we can see what kind of definition we have.
;;    (Well, I forgot that it is already contained in org-definition.
;;     But maybe it's handy to have it also separately.)
;;
;; Edited    Jul 27 2011 by karttu.
;;    Added toc arguments to all defined functions. (To be passed to expsynta).
;;

(define-structure (funinfo (keyword-constructor))
    deftype        ;; Currently define, define-wirm or declare-extfun

    funname

    org-definition ;; an unannotated/partially annotated definition read
;;                    straight from source file.

    exp-definition ;; a copy of org-definition after expansion of 'cond', etc

    typed-definitions ;; An assoc-list of (type-signature . t-expr-tree) pairs,
;;                       where each t-expr-tree is a copy of exp-definition
;;                       fully type-annotated according to the type-signature.

;; Later:
;;  rows ;; an inclusive range [beginning_row,ending_row], for debugging.

    moi ;; "pointer" to a module-info structure which contains this fun def.
)

;; Return a cached copy of the "Scheme-expanded" source:
(define (funinfo-sexpanded-source toc fis)
    (or (funinfo-exp-definition fis) ;; Already expanded?
        (begin (set-funinfo-exp-definition! ;; No. Do it now:
                      fis
                      (expsynta toc (funinfo-org-definition fis))
               )
               (funinfo-exp-definition fis) ;; And return the result:
        )
    )          
)

;; Return a cached copy of the type-resolved source with type-signature tsig:
(define (funinfo-type-resolved-source toc fis tsig coc)
  (cond ((assoc* tsig (funinfo-typed-definitions fis)) => cdr) ; Already there?
        (else
           (let ((tdefs (funinfo-typed-definitions fis))
                 (new-pair (cons tsig
                                 (resolve-expr-type
                                        (texp-tree-copy
                                            (funinfo-sexpanded-source toc fis)
                                        )
                                        coc
                                        (first tsig) ;; is etype???
                                 )
                           )
                )
               )
             (if (not tdefs)
                 (set-funinfo-typed-definitions! fis (list new-pair))
                 (attach! new-pair tdefs)
             )
           )
        )
  )
)

