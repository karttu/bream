
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                 ;;
;; BREAM / srcstrct.scm                                            ;;
;;   --  Accessor functions for handling type-annotated            ;;
;;       S-expression trees.                                       ;;
;;                                                                 ;;
;;       Used by many modules: compile1, expsynta, <to-be-named>   ;;
;;                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Unless otherwise mentioned, all the files in this code tree are
;; Copyright (C) 2010-2011 Antti Karttunen, subject to the terms of the GPL v2.
;; See the file COPYING for more information.  Contact the author for
;; further developments at the address: <his-firstname>.<his-surname>@gmail.com
;;

;;
;; Antti Karttunen ("karttu") started writing this module at Oct 27 2010.
;;

;;
;; Edited    Oct 28 2010 by karttu.
;;   Added typify, untyped & expr-rest-with-same-type.
;; 
;;
;; Edited    Nov 04 2010 by karttu.
;;   Renamed (expr-* ...) to (t-* ...) and (dt-* ...) to (t-u-* ...)
;;   Now implemented as a real structure, not as a dotted pair.
;;
;; Edited    Nov 20 2010 by karttu.
;;   Additional types, also arg-first, arg-second, etc. forms.
;;

;; Edited    Nov 21 2010 by karttu.
;;   Added texp-tree-copy.
;;
;; Edited    Nov 22 2010 by karttu.
;;   Added types-match? etc (probably in wrong module, move later to typreslv)
;;

;; Edited May 14-15 2011 by karttu.
;;   Added t-callees-fundef and set-callees-fundef!
;;   and callees-full-name-with-sig
;;

;; Edited    May 20 2011 by karttu.
;;   Added (type-output? t). We now specify output-wires as ,(o 1)
;;   or ,(o <bus-width>) given after a function's formal argument.
;;

;; Edited    May 21 2011 by karttu.
;;   Added (texp-print orgtree outport) for better debugging.
;;
;; Edited May 25-27 2011 by karttu.
;;   Now uses the new type scheme (defined in typesch1.scm).
;;   The external format of type-annotated code changed from:
;;     var-or-exp,its-type
;;   to
;;     type'var-or-exp
;;
;;   Added the new element fundef to our texp-structure, as to avoid
;;   the previous kludgy overloaded use of type-field of texp's.
;;
;; Edited    Jun 01 2011 by karttu.
;;   Renamed callees-full-name-with-sig as t-callees-fullname
;;   and fixed a few misfeatures in it.
;;

;; Edited    Jul 20 2011 by karttu.
;;   Added new untyped versions arg-u-first - arg-u-fourth
;;
;; Edited    Jul 22 2011 by karttu.
;;   Renamed some callees-fundef and other macros.
;;

;; Edited    Sep 05 2011 by karttu.
;;   Added find-with-ci-string-from-a-list-of-texps.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (define-structure texp type elem fundef)
(define-structure (texp (keyword-constructor))
    type
    elem
    definfo
)


;; (define (type-et-elem w s) (cons w s))
;; (define (t-elem s) (cdr s))
;; (define (t-type s) (car s))

(define (type-et-elem-et-definfo gtype gelem gdefinfo)
   (make-texp 'type gtype 'elem gelem 'definfo gdefinfo)
)

(define (type-et-elem type elem) (type-et-elem-et-definfo type elem #f))

(define t-elem   texp-elem)
(define t-type   texp-type)
(define t-definfo texp-definfo)


(define (texp-overwrite-in-place! old-texp new-texp)
  (begin
    (set-texp-elem!    old-texp (texp-elem new-texp))
    (set-texp-type!    old-texp (texp-type new-texp))
    (set-texp-definfo! old-texp (texp-definfo new-texp))
    old-texp
  )
)


(define (t-width s) (type-width-of (t-type s)))

(define (t-type-of-definite-width? texp) (type-width-definite? (t-type texp)))


;; This is temporary definition.
;; Instead of integer? we should have something like bream-int?
(define (resolved-int? x) (and (integer? (t-elem x)) (t-elem x)))

(define (t-rest s) (cdr (t-elem s)))

(define t-carglist t-rest) ;; Returns a list of call-arguments.

(define (t-rest-with-same-type s)
   (type-et-elem (t-type s) (t-rest s))
)


(define (t-length s) (length (t-elem s)))

(define (t-length1? s) (length1? (t-elem s)))

(define (t-is-pair? s) (pair? (t-elem s)))
(define (t-is-symbol? s) (symbol? (t-elem s)))
(define (t-is-integer? s) (integer? (t-elem s)))
(define (t-is-boolean? s) (boolean? (t-elem s)))


(define (t-first s) (first (t-elem s)))
;; Returns the unannotated functor-name of type-annotated expression:
(define (t-u-first s) (t-elem (t-first s)))


(define (t-second s) (second (t-elem s)))
(define (t-u-second s) (t-elem (t-second s)))

(define (t-third s) (third (t-elem s)))
(define (t-u-third s) (t-elem (t-third s)))

(define (t-fourth s) (fourth (t-elem s)))
(define (t-u-fourth s) (t-elem (t-fourth s)))

(define (t-fifth s) (fifth (t-elem s)))
(define (t-u-fifth s) (t-elem (t-fifth s)))

(define (t-calls-type s) (t-type s))


(define (set-callees-fundef-and-fis! s fundef fis)
        (set-texp-definfo! s (list fundef fis))
)


(define (t-callees-fundef s) (and (t-definfo s) (first (t-definfo s))))
(define (t-callees-fis s) (second (t-definfo s)))



(define (get-deftype-from-fundef fundef) (t-u-first fundef)) ;; Untyped!

;; Either define, define-wirm or declare-extfun:
(define (t-callees-deftype src)
        (get-deftype-from-fundef (t-callees-fundef src))
)

(define (t-callees-name-and-fargs src)  (t-second (t-callees-fundef src)))


;; Note: Currently, the type for function's return value has to be taken
;; from the body's type-field, as it is not propagated to the type-field
;; of name-and-fargs in the fundef.
;; However, with declare-extfun, it is explicitly specified as
;; in the type-field of name-and-fargs list, and also, in those
;; cases, there are no body, thus we use name-and-fargs -list
;; as a "type carrier", instead of normal body.
;; (declare-extfun 1'(outbyte1200 output'outchan 8'byte_to_output))

;; We use the widths of types (calling type-width-of), instead of
;; raw texp-type codes. Also, in the end, we convert the string
;; to symbol, because currently lev0veri cannot handle strings
;; in the arguments of its instantiate-form. (Might be changed).

(define (t-callees-fullname src)
  (let* ((callees-fundef (t-callees-fundef src))
         (callees-name-and-fargs (t-second callees-fundef))
         (ret-type-carrier (if (< (t-length callees-fundef) 3)
                                  callees-name-and-fargs
                                  (t-third callees-fundef)
                           )
         )
        )
    (string->symbol
       (full-name-with-sig callees-name-and-fargs ret-type-carrier)
    )
  )
)

(define (type-signature lista)
   (map type-width-of (map texp-type lista))
)


(define (full-name-with-sig name-et-fargs ret-type-carrier)
   (list-to-string 
        (cons (t-u-first name-et-fargs)
              (cons (type-width-of (texp-type ret-type-carrier))
                    (type-signature (t-rest name-et-fargs))
              )
        )
        "_"
   )
)


;; If using syntax like
;; (define (1'gcd a b) (bitor a b))
;; (define (1'lcm a b) (bitand a b))

;; or (1'+ a b c)

;; then we have to move the type-specifier (width) from "fundef's location"
;; to calls-type location, sometime when we are reading code in...

(define arg-first t-second)
(define arg-second t-third)
(define arg-third t-fourth)
(define arg-fourth t-fifth)


(define arg-u-first t-u-second)
(define arg-u-second t-u-third)
(define arg-u-third t-u-fourth)
(define arg-u-fourth t-u-fifth)

;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-with-ci-string-from-a-list-of-texps ci-string texp-list)
   (find (lambda (t) (string-ci=? ci-string (symbol->string (t-elem t))))
         texp-list
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;

(define (inherit-types-from-old old-expr new-src-with-only-args-typed)
   (type-et-elem (t-type old-expr)
                 (cons (type-et-elem (t-type (t-first old-expr))
                                     (car new-src-with-only-args-typed)
                       )
                       (cdr new-src-with-only-args-typed)
                 )
   )
)


;; Strips type-annotated source from its type (width) information:

(define (detyped s)
  (cond ((not (pair? (t-elem s))) (t-elem s))
        (else (map detyped (t-elem s)))
  )
)

(define (detyped* s) (detyped (type-et-elem 'dummy-type s)))

(define (old-annotate-with s deftype)
  (cond ((not (pair? s))  (type-et-elem deftype s))
        (else (type-et-elem deftype
                    (map (lambda (x) (old-annotate-with x deftype)) s)
              )
        )
  )
)


(define (typify* s deftype typeindicator)
  (cond ((not (pair? s)) s)
        ((and (pair? (cdr s))
              (pair? (second s))
              (eq? (first (second s)) typeindicator)
         )
              (cons (type-et-elem
                          (type-ext->bin (first s))
                          (typify* (second (second s)) deftype typeindicator)
                    )
                    (typify* (cddr s) deftype typeindicator)
              )
        )
        (else
              (cons (type-et-elem deftype
                                 (typify* (first s) deftype typeindicator)
                    )
                    (typify* (cdr s) deftype typeindicator)
              )
        )
  )
)

(define (typify s deftype typeindicator)
  (type-et-elem deftype (typify* s deftype typeindicator))
)

(define (texp-tree-copy tree) ;; Make a safe copy of texp-tree.
  (cond ((pair? tree)
            (cons (texp-tree-copy (car tree)) (texp-tree-copy (cdr tree)))
        )
        ((texp? tree)
            (type-et-elem-et-definfo
                (texp-tree-copy (t-type tree))
                (texp-tree-copy (t-elem tree))
                (texp-tree-copy (t-definfo tree))
            )
        )
        (else tree) ;; Atomic, probably.
  )
)


(define (texp-trees-equal? tree1 tree2)
  (cond ((and (pair? tree1) (pair? tree2))
            (and (texp-trees-equal? (car tree1) (car tree2))
                 (texp-trees-equal? (cdr tree1) (cdr tree2))
            )
        )
        ((and (texp? tree1) (texp? tree2))
            (and (texp-trees-equal? (t-type tree1) (t-type tree2))
                 (texp-trees-equal? (t-elem tree1) (t-elem tree2))
                 (texp-trees-equal? (t-definfo tree1) (t-definfo tree2))
            )
        )
        (else (equal? tree1 tree2)) ;; Atomic, at least the other, probably.
  )
)



(define (texp-print orgtree outport) ;; Print texp in "human-readable format".
  (begin
   (if (pair? orgtree) (format outport "("))
   (let loop ((tree orgtree))
     (begin
;; Output a blank between, if not the first element of (current sub-)list:
        (cond ((and (pair? tree) (not (eq? tree orgtree)))
                  (format outport " ")
              )
        )
        (cond ((texp? tree)
                  (let ((types-external-habitus (type-bin->ext (t-type tree))))
                      (if types-external-habitus
                          (format outport "~s'" types-external-habitus)
                      )
                  )
                  (texp-print (t-elem tree) outport)
                  (cond ((t-definfo tree)
                           (format outport ":with-cached-fundef=")
                           (texp-print (t-definfo tree) outport)
                        )
                  )
              )
              ((pair? tree)
                  (texp-print (car tree) outport)
                  (cond ((pair? (cdr tree)) (loop (cdr tree)))
                        ((not (null? (cdr tree)))
                           (format outport " . ")
                           (loop (cdr tree))
                        )
                  )
              )
              (else (write tree outport))
        )
     )
   )
   (if (pair? orgtree) (format outport ")"))
  )
)

(define (texp-print-with-nl orgtree outport)
  (begin
     (texp-print orgtree outport)
     (newline outport)
  )
)

(define (texp-sprint tree)
  (call-with-output-string (lambda (port) (texp-print tree port)))
)

;; Makes debugging bearable:
(define (tp s) (texp-print-with-nl s (current-output-port)))

