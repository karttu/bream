
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                 ;;
;; BREAM / utilits1.scm (general utility functions)                ;;
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
;; Edited    May 22 2011 by karttu.
;;   Added nonnegatives_least_binwidth and negatives_least_binwidth
;;
;; Edited    Jul 22 2011 by karttu.
;;   Added assoc-from-contlist.
;;
;; Edited    Jul 27 2011 by karttu.
;;   Added compose-funs, int-not and int-xnor
;;
;; Edited    Aug 28 2011 by karttu.
;;   Added make-directory-unless-it-already-exists
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-directory-unless-it-already-exists dirname)
   (cond ((not (file-exists? dirname)) (make-directory dirname)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compose-funs . funlist)
  (cond
    ((null? funlist) (lambda (x) x))
    (else (lambda (x) ((car funlist) ((apply compose-funs (cdr funlist)) x))))
  )
)


(define (length1? s) (null? (cdr s)))


(define (assoc* key a-list-or-false) ;; Allows the second arg. to be also #f
        (and (pair? a-list-or-false) (assoc key a-list-or-false))
)


(define (assoc-from-contlist key a-list)
   (let searchloop ((a a-list))
        (cond ((null? a) #f)
              ((null? (cdr a)) ;; The last pair of a-list?
;; Then continue searching with the provided fall-back list.
                     (searchloop (cdar a))
              )
              ((equal? (caar a) key) (car a)) ;; Found the pair.
              (else (searchloop (cdr a)))
        )
   )
)


;; This one is borrowed from Franz lisp, like destructive cons.
;; (I don't like set!, but I do like attach!)
;; Maybe the name should be push! ???

(define (attach! elem lista)
   (set-cdr! lista (cons (car lista) (cdr lista)))
   (set-car! lista elem)
   lista
)


(define (pop! lista)
  (let ((eka (first lista)))
     (set-car! lista (cadr lista))
     (set-cdr! lista (cddr lista))
     eka
  )
)

;; Note that (pop! (attach! item lista)) = item, and lista will be
;; same as it was before the call to attach!



(define (iota0 upto_n)
   (let loop ((n upto_n) (result (list)))
      (cond ((zero? n) (cons 0 result))
            (else (loop (- n 1) (cons n result)))
      )
   )
)

(define (iota1 upto_n)
   (let loop ((n upto_n) (result (list)))
      (cond ((zero? n) (cons 1 result))
            (else (loop (- n 1) (cons (1+ n) result)))
      )
   )
)


(define (string-search-forward-ci pattern string)
  (let ((len (string-length string)))
    (let loop ((i 0))
         (cond ((> i len) #f)
               ((string-prefix-ci? pattern (string-tail string i)) i)
               (else (loop (1+ i)))
         )
    )
  )
)



(define (symbol-with-num sym num)
  (symbol-append (symbol sym) '__ (string->symbol (format #f "~a" num)))
)


(define (list-to-string orglista inelem)
  (let loop ((s "")
             (lista orglista)
            )
      (cond ((null? lista) s)
            ((eq? lista orglista) ;; First elem?
               (loop (string-append s (format #f "~a" (car lista)))
                     (cdr lista)
               )
            )
            (else ;; Not the first elem, concatenate inelem between.
               (loop (string-append s (format #f "~a~a" inelem (car lista)))
                     (cdr lista)
               )
            )
      )
  )
)

(define (nonnegatives_least_binwidth n) ;; A070939 Length of binary representation of n.
  (if (zero? n) 1
      (let loop ((n n) (i 0))
           (if (zero? n)
               i
               (loop (floor->exact (/ n 2)) (1+ i))
           )
      )
  )
)

;; For two's-complement notation:
(define (negatives_least_binwidth n)
   (+ 1 (nonnegatives_least_binwidth (- -1 n)))
)




(define (obtain-integer-bitwise-function bit-string-FUN)
  (lambda (x y)
    (let ((size (max (nonnegatives_least_binwidth x)
                     (nonnegatives_least_binwidth y)
                )
          )
         )
      (bit-string->unsigned-integer
        (bit-string-FUN (unsigned-integer->bit-string size x)
                        (unsigned-integer->bit-string size y)
        )
      )
    )
  )
)


(define int-or   (obtain-integer-bitwise-function bit-string-or))
(define int-xor  (obtain-integer-bitwise-function bit-string-xor))
(define int-and  (obtain-integer-bitwise-function bit-string-and))
(define (int-not n) (-1+ (- n)))
(define int-xnor (compose-funs int-not int-xor))

