
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                      ;;
;; BREAM / combfscm.scm (combinational bream functions implemented      ;;
;;                       in Scheme, for the needs of expwirms.scm)      ;;
;; I.e. these are definitions for the functions that are executed       ;;
;; at the compile time. (Usually in wirm-macros.)                       ;;
;;                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Unless otherwise mentioned, all the files in this code tree are
;; Copyright (C) 2010-2011 Antti Karttunen, subject to the terms of the GPL v2.
;; See the file COPYING for more information.  Contact the author for
;; further developments at the address: <his-firstname>.<his-surname>@gmail.com
;;

;;
;; Antti Karttunen ("karttu") started writing this module Jul 27 2011.
;;

;;
;; Edited    Aug 24 2011 by karttu.
;;   Replaced the definition of bitnot (redand and rednand also use it)
;;   with a temporary error message, until we take the width of
;;   expressions properly into account. (Not a hard thing to do, actually.)
;;



(define (lognot b) (not b))

(define (logand . args)
  (let loop ((args args))
        (cond ((null? args) #t)
              ((not (car args)) #f)
              (else (loop (cdr args)))
        )
  )
)


(define (logor . args)
  (let loop ((args args))
        (cond ((null? args) #f)
              ((not (not (car args))) #t)
              (else (loop (cdr args)))
        )
  )
)


(define (bool->int b) (if b 1 0))

(define reduce-left reduce)

(define (bitand . args)  (reduce-left int-and 0 args))
(define (bitor . args)   (reduce-left int-or 0 args))
(define (bitxor . args)  (reduce-left int-xor 0 args))
(define (bitnot arg1)    (error "bitnot not yet implemented for compile-time reduction, because it needs also width of its arg to be known!")) ;; (int-not arg1)
(define (bitxnor . args) (reduce-left int-xnor 0 args))

;; These two are probably not well-defined in integer-literal context
;; in wirm-expansion:
(define (redand b) (bool->int (zero? (bitnot b))))
(define (rednand b) (bool->int (nonzero? (bitnot b)))) ;; XXX -- Is it this?!

(define (redor b) (bool->int (nonzero? b)))
(define (rednor b) (bool->int (zero? b)))

(define (redxor n) ;; reduced xor, i.e. the parity bit, the Thue-Morse seq.
  (let loop ((n n) (i 0))
     (if (zero? n) i (loop (>> n 1) (bitxor i (bitand n 1))))
  )
)

(define (redxnor n) (bitnot (redxor n)))


(define (nonzero? i) (not (zero? i)))

(define (pow2? x) (logand (redor x) (rednor (bitand x (-1+ x)))))


(define (<< n i) (if (<= i 0) (>> n (- i)) (<< (+ n n) (- i 1))))
(define (>> n i) (if (zero? i) n (>> (floor->exact (/ n 2)) (- i 1))))

(define <<< <<) ;; XXX -- Check these later! (The signs for arithmetic shifts!)
(define >>> >>) ;; XXX -- Check these later! (The signs for arithmetic shifts!)

(define (bit n i) (bitand 1 (>> n i))) ;; XXX -- Check these later!
(define (bits n hl ll) (bitand (-1+ (<< 1 (1+ (- hl ll)))) (>> n ll))) ;; XXX!

(define (drop n i) (>> n i))

