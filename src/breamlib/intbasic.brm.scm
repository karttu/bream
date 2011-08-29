;;
(-MODULE-INFO-BEGIN-)
;;
;; intbasic.brm.scm - The most basic integer functions not provided by Verilog.
;;
;; Copyright (C) 2010-2011 Antti Karttunen, subject to the terms of the GPL v2.
;; See the file COPYING for more information.  Contact the author for
;; further developments at the address: <his-firstname>.<his-surname>@gmail.com
;;
;; This module should contain only implementations of integer functions
;; (both define's and define-wirm's are OK) which have been tested
;; (at least rudimentarily) to work as expected on real FPGA platform.
;; Also, the implementation should be "plausible" in sense, that
;; it gives correct results with a modest footprint of space and time.
;; There is a separate module  intfuns-nonpractical.brm.scm
;; for functions that return correct results, but whose implementation
;; is currently very far from optimal.
;;
(-MODULE-INFO-END-)
;;
;; CHANGES
;;
;; Started writing this file Aug 25 2010.
;;
;; Edited    Aug 28 2011 by karttu.
;;   Moved both wholly untested as well as working but totally unpractical
;;   definitions (except for test-sets) to separate modules:
;;   intfuns-untested.brm.scm and intfuns-nonpractical.brm.scm
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; With the current version of zeqw-1 macro this compiles only
;; when orgdiv is specified with a definite width, for example,
;; as a constant 4'10. (The commented out initializations with zow
;; macro do not need are happy with atleast widths as well.)

(define (remainder dividend orgdiv)
   (let loop ((r (conc (zeqw-1 orgdiv) dividend))
              (divider (conc orgdiv (zeqw-1 dividend)))
;;            (r (conc (zow (ww-1 orgdiv)) dividend))
;;            (divider (conc orgdiv (zow (ww-1 dividend))))
             )
        (cond ((logor (zero? divider) (< r (zxt orgdiv)))
                 (bits r (ww-1 orgdiv) 0)
              )
              ((> divider r) (loop r (>> divider 1)))
              (else (loop (- r divider) (>> divider 1)))
        )
   )
)



(define (quotient dividend orgdiv)
   (let loop ((i (ww dividend))
              (q (zeqw dividend)) ;; q is a zero of width dividend.
              (r (conc (zeqw-1 orgdiv) dividend))
              (divider (conc orgdiv (zeqw-1 dividend)))
             )
        (cond ((logor (zero? i) (zero? orgdiv)) q)
              ((> divider r)
                    (loop (-1+ i) (<< q 1) r (>> divider 1))
              )
              (else (loop (-1+ i) (+ (<< q 1) 1) (- r divider) (>> divider 1)))
        )
   )
)

;; These versions of remainder and quotient work only
;; when msb of orgdiv is 1.
;; E.g. one can use them with orgdiv argument 4'10
;; for example. (Of course there are even more optimized
;; routines for dividing with a constant ten.)

(define (remainder_by_msb1_divisor dividend orgdiv)
   (let loop ((r dividend)
              (divider (conc orgdiv (zow (- (ww dividend) (ww orgdiv)))))
             )
        (cond ((logor (zero? divider) (< r (zxt orgdiv)))
                 (bits r (ww-1 orgdiv) 0)
              )
              ((> divider r) (loop r (>> divider 1)))
              (else (loop (- r divider) (>> divider 1)))
        )
   )
)

;; An auxiliary wirm-macro just for the quotient_by_msb1_divisor:
(define-wirm (diff-of-widths+1 dividend orgdiv) (- (w dividend) (w-1 orgdiv)))

(define (quotient_by_msb1_divisor dividend orgdiv)
   (let loop ((i (diff-of-widths+1 dividend orgdiv))
              (q (zeqw dividend)) ;; q is a zero of width dividend.
              (r dividend)
              (divider (conc orgdiv (zow (- (ww dividend) (ww orgdiv)))))
             )
        (cond ((logor (zero? i) (zero? orgdiv)) q)
              ((> divider r)
                    (loop (-1+ i) (<< q 1) r (>> divider 1))
              )
              (else (loop (-1+ i) (+ (<< q 1) 1) (- r divider) (>> divider 1)))
        )
   )
)



(define (gcd a b)
   (cond ((zero? b) a)
         ((>= a b) (gcd b (- a b)))
         (else (gcd (- b a) a))
   )
)
