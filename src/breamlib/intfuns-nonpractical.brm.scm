;;
(-MODULE-INFO-BEGIN-)
;;
;; intfuns-nonpractical.brm.scm -- A collection of integer functions in Scheme,
;; which work, but can be hardly considered to be optimal, or even reasonable
;; implementation of the mathematical function in question.
;;
;; Copyright (C) 2010-2011 Antti Karttunen, subject to the terms of the GPL v2.
;; See the file COPYING for more information.  Contact the author for
;; further developments at the address: <his-firstname>.<his-surname>@gmail.com
;;
;; Most of these functions I have skimmed from the set of functions I wrote
;; to be executed under MIT/GNU Scheme, which are computing various
;; integer sequences in Sloane's Online-Encyclopedia of Integer Sequences
;; That code is available under the files:
;; http://www.iki.fi/kartturi/matikka/Schemuli/intfuns1.scm.txt
;; http://www.iki.fi/kartturi/matikka/Schemuli/intfun_a.scm.txt
;; http://www.iki.fi/kartturi/matikka/Schemuli/intfun_b.scm.txt
;; (See also  http://oeis.org/ for the definition of each A-number.)
;; Many of those definitions are far from optimal, even if intended
;; to be run under an ordinary CPU-based architecture. The goal has
;; been more to check the various mathematical correspondences,
;; and especially, to get correct results.
;;
(-MODULE-INFO-END-)
;;
;; NOTE:
 ;; (We probably need also a separate module for "reasonably implemented,
;;  but very seldomly needed functions".)
;;
;; CHANGES
;;
;;
;; Edited    Aug 28 2011 by karttu.
;;   Separated this module from intbasic.brm.scm
;;


;; Some basic number theoretic functions:
;; phi (A000010) (maybe later also: tau (A000005) and sigma (A000203)) follow.
;; (For more optimal implementations of these, I reckon that we would need
;; to factor n first.)

;; Euler totient function phi(n): count numbers <= n and prime to n.
;; Extremely naive algorithm, just for testing gcd.

(define (A000010_naive_singular n)
  (let loop ((i n) (s (zeqw n))) ;; s <= n, so let's use the same width.
       (cond ((zero? i) s)
             (else
               (let ((a (gcd n i)))
                 (loop (-1+ i)
                       (+ s (zxt (bitand (bit a 0) (rednor (drop a 1)))))
                 )
               )
             )
       )
  )
)


;; Next step: divide this embarrassignly naive summing algorithm exhibiting
;; embarrassing parallelism, into d independent parts,
;; where each part part_k, (k=0..d) is summing its own subset of
;; n, for which n modulo d = k. Note that although here I have
;; used d = 4 and d = 8, the number of parts doesn't need to be
;; 2's power, and actually, in this case, using prime such as 7
;; would probably ensure that the load between parts would be
;; even more evenly distributed.
;; (For now, for instance, when n is a multiple of 4, the parts
;; with even starting offset will probably execute the gcd-algorithm
;; in average little faster (less steps needed).)
;; 
;; Also, use the unrolled version of gcd, possibly hand-optimized.
;;
;; In future we might have a version of unrolled loops or some
;; wirm-macro contraption with which we wouldn't need to write
;; all the calls open by ourselves:

(define (A000010_naive_par4 n)
  (+ (+ (A000010_naive_par_aux n 0 4)
        (A000010_naive_par_aux n 2 4)
     )
     (+ (A000010_naive_par_aux n 1 4)
        (A000010_naive_par_aux n 3 4)
     )
  )
)

(define (A000010_naive_par8 n)
  (+ (+ (+ (A000010_naive_par_aux n 0 8)
           (A000010_naive_par_aux n 2 8)
        )
        (+ (A000010_naive_par_aux n 1 8)
           (A000010_naive_par_aux n 3 8)
        )
     )
     (+ (+ (A000010_naive_par_aux n 4 8)
           (A000010_naive_par_aux n 6 8)
        )
        (+ (A000010_naive_par_aux n 5 8)
           (A000010_naive_par_aux n 7 8)
        )
     )
  )
)


;; Actually, I think we already have it: (Although, again, with current
;; Aug 2011 implementation, this leaves behind a trail of unnecessary
;; wires, to be optimized off by the third-party Verilog-compiler).

(define-wirm (A000010_naive_par31 n) (A000010_naive_par_aux_i_times n 31))

(define-wirm (A000010_naive_par_aux_i_times n i)
   (if (zero? i)
       0
       (+ (A000010_naive_par_aux n i 31)
          (A000010_naive_par_aux_i_times n (- i 1))
       )
   )
)

;; For testing, we use gcd_once_unrolled instead of ordinary gcd:
(define (A000010_naive_par_aux n start_offset increment)
  (let loop ((prev_i (+ start_offset 1)) (i (+ start_offset 1)) (s (zeqw n)))
       (cond ((logor (> i n) (< i prev_i))
                s ;; Return the result if i has grown over n (or wrapped over)
             )
             (else
               (let ((a (gcd_once_unrolled n i)))
                 (loop i
                       (+ i increment)
                       (+ s (zxt (bitand (bit a 0) (rednor (drop a 1)))))
                 )
               )
             )
       )
  )
)

