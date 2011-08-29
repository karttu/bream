;;
(-MODULE-INFO-BEGIN-)
;;
;; intfuns-untested.brm.scm -- A collection of integer functions in Scheme,
;; which are yet to be "breamified" and tested.
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
;; Note: After founding that Bream compiles a function correctly, and
;; that it works OK in real FPGA, one should move its definition then
;; either to
;;   intbasic.brm.scm
;; if it is often needed function implemented reasonably, or, to
;;   intfuns-nonpractical.brm.scm
;; if it is never meant for serious use, but is otherwise useful for testing.
;;
(-MODULE-INFO-END-)
;;
;; NOTE:
 ;; (We probably need also a separate module for "reasonably implemented,
;;  but very seldomly needed functions".)
;;
;;
;; CHANGES
;;
;;
;; Edited    Aug 28 2011 by karttu.
;;   Separated this module from intbasic.brm.scm
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define (abs n) (if (< n 0) (- n) n)) ;; XXX - Needs signed integers!


(define (gcd_once_unrolled a b)
   (let-unrolled 1 loop ((a a) (b b))
         (cond ((zero? b) a)
               ((>= a b) (loop b (- a b)))
               (else (loop (- b a) a))
         )
   )
)


(define (gcd_r a b) (if (zero? b) a (gcd_r b (remainder a b))))

(define (gcd_v3 a b)
   (cond ((zero? b) a)
         ((>= a b) (gcd_v3 b (- a b)))
         (else (gcd_v3 b (- b a))) ;; Not the most optimal!
   )
)

(define (gcd_v4 a b)
   (cond ((zero? b) a)
         (else (gcd_v4 b (abs (- a b))))
   )
)

(define-wirm (lcm a b) (*c a (quotient b (gcd a b))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                               ;;
;; Most of the following stuff is for the testing suite:         ;;
;;                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lcm_v2 a b) (quotient (* a b) (gcd_r a b)))

(define (lcm_v3 a b) (* a (quotient b (FASTEST (gcd a b) (gcd_r a b)))))


(define (gcd-v1-count a b)
  (let loop ((a a) (b b) (c 0))
     (cond ((zero? b) c)
           (else (loop b (remainder a b) (1+ c)))
     )
  )
)

;; Neither of (gcd-v1-count (A025581 n) (A002262 n))
;; nor its transpose (gcd-v1-count (A002262 n) (A025581 n))
;; seems to be present in the old OEIS base.

;; Neither of (gcd-v2-count (A025581 n) (A002262 n))
;; nor its transpose (gcd-v2-count (A002262 n) (A025581 n))
;; seems to be present in old OEIS. (Check also +1 versions.)

(define (gcd-v2-count a b)
  (let loop ((a a) (b b) (c 0))
     (cond ((zero? b) c)
           ((>= a b) (loop b (- a b) (1+ c)))
           (else (loop (- b a) a (1+ c)))
     )
  )
)


;; (gcd-v1-count 65534 2) --> 1
;; (gcd-v1-count 2 65534) --> 2
;; (gcd-v1-count (A000045 24) (A000045 23)) --> 22 ;; Times division algorithm!
;; (gcd-v1-count (A000045 23) (A000045 24)) --> 23

;; (gcd-v1-count 2 254) --> 2
;; (gcd-v1-count 254 2) -->  1

;; (gcd-v3-count 2 254) --> 190
;; (gcd-v3-count 254 2) --> 190

;; (gcd-v3-count 1 255) --> 382
;; (gcd-v3-count 255 1) --> 382

;; (gcd-v1-count 233 144) --> 11 ;; Times division algorithm!
;; (gcd-v1-count 144 233) --> 12

;; (gcd-v2-count 233 144) --> 12
;; (gcd-v2-count 144 233) --> 12

;; (gcd-v3-count 233 144) --> 12
;; (gcd-v3-count 144 233) --> 23

;; (gcd-v2-count (A000045 24) (A000045 23)) --> 23
;; (gcd-v2-count (A000045 23) (A000045 24)) -->  23

;; (gcd-v2-count 65534 2) --> 32767
;; (gcd-v2-count 2 65534) --> 32767

;; (gcd-v3-count 65534 2) --> 49150
;; (gcd-v3-count (A000045 24) (A000045 23)) --> 23
;; (gcd-v3-count (A000045 23) (A000045 24)) --> 45


;; Neither of (gcd-v3-count (A025581 n) (A002262 n))
;; nor its transpose (gcd-v3-count (A002262 n) (A025581 n))
;; seems to be present in OEIS:
(define (gcd-v3-count a b)
  (let loop ((a a) (b b) (c 0))
     (cond ((zero? b) c)
           ((>= a b) (loop b (- a b) (1+ c)))
           (else (loop b (- b a) (1+ c)))
     )
  )
)


;; For testing:
;; (map (lambda (n) (gcd (quotient n 256) (remainder n 256))) (iota0 65535))
;; (map (lambda (n) (lcm (quotient n 256) (remainder n 256))) (iota0 65535))


(define (A005408 n) (+ (* 2 n) 1)) ;; (* 2 n) should be converted to (<< n 1)



;; How to do a staged, compile-time expanded version which directly sums
;; all the bits of n?

(define (A000120 n) ;; Number of 1-bits in unsigned integer n.
  (let loop ((n n) (i 0))
     (if (zero? n) i (loop (>> n 1) (+ i (bitand n 1))))
  )
)


;; How to do staged, combinational versions of these?
;; Is it possible with that kind of wrapped addition?
;; Should be, in principle. If we add (16/2) = 8
;; max 2 bits (max 3) values, we get 8*3 = 24, which fits
;; into 5 bits. This in turn can be divided to 3 2-bit substrings,
;; and 3*3 = 9, two 2-bit substrings.

(define (A010872 n) ;; n mod 3
  (let loop ((n n) (s 0))
     (if (zero? n) s
         (loop (>> n 2) (add_mod3 s (bitand n 3)))
     )
  )
)


;; XXX - Doesn't compile as such, but recursive macro version would be nice!
;; We need mod3-adder, with max. 2-bit value in range 0-2.
;; (sixteen possible input-combinations {0..3} x {0..3} --> {0..2})

;; Similarly mod5-adder, with max. 3-bit value in range 0-4.
;; (64 possible input-combinations {0..7} x {0..7} --> {0..4})

;; Similarly mod7-adder, with max. 3-bit value in range 0-6.
;; (64 possible input-combinations {0..7} x {0..7} --> {0..6})


(define (add-two-bit-pairs a b)
  (+ ;; i.e. bitor
     (bitxor (bit a 0) (bit b 0))
     (<< (bitxor (bitxor (bit a 1) (bit b 1)) (bitand (bit a 0) (bit b 0))) 1)
     (<< (bitxor (bitand (bit a 1) (bit b 1))
                 (bitand (bitxor (bit a 1) (bit b 1))
                         (bitand (bit a 0) (bit b 0))
                 )
         )
         2
     )
  )
)

;; Modify the one above to create a mod3-adder:
;; (Or use a Karnaugh-map? Two four-input functions, for bit_0 and for bit_1
;; of the reduced sum. Hmm, not really reducing at all, as all 1's are
;; in both cases in different rows and different columns (except two)).

(define (add-two-bit-pairs-mod3 a b)
  (+ ;; i.e. bitor
    (bitand (bit a 0)
            (bitor (bitand (bitnot (bit a 1)) ;; Either 1.
                           (bitand (bitnot (bit b 0))
                                   (bitnot (bit b 1))
                           )
                   )
;; or 7 or 13.
                   (bitand (bit b 0) (bitxor (bit a 1) (bit b 1)))
            )
    )
;; Or 4 or 10:
    (bitand (bitnot (bit a 0))
            (bitand (bitxnor (bit a 1) (bit b 1))
                    (bitxor (bit b 0) (bit b 1))
            )
    )

    (<<
        (+ 
           (bitand (bit b 1)
                   (bitor (bitand (bitnot (bit b 0)) ;; Either 8.
                                  (bitand (bitnot (bit a 0))
                                          (bitnot (bit a 1))
                                  )
                          )
;; or 11 or 14.
                          (bitand (bit a 1) (bitxor (bit a 0) (bit b 0)))
                   )
           )
;; or 2 or 5:
           (bitand (bitnot (bit b 1))
                   (bitand (bitxnor (bit a 0) (bit b 0))
                           (bitxor (bit b 0) (bit a 1))
                   )
           )
        )
        1
     )
  )
)

;; Then we indeed have:
;;
;; (map (lambda (p) (apply add-two-bit-pairs-mod3 p))
;;     '((0 0) (0 1) (0 2) (0 3)
;;       (1 0) (1 1) (1 2) (1 3)
;;       (2 0) (2 1) (2 2) (2 3)
;;       (3 0) (3 1) (3 2) (3 3)
;;      )
;; )
;; --> (0 1 2 0 1 2 0 1 2 0 1 2 0 1 2 0)

;; Now, if one compiles the above Bream function
;; to Verilog function (i.e. really a Verilog compile-time macro)
;; is it any faster, than definition

(define (add_mod3 a b)
      (if (> (+ a b) 5) (- (+ a b) 6)
          (if (> (+ a b) 2) (- (+ a b) 3) (+ a b))
      )
)

;; which should create a Verilog function like this:

;; function [1:0] add_mod3;
;;   input a;
;;   input b;
;;   add_mod3 = (((a+b)>5) ? (a+b)-6 : (((a+b)>2) ? (a+b)-3 : (a+b)));
;; endfunction;

;; Or does the Verilog compiler know how to optimize it all the same?

(define (A010872v2 n)
 (cond ((< n 3) n)
       ((= 3 n) 0)
       (else
          (let loop ((n n) (s 0))
            (if (zero? n)
                (A010872v2 s) ;; Whee, wee recursion.
                (loop (>> n 2)  (+ s (bitand n 3))) ;; Sum in two bit groups.
            )
          )
       )
 )
)


;; The staging form should expand to something like this, with 16 bit args:
;; which is combinational function, after all.
(define (A010872v3 n) ;; n mod 3
   (add_mod3 (bits n 15 14)
       (add_mod3 (bits n 13 12)
           (add_mod3 (bits n 11 10)
               (add_mod3 (bits n 9 8)
                   (add_mod3 (bits n 7 6)
                       (add_mod3 (bits n 5 4)
                           (add_mod3 (bits n 3 2) (bits n 1 0))
                       )
                   )
               )
           )
       )
   )
)

;; Might be more balanced nesting, still seven additions needed:
;; How to create with a recursive macro?
(define (A010872v4 n) ;; n mod 3
   (add_mod3
       (add_mod3 (add_mod3 (bits n 15 14) (bits n 13 12))
                 (add_mod3 (bits n 11 10) (bits n 9 8))
       )
       (add_mod3 (add_mod3 (bits n 7 6) (bits n 5 4))
                 (add_mod3 (bits n 3 2) (bits n 1 0))
       )
   )
)



;; How to make this kind of macro work, and with what kind of syntax?
(define (A010872macro n)
   (if (<= (WIDTH n) 2)
       (add_mod3 n 0)
       (add_mod3
         (A010872macro (bits n (-1+ (WIDTH n)) (>> (WIDTH n) 1)))
         (A010872macro (bits n (-1+ (>> (WIDTH n) 1)) 0))
       )
   )
)

;; (A010872macro n), where (WIDTH n)=16, expands at the first step to:
;;    (add_mod3 (A010872macro (bits n 15 8))
;;              (A010872macro (bits n 7 0))
;;    )



(define (A079978 n) (rednor (A010872 n))) ;; a(n)=1 if n=3k, a(n)=0 otherwise.

;; For testing with the divisibility with 3, we can also use:
;;    (OR-F (AND (= 0 (redxor n)) (not-in-Ajoku_uus n)) ...)
;; if the membership in Ajoku_uus:
;;   5,10,17,20,23,29,34,40,43,46,53,58,65,68,71,77,
;; could be quickly decided.

;; These both, mod19a and mod19b work at least up to n=65536.
;; We could (most probably) use (zero? i) as the ending condition, if
;; i is set to large enough value in beginning:
(define (mod19a n)
     (let loop ((s n) (i 5))
           (cond ((zero? (>> s 5)) (if (>= s 19) (- s 19) s))
                 (else (loop (+ (bitand s 31) ;; i.e. (bits s 4 0)
                                (* 13 (>> s 5)) ;; i.e. (* 13 (drop s 5))
                             )
                             (- i 1)
                       )
                 )
          )
     )
)


(define (mod19b n)
     (let loop ((s n) (i 5))
           (cond ((zero? (>> s 6))
                     (cond ((>= s 57) (- s 57)) ;; This "after-treatment"
                           ((>= s 38) (- s 38)) ;; should take five 6-bit LUTs
                           ((>= s 19) (- s 19)) ;; in total. (at most)
                           (else s)
                     )
                 )
                 (else (loop (+ (bitand s 63) ;; i.e. (bits s 5 0)
                                (- (<< (>> s 6) 3) ;; i.e. (* 8 (drop s 6))
                                   (>> s 6) ;; - (drop s 6) = 7*(drop s 6)
                                )
                             )
                             (- i 1)
                       )
                 )
          )
     )
)


;; (define A001969 (MATCHING-POS 0 0 (lambda (i) (= 0 (redxor i))))) ;; Evil nums

;; Check:

;; (define A039004 (MATCHING-POS 0 0 (lambda (i) (and (zero? (modulo i 3)) (= 0 (redxor i))))))

;; %S A039004 0,3,6,9,12,15,18,24,27,30,33,36,39,45,48,51,54,57,60,63,66,72,
;; %N A039004 Numbers n such that representation in base 4 has same number of 1's and 2's.

;; Evil numbers not divisible by 3:
;; (define Ajoku_uus (MATCHING-POS 0 0 (lambda (i) (and (not (zero? (modulo i 3))) (= 0 (redxor i))))))


;; (define A036556 (MATCHING-POS 1 1 (lambda (i) (= 1 (redxor (A008585 i))))))

;; %S A036556 7,14,23,27,28,29,31,39,46,54,56,57,58,62,71,78,87,91,92,
;;
;; %N A036556 Integers which when multiplied by 3 have an odd number of 1's
;; in their binary expansion (cf. A000069).
;; Intersection of A000069 and A008585 (multiples of 3), divided by 3.



(define (A010874 n) ;; n mod 5.
  (let loop ((n n) (p 0) (s 0))
     (cond ((zero? n) s)
           ((even? p)
              (loop (>> n 2) (bitnot p)
                    (let ((next (+ s (bitand n 3))))
                       (if (>= next 5) (- next 5) next)
                    )
              )
           )
           (else ;; (odd? p)
              (loop (>> n 2) (bitnot p)
                    (let ((next (- s (bitand n 3))))
                       (if (< next 0) (+ next 5) next)
                    )
              )
           )
     )
  )
)


(define (A010876 n) ;; n mod 7
  (let loop ((n n) (s 0))
     (if (zero? n) s
         (loop (>> n 3)
               (let ((next (+ s (bitand n 7))))
                  (if (>= next 7) (- next 7) next)
               )
         )
     )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (A000045 n) ;; Tail-recursive fibonacci, quite dumb.
  (let loop ((n n) (z1 0) (z2 1))
      (if (zero? n) z1 (loop (-1+ n) z2 (+ z1 z2)))
  )
)




(define (cut-factorial i j) ;; Compute the product i*(i+1)*(i+2)*...*j, i<j
  (let loop ((i i) (p i))
     (cond ((== i j) p)
           ((> i j) 1) ;; Is this needed?
           (else (loop (1+ i) (* (1+ i) p)))
     )
  )
)


;; (binomial r n) stands for the binomial {r choose n}
;; Probably not the most optimal way to compute this:
(define (binomial r n) (quotient (cut-factorial (1+ n) r) (A000142 (- r n))))

(define (A000079 n) (<< 1 n)) ;; Does this produce synthesizable Verilog?

(define (A000079v2 n)
   (let loop ((i n) (s 1))
        (if (zero? i) s (loop (-1+ i) (+ s s)))
   )
)

(define (A000079v3 n) ;; Does this synthesize differently than one above?
   (let loop ((i n) (s 1))
        (if (zero? i) s (loop (-1+ i) (<< s 1)))
   )
)

(define (A000079v4 n)
   (let loop ((i n) (s 0))
        (cond ((zero? i) (1+ s))
              (else (loop (-1+ i) (+ s (binomial n i))))
        )
   )
)


;; (A000108 11) = 58786, (A000108 12) = 208012
(define (A000108 n) (quotient (binomial (<< n 1) n) (1+ n)))

(define (A009766tr n m) ;; a(n,m) = C(n+m,n)*(n-m+1)/(n+1), n >= m >= 0.
  (quotient (* (1+ (- n m)) (binomial (+ n m) n)) (1+ n))
)

;; Cannot be optimized this way:
;; (define (A009766tr2 n m) ;; a(n,m) = C(n+m,n)*(n-m+1)/(n+1), n >= m >= 0.
;;  (* (1+ (- n m)) (quotient (binomial (+ n m) n) (1+ n)))
;; )


;; Optimize, just one loop call with ternary ifs, or something:
(define (CatalanUnrank size rank)
  (let loop ((a 0)
             (m (-1+ size))        ;; The row on A009766
             (y size)              ;; The position on row m of A009766
             (rank rank)
             (c (A009766tr (-1+ size) size))
            )
      (if (negative? m) a
          (if (>= rank c)
              (loop (1+ (<< a 1))   ;; Up the mountain high
                    m
                    (-1+ y)
                    (- rank c)
                    (A009766tr m (-1+ y))
              )
              (loop (<< a 1)        ;; Down to the valley low
                    (-1+ m)
                    y
                    rank
                    (A009766tr (-1+ m) y)
              )
          )
      )
  )
)

;; Note that (A000142 9) = 362880 and (binwidth 362880) = 19
(define (A000142 n) ;; Tail-recursive factorial:
  (let loop ((n n) (z 1))
      (if (zero? n) z (loop (-1+ n) (* n z)))
  )
)

;; Only up to n=12 ? (A003418 12) = 27720 (binwidth (A003418 12)) = 15
;; (A003418 13) = (A003418 14) = (A003418 15) =  360360, (binwidth 360360) = 19

(define (A003418 n)
  (let loop ((n n) (z 1))
      (if (zero? n) z (loop (-1+ n) (lcm n z)))
  )
)

;; This one just for testing speculative evaluation.
;; Both A010051 and lcm are started simultaneously.
;; However, if n is indeed prime,
;; then we can use an ordinary multiplication (*, combinational function)
;; instead of more heavy lcm:

;; Use a fast variant of A010051, so that it takes at least one cycle
;; but is ready a long before lcm.
;; e.g. A010051v2 which uses A000010, and thus gcd instead of remainder.

(define (A003418v2 n)
  (let loop ((i 1) (z 1))
       (cond ((> i n) z)
             (else (loop (1+ i) (if (zero? (A010051v2 i)) (lcm i z) (* i z))))
       )
  )
)


;; The evaluation of A000142 and A003418 starts simultaneously:
(define (A025527 n) (quotient (A000142 n) (A003418 n)))


;; %N A000254 Stirling numbers of first kind s(n,2): a(n+1)=(n+1)*a(n)+n!.
;; %S A000254 0,1,3,11,50,274,1764,13068,109584,1026576,10628640,120543840,
;; %O A000254 0,3
;; (MuPAD) A000254 := proc(n) begin n*A000254(n-1)+fact(n-1) end_proc: A000254(1) := 1:

(define (A000254 n)
   (let loop ((i 0) (z 0))
        (cond ((= i n) z)
              (else (loop (1+ i) (+ z (* i z) (A000142 i))))
        )
   )
)


(define (A000254v2 n) ;; Compute the factorial in the same loop, much faster!
   (let loop ((i 0) (z 0) (f 1))
        (cond ((= i n) z)
              (else (loop (1+ i) (+ z (* i z) f) (* f (1+ i))))
        )
   )
)

;; Some basic number theoretic functions:
;; phi (A000010), tau (A000005) and sigma (A000203) follow:


(define (A000010v2 n)
  (let loop ((i n) (s (zeqw n)))
       (cond ((zero? i) s)
             (else (loop (-1+ i) (+ s (if (= 1 (gcd n i)) 1 0))))
       )
  )
)


(define (A051953 n) ;; Cototient(n) := n - phi(n).
  (let loop ((i n) (s 0))
       (cond ((zero? i) (- n s))
             ((= 1 (gcd n i)) (loop (-1+ i) (1+ s)))
             (else (loop (-1+ i) s))
       )
  )
)


(define (A023896 n)
  (let loop ((i n) (s 0))
       (cond ((zero? i) s)
             ((= 1 (gcd n i)) (loop (-1+ i) (+ s i)))
             (else (loop (-1+ i) s))
       )
  )
)


(define (A000005 n) ;; d(n) (also called tau(n) or sigma_0(n)),
  (let loop ((i n) (s 0)) ;;  the number of divisors of n.
       (cond ((zero? i) s)
             (else (loop (-1+ i) (+ s (rednor (remainder n i)))))
       )
  )
)

;; remainder in the test-expr of the second branch:
(define (A000005v2 n) ;; d(n) (also called tau(n) or sigma_0(n)),
  (let loop ((i n) (s 0)) ;;  the number of divisors of n.
       (cond ((zero? i) s)
             ((zero? (remainder n i)) ;; i divides n?
                   (loop (-1+ i) (+ s 1))
             )
             (else (loop (-1+ i) s))
       )
  )
)


(define (A000203 n) ;; sigma(n) = sum of divisors of n. Also called sigma_1(n)
  (let loop ((i n) (s 0))
       (cond ((zero? i) s)
             (else (loop (-1+ i) (+ s (if (zero? (remainder n i)) i 0))))
       )
  )
)

;; Use multiply:
(define (A000203v2 n)
  (let loop ((i n) (s 0))
       (cond ((zero? i) s)
             (else (loop (-1+ i) (+ s (* i (rednor (remainder n i))))))
       )
  )
)

;; remainder in the test:
(define (A000203v3 n)
  (let loop ((i n) (s 0))
       (cond ((zero? i) s)
             ((zero? (remainder n i)) ;; i divides n?
                   (loop (-1+ i) (+ s i))
             )
             (else (loop (-1+ i) s))
       )
  )
)


;; Various versions of A010051:
;; (Characteristic function of primes: 1 if n is prime else 0)
(define (A010051 n) (if (= 2 (A000005 n)) 1 0)) ;, Exactly 2 factors:

(define (A010051v2 n)
   (if (= (-1+ n) (A000010 n)) 1 0)
)

(define (A010051v2 n)
   (if (= (1+ n) (A000203 n)) 1 0)
)


(define (A005171 n) (bitnot (A010051 n))) ;; 0 if n is prime else 1.


;; An example how to use OR-F (or-fastest), to test whether n is composite:

(define (A005171-F n)
  (OR-F (= n 2) (even? n)
        (= n 3) (nonzero? (A079978 n)) ;; Preferably the staged version.
        (= n 5) (zero? (A010874 n)) ;; Preferably the staged version.
        (= n 7) (zero? (A010876 n)) ;; Preferably the staged version.
        (not (A010051 n))
  )
)


;; Characteristic function of squares: 1 if n is a square else 0.
(define (A010052 n) (bitand (A000005 n) 1)) ;; (gives wrong result for zero.)
;; Or (bit (A000005 n) 0), just take the least significant.

;; %S A007978 2,3,2,3,2,4,2,3,2,3,2,5,2,3,2,3,2,4,2,3,2,3,2,5,2,3,2,3,2,4,2,3,

(define (A007978 n) ;; Least non-divisor of n.
  (let loop ((i 1))
       (cond ((= i (1+ n)) i)
             ((not (zero? (remainder n i))) i)
             (else (loop (1+ i)))
       )
  )
)


(define (A020639 n) ;; Lpf(n): least prime dividing n (a(1)=1).
  (let loop ((i 1))
       (cond ((= i n) i)
             ((not (= 1 (gcd n i))) i)
             (else (loop (1+ i)))
       )
  )
)


;; Good for testing suite:

(define (A023022 n) (if (<= n 2) 1 (>> (A000010 n) 1)))

(define (A023022v2 n) (if (<= n 2) 1 (quotient (A023896 n) n)))

;; "Almost constant test" in loop cond. Non-comb function on the final branch:
(define (A023022v3 n)
  (let loop ((i n) (s 0))
       (cond ((<= n 2) 1) ;; The test result doesn't change in the loop.
             ((zero? i) (quotient s n))
             ((= 1 (gcd n i)) (loop (-1+ i) (+ s i)))
             (else (loop (-1+ i) s))
       )
  )
)

;; (let loop) inside an if. Non-comb function on the final branch:
(define (A023022v4 n)
  (if (<= n 2) 1
      (let loop ((i n) (s 0))
          (cond ((zero? i) (quotient s n))
                ((= 1 (gcd n i)) (loop (-1+ i) (+ s i)))
                (else (loop (-1+ i) s))
          )
      )
  )
)


; %S A018804 1,3,5,8,9,15,13,20,21,27,21,40,25,39,45,48,33,63,37,72,65,63,45,
; %N A018804 Sum of gcd(k,n) for 1 <= k <= n.
; %F A018804 a(n)=Sum_{d|n} d*phi(n/d), where phi(n) is Euler totient function.

(define (A018804 n)
  (let loop ((i n) (s 0))
       (cond ((zero? i) s)
             (else (loop (-1+ i) (+ s (gcd n i))))
       )
  )
)

;; Hard way, using essentially the latter, Jovovic's formula:
;; noncomb function in the test as well as the new assignments of the
;; second branch:
(define (A018804v2 n)
  (let loop ((i n) (s 0))
       (cond ((zero? i) s)
             ((zero? (remainder n i)) ;; i divides n?
                   (loop (-1+ i) (+ s (* i (A000010 (quotient n i)))))
             )
             (else (loop (-1+ i) s))
       )
  )
)

;; XXX -- How to compute A072649 ?

;; And then this:
;;
;; (define (A003714 n)
;;    (let loop ((n n) (s 0))
;;      (cond ((< n 3) (+ s n))
;;            (else (loop (- n (A000045 (1+ (A072649 n))))
;;                        (+ s (<< 1 (-1+ (A072649 n))))
;;                  )
;;            )
;;      )
;;   )
;; )
;;


;; Quotient and remainder should be really computed with the same
;; routine, quotient-remainder (that is, integer-divide) !

(define (A034968 n)
   (let loop ((n n) (i 2) (s 0))
      (cond ((zero? n) s)
            (else (loop (quotient n i)
                        (1+ i)
                        (+ s (remainder n i))
                  )
            )
      )
   )
)

;; The function divide should propagate (* 2 (WIDTH its-first-argument))
;; to its caller: (Try to keep Scheme evaluatable as well!)
;; (define (divide x y) (:WITH-RESULT-WIDTH (* 2 (WIDTH x))) ???
;;   (let ((result (ZEROS (* 2 (WIDTH x))))) ???
;;   )
;; )

;; Because (WIDTH n) can be resolved at the compile time,
;; so should all the surrounding expressions as well, as long
;; as they involve only combinational functions like * 2 or -1+.
;; However, bits itself shouldn't be "resolved" at compile time:

(define (A034968-v2 n)
   (let loop ((n n) (i 2) (s 0))
     (let* ((quot-et-rem (divide n i))
            (quotient  (bits quot-et-rem (-1+ (WIDTH n)) 0))
            (remainder (bits quot-et-rem (-1+ (* 2 (WIDTH n))) (WIDTH n)))
           )
       (cond ((zero? n) s)
             (else (loop quotient (1+ i) (+ s remainder)))
       )
     )
   )
)


(define (A060130 n)
   (let loop ((n n) (i 2) (s 0))
      (cond ((zero? n) s)
            (else (loop (quotient n i)
                        (1+ i)
                        (+ s (if (zero? (remainder n i)) 0 1))
                  )
            )
      )
   )
)


(define (A060130-v2 n)
   (let loop ((n n) (i 2) (s 0))
      (cond ((zero? n) s)
            (else (loop (quotient n i)
                        (1+ i)
                        (+ s (redor (remainder n i)))
                  )
            )
      )
   )
)

(define (A099563 n)
   (let loop ((n n) (i 2) (r 0))
      (cond ((zero? n) r) ;; Give the last remainder.
            (else (loop (quotient n i)
                        (1+ i)
                        (remainder n i)
                  )
            )
      )
   )
)



(define (A153880 n)
   (let loop ((n n) (z 0) (i 2) (f 2))
      (cond ((zero? n) z)
            (else (loop (quotient n i)
                        (+ (* f (remainder n i)) z)
                        (1+ i)
                        (* f (+ i 1))
                  )
            )
      )
   )
)

;; And the same halved:
;; (define (A153883 n) (>> (A153880 n) 1))



(define (A117966 n) ;; 0,1,-1,3,4,2,-3,-2,-4,9,10,8,12,... XXX - Negative nums!
  (let loop ((z 0) (p3 1) (n n))
    (if (zero? n)
        z
        (loop (+ z
                 (* p3 (if (= 2 (remainder n 3)) -1 (remainder n 3)))
              )
              (* 3 p3)
              (quotient n 3)
        )
    )
  )
)


;; When applied to natural numbers >= 1, gives:
;; i.e. a bisection of the next sequence.

;; Inverse of A117966:
;; (map A117967 (map A117966 (iota0 729))) = (iota0 729)

;; Gives a non-negative integer, which when converted to ternary
;; and interpreted as _balanced ternary_, is equal to argument z.
;; When restricted to non-negative integers, gives A117967.
;; (XXX - TODO: CONVERT TO TAIL-RECURSIVE FORM:)
(define (A117967 z)
   (cond ((zero? z) 0)
         ((negative? z) (A004488 (A117967 (- z))))
         (else ;; z is positive.
            (let* ((lp3 (expt 3 (A062153 z))) ;; largest power of three of z.
                   (np3 (* 3 lp3)) ;; Next Power of Three.
                  )
                (if (< (* 2 z) np3) ;; z less than np3/2
                    (+ lp3 (A117967 (- z lp3)))
                    (+ np3 (A117967 (- z np3)))
                )
            )
         )
   )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (jacobi-symbol p q) ;; XXX - TODO, THE SIGNED INTEGERS!
  (let loop ((p p)
             (q q)
             (s 0))  ;; 0 in bit-1 stands for +1, 1 in bit-1 for -1.
       (cond ((zero? p) 0)
             ((= 1 p) (- 1 (bitand s 2)))
             ((= 1 (bitand p 1)) ;; Odd p ?
                (loop (remainder q p) p (bitxor s (bitand p q)))
             )
             (else ;; It's even, get rid of one 2:
                (loop (>> p 1) q (bitxor s (bitxor q (>> q 1))))
             )
       )
  )
)


;; "Number of times Sum_{i=1..u} J(i,2n+1) obtains value zero when u ranges from 1 to (2n+1). Here J(i,k) is the Jacobi symbol."
(define (A166040 n)
    (let ((w (A005408 n)))
;; s = sum of the first i Jacobi-symbols in [1,i]
      (let loop ((i 1) (s 1) (zv 0)) ;; zv = # of sea level visits.
            (cond ((= i w) zv)
                  ((zero? s)
                       (loop (1+ i) (+ s (jacobi-symbol (1+ i) w)) (1+ zv))
                  )
                  (else (loop (1+ i) (+ s (jacobi-symbol (1+ i) w)) zv))
            )
      )
    )
)

;; "Sum of the quadratic residues of 2n+1 in range [1,2n+1]." How large sum?
(define (A166100 n)
    (let ((w (A005408 n)))  ;; w = 2n+1
      (let loop ((i 1) (s 1))
            (cond ((= i w) s)
               (else (loop (1+ i)
                           (+ s (if (= +1 (jacobi-symbol (1+ i) w)) (1+ i) 0))
                     )
               )
            )
      )
    )
)

;; Two bisections of A166040:
;; (define (A166085 n) (A166040 (A005843 n))) ;; For searching among 4n+1
(define (A166086 n) (A166040 (A005408 n))) ;; and for amongst 4n+3


(define (A166091bi n k)
  (let ((m (A005408 k)))
   (let loop ((i 0) (n n))
        (cond ((= m (A166086 i))
                  (if (zero? n)
                      i
                      (loop (1+ i) (-1+ n))
                  )
              )
              (else (loop (1+ i) n))
        )
   )
  )
)

(define (A166091 n) (A166091bi (A025581 n) (A002262 n)))


;; (define legendre-symbol jacobi-symbol)
;; (define (A165471 n) (legendre-symbol n 65537)) ;; A019434(4)=65537.
;; (define A165472 (PARTIALSUMS 0 0 A165471))

