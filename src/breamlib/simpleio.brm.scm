
;;
(-MODULE-INFO-BEGIN-)
;;
;; simpleio.brm.scm - Very simple I/O routines for the initial version
;;                    of Bream. Currently just a couple of output routines
;;                    that use a separate outbyte115200_1_1_8.v Verilog-module
;;                    which sends a byte to UART with the speed 115200 bps.
;;
;; Copyright (C) 2010-2011 Antti Karttunen, and subject to LGPL v2
;;
(-MODULE-INFO-END-)
;;
;;
;;
;; CHANGES
;;
;; Edited    Aug 29 2011 by karttu.
;;   Removed all functions and macros that were intended solely for
;;   the MIT/GNU Scheme-interpreter at the initial testing phase.
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Output n 8-bit bytes given in c0 -- c(n-1)
;, to outchan, with Verilog-routine V_outchar.
;; Return 1 as a result when finally ready.

;; With eleven char-arguments (from c0 to c10) we can output
;; all 32-bit integers plus one delimiter character.
;; (-1+ (expt 2 32)) = 4294967295 is 10 digits.

;; If the n is here larger than 11, the first 11 characters are
;; followed by n-11 tildes (~, character code 126), which works
;; as a kind of an error indicator.

(define (outchars11 outchan n c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10)
   (let loop ((n n) (c0 c0) (c1 c1) (c2 c2) (c3 c3) (c4 c4)
              (c5 c5) (c6 c6) (c7 c7) (c8 c8) (c9 c9) (c10 c10)
             )
     (seq-if (zero? n)
             1 ;; Just return something.
             (seq (outbyte115200 outchan c0)
                  (loop (- n 1) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 126)
             )
     )
   )
)


(define (outchars12 outchan 4'n c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11)
   (let loop ((n n) (c0 c0) (c1 c1) (c2 c2) (c3 c3) (c4 c4)
              (c5 c5) (c6 c6) (c7 c7) (c8 c8) (c9 c9) (c10 c10) (c11 c11)
             )
     (seq-if (zero? n)
             1 ;; Just return something.
             (seq (outbyte115200 outchan c0)
                  (loop (- n 1) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 126)
             )
     )
   )
)


;; Output an unsigned integer n in decimal notation to
;; output-channel outchan, followed by a single delim-byte
;; byte.

;; This is the first rudimentary version, which uses explicit
;; loop registers c0 - c10, as so far we haven't yet implemented
;; stacks (in any case, it would be wasteful with BRAMs).
;;
;; Also, outbyte115200 can be invoked from one location only,
;; thus we must output the delimiter byte at the same call.
;;
;; Later outchan would automatically synthesize to a some kind of
;; PISO, with different calls of outbyte115200 activating its
;; different "input_signal_ports", and PISO's internal FSM
;; would then queue the bytes to be output in an appropriate manner.

;; Note that we first collect the digits in loop registers
;; c0 - c10, in such a way that the most significant digit
;; comes to c0, and the least significant digit is
;; located somewhere at c_k, followed by delim-byte
;; given as an argument.
;; When n, after being repeatedly divided by 10, is finally zero,
;; the collected digits are output, from left to right,
;; and followed by the delim-byte.



(define (outdec_with_postdelim outchan n delim-byte)
   (let loop ((n n)
              (i 1)
              (c0 delim-byte) (c1 0) (c2 0) (c3 0) (c4 0)
              (c5 0) (c6 0) (c7 0) (c8 0) (c9 0) (c10 0)
             )
      (seq-if (and (zero? n) (> i 1)) ;; Make sure 0 is output as '0'
              (outchars11 outchan i c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10)
              (loop (quotient n 10)
                    (1+ i)
                    (+ 48 (remainder n 10)) ;; '0' is 48 in ascii.
                    c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 ;; Shift one right...
              )
      )
   )
)


(define-wirm (print_dec_with_crlf outchan n)
   (outdec_with_2_postdelims outchan n 8'13 8'10)
)


(define (outdec_with_2_postdelims outchan n 8'delim1byte 8'delim2byte)
 (let loop ((n n)
            (i 4'0)
            (c0 delim1byte) (c1 delim2byte) (c2 0) (c3 0) (c4 0)
            (c5 0) (c6 0) (c7 0) (c8 0) (c9 0) (c10 0) (c11 0)
           )
    (seq-if (logand (zero? n) (nonzero? i)) ;; Make sure 0 is output as '0'
            (outchars12 outchan (+ i 2) c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11)
            (loop (quotient_by_msb1_divisor n 4'10)
                  (+ i 1)
                  (+ 8'48 (zxt (remainder_by_msb1_divisor n 4'10)))
                  c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 ;; Shift one right...
            )
    )
 )
)


(define-wirm (print_fac_with_crlf outchan n)
   (outfac_with_2_postdelims outchan n 8'13 8'10)
)

;; Similar function for factorial number system:
;; n should be in range 0..36287999 (i.e. the upper lim=(10*10!)-1)
;; which fits into 26 bits.
(define (outfac_with_2_postdelims outchan n 8'delim1byte 8'delim2byte)
 (let loop ((n n)
            (i 4'2) ;; Start dividing with 2, then 3, 4, etc.
            (c0 delim1byte) (c1 delim2byte) (c2 0) (c3 0) (c4 0)
            (c5 0) (c6 0) (c7 0) (c8 0) (c9 0) (c10 0) (c11 0)
           )
    (seq-if (logand (zero? n) (> i 2)) ;; Make sure 0 is output as '0'
            (outchars12 outchan i c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11)
            (loop (quotient n i)
                  (+ i 1)
                  (+ 8'48 (zxt (remainder n i))) ;; 48 = '0'.
                  c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 ;; Shift one right...
            )
    )
 )
)



(define (output_n_and_gcd_hi_lo_n_upto_uplimit outchan uplimit)
  (let loop ((i (zeqw uplimit)) (p 1'0))
          (seq (outdec_with_2_postdelims
                   outchan
                    (if (zero? p) i (gcd_hi_lo i))
                    (if (zero? p) 58 13)
                    (if (zero? p) 32 10)
               )
               (cond ((zero? p) (loop i (bitnot p)))
                     ((< i uplimit) (loop (+ 1 i) (bitnot p)))
                     (else i)
               )
          )
  )
)


(define (output_n_and_A000010_n_upto_uplimit outchan uplimit)
  (let loop ((i (zeqw uplimit)) (p 1'0))
          (seq (outdec_with_2_postdelims
                   outchan
                    (if (zero? p) i (A000010_naive i))
                    (if (zero? p) 58 13)
                    (if (zero? p) 32 10)
               )
               (cond ((zero? p) (loop i (bitnot p)))
                     ((< i uplimit) (loop (+ 1 i) (bitnot p)))
                     (else i)
               )
          )
  )
)


(define (outdec_with_predelim outchan n delim-byte)
   (let loop ((n n)
              (i 1)
              (c0 0) (c1 0) (c2 0) (c3 0) (c4 0)
              (c5 0) (c6 0) (c7 0) (c8 0) (c9 0)
             )
      (seq-if (and (zero? n) (> i 1)) ;; Make sure 0 is output as '0'
              (outchars11 outchan i delim-byte c0 c1 c2 c3 c4 c5 c6 c7 c8 c9)
              (loop (quotient n 10)
                    (1+ i)
                    (+ 48 (remainder n 10)) ;; '0' is 48 in ascii.
                    c0 c1 c2 c3 c4 c5 c6 c7 c8 ;; Shift one right...
              )
      )
   )
)


