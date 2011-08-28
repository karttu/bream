
(-MODULE-INFO-BEGIN-)
;;
;; t_inttests.brm.scm - A suite of tests for various integer functions.
;; Copyright (C) 2010-2011 Antti Karttunen, and subject to GPL v2.
;;
(-MODULE-INFO-END-)

(define-wirm (*c_hi_lo word) ;; For test sets.
   (*c (drop word (ww/2 word))
       (bits word (ww/2-1 word) 0)
   )
)

;; (define (output_n_and_A000010_n_upto_uplimit outchan uplimit)
 ((lambda (outchan 18'uplimit)
   (let loop ((i (bitxor uplimit uplimit)) (p 1'0))
           (seq (outdec_with_2_postdelims
                    outchan
                     (if (zero? p) i (lcm_hi_lo i)) ;; (A000010_naive_par8 i)
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
  txd
  18'262143
 )
;; )


