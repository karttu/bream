
(-MODULE-INFO-BEGIN-)
;;
;; t_inttests.brm.scm - A suite of tests for various integer functions.
;; Now testing the computation of http://oeis.org/A000010 (i.e. "phi"),
;; with a heavily parallelized naive algorithm.
;;
;; Copyright (C) 2010-2011 Antti Karttunen, and subject to GPL v2.
;;
(-MODULE-INFO-END-)

(define-wirm (*c_hi_lo word) ;; For testing *c
   (*c (drop word (ww/2 word))
       (bits word (ww/2-1 word) 0)
   )
)

 ((lambda (outchan 18'uplimit)
   (let loop ((n (zeqw uplimit)) (p 1'0))
           (seq (outdec_with_2_postdelims
                    outchan
                     (if (zero? p) n (A000010_naive_par_aux_i_times n 31))
                     (if (zero? p) 58 13) ;; Output either ": " or CR+LF.
                     (if (zero? p) 32 10)
                )
                (cond ((zero? p) (loop n (bitnot p)))
                      ((< n uplimit) (loop (+ 1 n) (bitnot p)))
                      (else n)
                )
           )
   )
  )
  out_uart_txd
  18'262143
 )


