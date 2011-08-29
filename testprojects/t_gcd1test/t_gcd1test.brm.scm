
(-MODULE-INFO-BEGIN-)
;;
;; t_gcd1test.brm.scm - Output n: (gcd (high-half n) (low-half n))
;; Copyright (C) 2010-2011 Antti Karttunen, and subject to GPL v2.
;;
(-MODULE-INFO-END-)


(define-wirm (gcd_hi_lo word) ;; For test sets.
   (zxt (gcd (drop word (ww/2 word))
             (bits word (ww/2-1 word) 0)
        )
   )
)

(define-wirm (lcm_hi_lo word) ;; For test sets.
   (lcm (drop word (ww/2 word))
        (bits word (ww/2-1 word) 0)
   )
)

 ((lambda (outchan uplimit)
   (let loop ((i (bitxor uplimit uplimit)) (p 1'0))
           (seq (outdec_with_2_postdelims
                    outchan
                     (if (zero? p) i (gcd_hi_lo i))
                     (if (zero? p) 58 13) ;; Output either ": " or CR+LF.
                     (if (zero? p) 32 10)
                )
                (cond ((zero? p) (loop i (bitnot p)))
                      ((< i uplimit) (loop (+ 1 i) (bitnot p)))
                      (else i)
                )
           )
   )
  )
  out_uart_txd
  20'1048575
 )

