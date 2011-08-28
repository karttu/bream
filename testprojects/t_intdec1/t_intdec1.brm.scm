
(-MODULE-INFO-BEGIN-)
;;
;; t_intdec1.brm.scm - A very first simple test program for Bream compiler.
;; Copyright (C) 2010-2011 Antti Karttunen, and subject to GPL v2.
;;
(-MODULE-INFO-END-)

  (let loop ((i 16'0))
    (seq
          (outdec_with_2_postdelims TXD i 13 10)
;;        (outchars12 TXD 3 (+w 8'48 (zxt (remainder i 10))) 13 10 
;;                    0 0 0 0 0 0 0 0 0
;;        )
          (seq-if (= 1 (redand i)) i (loop (+ i 1)))
    )
  )

