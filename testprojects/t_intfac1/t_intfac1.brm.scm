
(-MODULE-INFO-BEGIN-)
;;
;; t_intfac1.brm.scm - A very first simple test program for Bream compiler.
;; Copyright (C) 2010-2011 Antti Karttunen, and subject to GPL v2.
;; This outputs numbers 0 - 10! in factorial number system notation.
;; That is, from 0 to 1000000000
;;
(-MODULE-INFO-END-)

  (let loop ((i 22'0))
    (seq
          (outfac_with_2_postdelims TXD i 13 10)
          (seq-if (= 3628801 i) i (loop (+ i 1)))
    )
  )

