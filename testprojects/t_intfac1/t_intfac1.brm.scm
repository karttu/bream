
(-MODULE-INFO-BEGIN-)
;;
;; t_intfac1.brm.scm - A very first simple test program for Bream compiler.
;; Copyright (C) 2010-2011 Antti Karttunen, and subject to GPL v2.
;; This outputs numbers 0 - 10! in factorial number system notation.
;; That is, from 0 to 1000000000
;;
;; CHANGES
;;
;; Edited    Aug 28 2011 by karttu.
;;   Renamed the I/O argument txd to out_uart_txd (by the new naming scheme).
;;   Use macro print_dec_with_crlf for printing out the decimal numbers.
;;   Corrected the upper limit to conform with our claims above...
;;
(-MODULE-INFO-END-)

  (let loop ((i 22'0))
    (seq
          (print_fac_with_crlf out_uart_txd i)
          (seq-if (= 3628800 i) i (loop (+ i 1)))
    )
  )

