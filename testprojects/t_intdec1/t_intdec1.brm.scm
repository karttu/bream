
(-MODULE-INFO-BEGIN-)
;;
;; t_intdec1.brm.scm - A very first simple test program for Bream compiler.
;; Copyright (C) 2010-2011 Antti Karttunen, and subject to GPL v2.
;;
;; CHANGES
;;
;; Edited    Aug 28 2011 by karttu.
;;   Renamed the I/O argument txd to out_uart_txd (by the new naming scheme).
;;   Use macro print_dec_with_crlf for printing out the decimal numbers.
;;
(-MODULE-INFO-END-)

  (let loop ((i 16'0))
    (seq
          (print_dec_with_crlf out_uart_txd i)
          (seq-if (= 1 (redand i)) i (loop (+ i 1)))
    )
  )

