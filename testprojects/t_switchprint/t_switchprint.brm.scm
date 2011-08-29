
(-MODULE-INFO-BEGIN-)
;;
;; t_switchprint.brm.scm - Output the binary number specified by the eight
;;  slide switches (i.e. in_switches[7:0]) to UART as a decimal number.
;;  This will output a single integer after a start, and after that,
;;  a new integer on a new line, always after the user has changed
;;  some of the switches.
;;
;; Copyright (C) 2010-2011 Antti Karttunen, and subject to GPL v2.
;;
;; CHANGES
;;
;; Edited    Aug 28 2011 by karttu.
;;   Renamed the I/O arguments according to the new naming scheme.
;;   Use macro print_dec_with_crlf for printing out the decimal numbers.
;;
(-MODULE-INFO-END-)

  (let loop ((copy_of_switches in_switches))
    (seq (print_dec_with_crlf out_uart_txd copy_of_switches)
         (wait_until (!= in_switches copy_of_switches))
         (loop in_switches)
    )
  )

