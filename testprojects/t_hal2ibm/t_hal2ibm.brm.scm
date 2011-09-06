
(-MODULE-INFO-BEGIN-)
;;
;; t_hal2ibm.brm.scm - A simple test testing UART routines.
;; Copyright (C) 2010-2011 Antti Karttunen, and subject to GPL v2.
;;
(-MODULE-INFO-END-)

   (let loop ()
      (let ((c (inbyte115200leds in_uart_rxd out_leds)))
         (seq
             (outbyte115200 out_uart_txd
                            (ifnz (bitand c 64) (+ c 1) c)
             )
             (loop)
         )
      )
   )

