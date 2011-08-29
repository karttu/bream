
(-MODULE-INFO-BEGIN-)
;;
;; test1out.brm.scm - A very first simple test program for Bream compiler.
;; Copyright (C) 2010-2011 Antti Karttunen, and subject to GPL v2.
;;
(-MODULE-INFO-END-)

;; (let loop ((c 8'32))
;;   (if (= c 126) c
;;       (seq
;;         (outbyte115200 out_uart_txd c)
;;         (loop (+ c 1))
;;       )
;;   )
;; )

;; The next version was just for testing how seq compiles.
;; Currently the code produced is not valid Verilog, as WebPack
;; will give the following error message, as expected:
;; ERROR:Xst:528 - Multi-source in Unit <test1out> on signal <out_uart_txd>; this signal is connected to multiple drivers.
;; 

;; (quotient 7'65 7'5)


;; (let loop ((c 8'32))
;;  (if (= c 126) c
;;      (seq
;;        (outbyte115200 out_uart_txd (+ c 1))
;;        (outbyte115200 out_uart_txd (+ c 2))
;;        (outbyte115200 out_uart_txd (lcm (A000120 (+ c 3)) (+ c 4)))
;;        (loop (- c 4))
;;      )
;;  )
;; )
;; 


(let loop ((i 0))
 (seq-if
     (= i 11) i
     (seq
       (outchars12 out_uart_txd (+w i 2) 13 10 72 101 108 108 111 87 111 114 108 100)
;;     (outchars11 out_uart_txd (+w (zxt i) 4'4) 13 10 84 101 114 118 101 104 100 121 115)
;;     (outchars11 out_uart_txd (+w (zxt i) 4'4) 13 10 84 101 114 118 101 105 115 105 110)
       (loop (+w i 1))
     )
 )
)

;; (let loop ((i 3'7))
;;  (if (zero? i) i
;;     (loop (- i (zxt (outchars11 out_uart_txd (+w (zxt i) 4'4) 13 10 84 101 114 118 101 104 100 121 115))))
;;  )
;; )

