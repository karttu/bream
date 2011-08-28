
(-MODULE-INFO-BEGIN-)
;;
;; t_switchprint.brm.scm - Output the binary number switch[7:0] to UART in dec.
;; Copyright (C) 2010-2011 Antti Karttunen, and subject to GPL v2.
;;
(-MODULE-INFO-END-)

  (let loop ((switches switch))
    (seq
          (outdec_with_2_postdelims txd switches 13 10)
          (wait_until (!= switches switch))
          (loop switch)
    )
  )

