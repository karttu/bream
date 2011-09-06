
(declare-extfun 1'(outbyte1200 output'outchan 8'byte_to_output))
(declare-extfun 1'(outbyte115200 output'outchan 8'byte_to_output))
(declare-extfun 1'(outbyte115200ub output'outchan 8'byte_to_output))

(declare-extfun 8'(inbyte115200leds 1'inchan (output 7)'statusleds))

(declare-extfun 1'(wait_until_start_debounced)) ;; Experimental...
(declare-extfun 1'(wait_until boolean'condition))


