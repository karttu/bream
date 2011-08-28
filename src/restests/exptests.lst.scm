
;; Run as
;(run-expsynta-testset "/home/karttu/bream/src/restests/exptests.lst.scm" 1)
;; (For example).

("IF-Test #n"
 (if (= 32 256) (bitxor 1 4) (if (zero? 3) (+ 2 8) (- 16 2)))
 (if (= 32 256) (bitxor 1 4) (if (zero? 3) (+ 2 8) (- 16 2)))
)

("IFZ-Test"
 (ifz a b c)
 (if boolean'(zero? a) b c)
)

