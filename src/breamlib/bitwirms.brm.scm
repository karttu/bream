
(define-wirm (ew2) (EW)) ;; Just for testing (ew)'s implementation.

;; Just wrappers for W, W-1 and W+1, currently (temporarily) needed:

(define-wirm (ww x) (W x))
(define-wirm (ww-1 x) (W-1 x))
(define-wirm (ww+1 x) (W+1 x))

(define-wirm (ww/2 x) (>> (W x) 1))
(define-wirm (ww/2-1 x) (-1+ (>> (W x) 1)))

(define-wirm (revbits b) :type-resolving-form <equitype-etype-and-args>
   (if (< (W b) 2) b
       (conc (bit b 0) (revbits (bits b (W-1 b) 1)))
   )
)

;; Currently we cannot use this one. Although it expands correctly,
;; the compiler will produce invalid Verilog-code for width-0 lambda
;; args. (They should be filtered out from the final expanded lambda
;; before that.)
(define-wirm (mult0 x y)
   (if (zero? (W x)) 0
       (+ (mult0 (drop x 1) (<< y 1))
          (if (odd? x) y 0)
       )
   )
)

;; So instead, use this one:
(define-wirm (mult x y)
   (if (= 1 (W x))
       (if (odd? x) y 0)
       (+ (mult (drop x 1) (<< y 1))
          (if (odd? x) y 0)
       )
   )
)

;; Doesn't work yet.
(define-wirm (mult/c x y)
   (if (= 1 (W x))
       (if (odd? x) y 0)
       (+ (mult/c (drop x 1) (conc2 y 1'0))
          (if (odd? x) (conc2 1'0 y) 0)
       )
   )
)


;; A+B = (A XOR B) + 2*(A AND B).  See Schroeppel, HAKMEM Item 23.
;; (Also A+B = (A OR B) + (A AND B) which is not amenable to recursion).

;; (define (add a b) (if (zero? b) a (add (bitxor a b) (<< (bitand a b) 1))))

(define-wirm (add a b) :type-resolving-form <equitype-etype-and-args>
   (if (< (EW) 2)
       (bitxor a b)
       (conc (add (drop (bitxor a b) 1)
                  (bits (bitand a b) (- (W-1 a) 1) 0)
             )
             (bitxor (bit a 0) (bit b 0))
       )
   )
)

;; A naive implementation:
(define-wirm (addv2 a b) :type-resolving-form <equitype-etype-and-args>
   (if (< (EW) 2)
       (bitxor a b)
       (conc (addv2 (addv2 (drop a 1) (drop b 1))
                    (zxt (bitand (bit a 0) (bit b 0)))
             )
             (bitxor (bit a 0) (bit b 0))
       )
   )
)

;; (addv1 (drop (bitxor a b) 1) ..)

;; How to implement ripple-adder? Carry-look-ahead adder?



(define-wirm (inc a) :type-resolving-form <equitype-etype-and-args>
  (if (= 1 (W a))
      (bitnot a)
      (let ((b (inc (bits a (-1+ (W-1 a)) 0))))
          (conc (bitxor (bit a (W-1 a)) (rednor b))
                b
          )
      )
  )
)


(define-wirm (inc-alt x) :type-resolving-form <equitype-etype-and-args>
  (let INCLOOP ((z (bitnot (bit x 0)))
                (i 1) ;; A marker that is a "compile time variable"
                (b (bit x 0))
               )
       (cond ((= i (EW)) z) ;; or: (= i (W x)) ?
             (else (INCLOOP (conc (bitxor (bit x i) b) z)
                            (1+ i) ;; Must be simplevaled at compile time!
                            (bitand (bit x i) b)
                   )
             )
       )
  )
)


;; This kind of code currently leaves a trail of unnecessary wires
;; n_2, n_3, n_4, ... I hope Verilog-compiler will prune them away.
(define-wirm (zow n) ;; Make zero of width n.
   (if (= 1 n)
       1'0
       (conc2 1'0 (zow (- n 1)))
   )
)

;; A bug/misfeature: this doesn't have an effect until wirm-expansion
;; time, so often (bitxor x x) or (- x x) gives better results:
;; (define-wirm (zeqw x) ;; Make Zero of Equal Width as x has.
;;   (bitxor x x) ;; A hack. Verilog-compiler should know to optimize this.
;; )

;; Come on now, the solution is really simple:
(define-wirm (zeqw x) :type-resolving-form <equitype-etype-and-args>
 0
)

;; A similar macro hat creates zero of of one narrower dimension:
;; (And the heaven will fall if you use this with x of width 1 !)
(define-wirm (zeqw-1 x) :type-resolving-form <etype-one-narrower-than-arg>
 0
)


(define-wirm (zxt a)
;; :type-resolving-form <force-etype-definite> ;;Not one of my brightest ideas.
;; :type-resolving-form <etype-atleast-as-wide-as-the-arg>
   (if (= (W a) (EW))
       a
       (conc 1'0 (zxt a))
   )
)


(define-wirm (zxt-alt a) ;; :type-resolving-form <etype-atleast-as-wide-as-the-arg>
   (if (= (W a) (EW))
       a
       (zxt-alt (conc 1'0 a))
   )
)

(define-wirm (sxt a)
;; :type-resolving-form <force-etype-definite>
;; :type-resolving-form <etype-atleast-as-wide-as-the-arg>
   (if (= (W a) (EW))
       a
       (conc (bit a (W-1 a)) (sxt a))
   )
)


(define-wirm (sxt-alt a) ;; :type-resolving-form <etype-atleast-as-wide-as-the-arg>
   (if (= (W a) (EW))
       a
       (sxt-alt (conc (bit a (W-1 a)) a))
   )
)


(define-wirm 2'(add-mod3 a b) ;; XXX -- Think about widths and carries!
   (if (> (+C a b) 5) (- (+C a b) 6)
       (if (> (+C a b) 2) (- (+C a b) 3) (+ a b))
   )
)

(define-wirm 2'(A010872 n)
   (cond ((< (W n) 2) n)
         ((= (W n) 2) (add-mod3 n 0))
         (else (add-mod3 (A010872 (bits n (W-1 n) (>> (W n) 1)))
                         (A010872 (bits n (-1+ (>> (W n) 1)) 0))
               )
         )
   )
)

