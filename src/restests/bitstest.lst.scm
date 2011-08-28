
("bits non-integer limit(s). Should succeed now."
;; Not anymore: (Expecting TYPE MISMATCH: the uplimit and/or lowlimit invalid in call to bits.)
  (let ((a 5'0) (b 3))
     (bits a 4 b)
  )
  ((lambda (5'a (atleast 2)'b) (bits 5'a (atleast 3)'4 (atleast 2)'b)) 5'0 (atleast 2)'3)
)

("bits non-integer limit(s). Should succeed now."
;; Not anymore: (Expecting TYPE MISMATCH: the uplimit and/or lowlimit invalid in call to bits.)
  (let ((a 5'0))
     (bits a (+ 1 2) 0)
  )
  ((lambda (5'a) (bits 5'a (atleast 2)'(+ (atleast 2)'1 (atleast 2)'2) (atleast 1)'0)) 5'0)
)


("bits reversed limit. (Expecting TYPE MISMATCH: the uplimit and/or lowlimit invalid in call to bits.)"
  (let ((a 5'0))
     (bits a 2 4)
  )
  #f
)



("bits reversed limit. (Expecting TYPE MISMATCH: the upper bit index of bits greater than or equal to the definite width of its first arg: (5>=5)."
  (let ((a 5'0))
     (bits a 5 1)
  )
  #f
)


("bits full limits. Should succeed."
  (let ((a 5'0))
     (= a (bits a 4 0))
  )
  boolean'((lambda (5'a) boolean'(= 5'a 5'(bits 5'a 4 0))) 5'0)
)

("bits limits. Should succeed."
  (let ((a 0))
     (= a (bits a 4 0))
  )
  boolean'((lambda (5'a) boolean'(= 5'a 5'(bits 5'a 4 0))) 5'0)
)

("bits limits. Should succeed."
  (let ((a 0) (b 0))
     (bitor b (bits a 4 0))
  )
  5'((lambda ((atleast 5)'a 5'b) 5'(bitor 5'b 5'(bits (atleast 5)'a 4 0))) (atleast 5)'0 5'0)
)

("bits limits. Should succeed."
  (let ((a 1234) (b 0))
     (bitor b (bits a 3 3))
  )
  1'((lambda ((atleast 11)'a 1'b) 1'(bitor 1'b 1'(bits (atleast 11)'a 3 3))) (atleast 11)'1234 1'0)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
