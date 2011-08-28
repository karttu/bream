
;; Try e.g.
;(run-typeresolve-testset "/home/karttu/bream/src/restests/droptest.lst.scm" 1)
;;

;; Edited May 29-30 2011 by karttu.
;;

;;;;;;;;;

;;
;;    etype           t1 
;;     DEF            DEF    Should hold: width(t1) = width(etype)+n
;;                           (i.e. width(etype) = width(t1)-n)
;;                           


("drop test. Should succeed, dropping three bits from a definite width 5 and matching it against definite width 2."
  (let ((a 5'17) (b 2'3))
     (+ (drop a 3) b)
  )
  2'((lambda (5'a 2'b) 2'(+ 2'(drop 5'a 3) 2'b)) 5'17 2'3)
)


("drop test. Should fail, dropping three bits from a definite width 5 and matching it against definite width 1. (Expecting TYPE MISMATCH: expected type 2 doesn't match with type of src 1)."
  (let ((a 5'17) (b 1'0))
     (+ (drop a 3) b)
  )
  #f
)

("drop test. Should fail, dropping three bits from a definite width 5 and matching it against definite width 3. (Expecting TYPE MISMATCH: expected type 2 doesn't match with type of src 3)."
  (let ((a 5'17) (b 3'5))
     (+ (drop a 3) b)
  )
  #f
)


("drop test. Should fail, dropping six bits from a definite width 5 and matching it against definite width 3. (Expecting TYPE MISMATCH: the number of bits to drop invalid in call to drop-bits.)"
  (let ((a 5'17) (b 3'5))
     (+ (drop a 6) b)
  )
  #f
)


;;
;;    INDEF           DEF    Should hold: width(t1) >= width(etype)+n
;;                           (i.e. width(etype) <= width(t1)-n)
;;                           (Implies that n >= width(t1) as etype cannot
;;                            have negative width).
;;                           (Fix etype to definite width(t1)-n.)
;;

("drop test. Should fail, dropping six bits from a definite width 5 and matching it against indefinite width 3. (Expecting TYPE MISMATCH: the number of bits to drop invalid in call to drop-bits.)"
  (let ((a 5'17) (b 5))
     (+ (drop a 6) b)
  )
  #f
)


("drop test. Should succeed, dropping five bits from a definite width 6 and matching it against nondefinite width of 1."
  (let ((a 6'55) (b 0))
     (< (drop a 5) b)
  )
  boolean'((lambda (6'a 1'b) boolean'(< 1'(drop 6'a 5) 1'b)) 6'55 1'0)
)


("drop test. Should succeed as width(t1)>=width(etype)+0, dropping zero bits from a definite width 6 and matching it against nondefinite width of 1 of b, raising the latter also to 6."
  (let ((a 6'55) (b 0))
     (- (drop a 0) b)
  )
  6'((lambda (6'a 6'b) 6'(- 6'(drop 6'a 0) 6'b)) 6'55 6'0)
)


("drop test. Should succeed (currently) as width(t1)>=width(etype)+3, dropping 3 bits from a definite width of 3, resulting of type of definite width 0 (a strange beast)"
  (let ((a 3'7)) (drop a 3))
  0'((lambda (3'a) 0'(drop 3'a 3)) 3'7)
)



("drop test. Should succeed, as width(t1)>=width(etype)+5, dropping 5 bits from a definite width 10 and matching it against nondefinite width of 3."
  (let ((a 10'512) (b 7))
     (>= (drop a 5) b)
  )
  boolean'((lambda (10'a 5'b) boolean'(>= 5'(drop 10'a 5) 5'b)) 10'512 5'7)
)


("drop test. Should succeed, as width(t1)>=width(etype)+6, dropping 6 bits from a definite width 10 and matching it against nondefinite width of 3."
  (let ((a 10'512) (b 7))
     (> (drop a 6) b)
  )
  boolean'((lambda (10'a 4'b) boolean'(> 4'(drop 10'a 6) 4'b)) 10'512 4'7)
)


("drop test. Should succeed, as width(t1)=width(etype)+5, dropping five bits from a definite width 6 and matching it against nondefinite width of 1."
  (let ((a 6'55) (b 0))
     (< (drop a 5) b)
  )
  boolean'((lambda (6'a 1'b) boolean'(< 1'(drop 6'a 5) 1'b)) 6'55 1'0)
)


("drop test. Should fail, as width(t1)<width(etype)+5, dropping five bits from a definite width 6 and matching it against nondefinite width of 2. (Expecting TYPE MISMATCH: expected type 1 doesn't match with type of src (atleast 2).)"
  (let ((a 6'55) (b 2))
     (<= (drop a 5) b)
  )
  #f
)


;;
;;     DEF           INDEF   Should hold: width(t1) <= width(etype)+n
;;                           (i.e. n >= width(t1) - width(etype) )
;;                           in that case, fix arg1 to definite width(etype)+n 
;;                           otherwise, a type mismatch.
;;


("drop test. Should succeed, as width(t1) <= width(etype)+3, dropping three bits from nondefinite width 'a', and matching it against a definite width 'b', which should also fix 'a'."
  (let ((a 1234) (b 20'0))
     (bitxnor b (drop a 3))
  )
  20'((lambda (23'a 20'b) 20'(bitxnor 20'b 20'(drop 23'a 3))) 23'1234 20'0)
)


("drop test. Should succeed, as width(t1) = width(etype)+3, dropping three bits from nondefinite width 'a', and matching it against a definite width 'b', which should also fix 'a'."
  (let ((a 255) (b 5'31))
     (bitxnor b (drop a 3))
  )
  5'((lambda (8'a 5'b) 5'(bitxnor 5'b 5'(drop 8'a 3))) 8'255 5'31)
)


("drop test. Should fail, as width(t1) > width(etype)+3, when dropping three bits from nondefinite width 'a' (9), and matching it against a definite width 'b' (5). (Expecting TYPE MISMATCH: the number of bits to drop invalid in call to drop-bits. Expected type (width) of result=5.)"
  (let ((a 257) (b 5'31))
     (bitxnor b (drop a 3))
  )
  #f
)


("drop test. Dropping three bits from nondefinite width 'a', and matching it against a definite width 'b', should fail because a's atleast-width is already too big. (Expecting TYPE MISMATCH: the number of bits to drop invalid in call to drop-bits. Expected type (width) of result=20.)"
  (let ((a 1234567890) (b 20'0))
     (bitxnor b (drop a 3))
  )
  #f
)



;;
;;    INDEF          INDEF
;;                           If width(t1) < width(etype)+n,
;;                           then raise it to width(etype)+n.
;;                           (i.e. if width(etype) is still say 1,
;;                           and width(t1) is 3, and n is 10,
;;                           then width(t1) should be made 11.
;;                           (Here we have case where n > width(t1)).
;;
;;                           If width(t1) > width(etype)+n
;;                           (i.e. width(etype) < width(t1)-n),
;;                           then raise width(etype) to width(t1)-n.
;;                           (If width(t1) == width(etype)+n, then OK already).
;;



("drop test. Should succeed, as width(t1) > width(etype)+3, dropping three bits from nondefinite width 11, and matching it against a nondefinite width of 1 of b, raising the latter to 11-3 = 8."
  (let ((a 1234) (b 0))
     (bitxnor b (drop a 3))
  )
  (atleast 8)'((lambda ((atleast 11)'a (atleast 8)'b) (atleast 8)'(bitxnor (atleast 8)'b (atleast 8)'(drop (atleast 11)'a 3))) (atleast 11)'1234 (atleast 8)'0)
)


("drop test. Should succeed, as width(t1) > width(etype)+0, dropping zero bits from indefinite width."
  (let ((a 1234) (b 0))
     (bitand b (drop a 0))
  )
  (atleast 11)'((lambda ((atleast 11)'a (atleast 11)'b) (atleast 11)'(bitand (atleast 11)'b (atleast 11)'(drop (atleast 11)'a 0))) (atleast 11)'1234 (atleast 11)'0)
)


("drop test. Should succeed, as width(t1) (atleast 5) = width(etype)+3 (2+3), keeping both a's and b's indefinite widths same."
  (let ((a 25) (b 2)) (bitand b (drop a 3)))
  (atleast 2)'((lambda ((atleast 5)'a (atleast 2)'b) (atleast 2)'(bitand (atleast 2)'b (atleast 2)'(drop (atleast 5)'a 3))) (atleast 5)'25 (atleast 2)'2)
)


("drop test. Should succeed, as width(t1) (atleast 3) < width(etype)+3 (1+3), raising the indefinite width of 'a' from 3 to 4."
  (let ((a 7) (b 1)) (bitand b (drop a 3)))
  (atleast 1)'((lambda ((atleast 4)'a (atleast 1)'b) (atleast 1)'(bitand (atleast 1)'b (atleast 1)'(drop (atleast 4)'a 3))) (atleast 4)'7 (atleast 1)'1)
)


("drop test. Should succeed, as width(t1) < width(etype)+3, dropping more bits than the current atleast width of 'a' raises the width of 'a'"
  (let ((a 0)) (drop a 3))
  ((lambda ((atleast 3)'a) (drop (atleast 3)'a 3)) (atleast 3)'0)
)


("drop test. Should succeed (currently), resulting/reverting to type of completely-unresolved (instead of (type-i-of-atleast-width 0))."
  (let ((a 7)) (drop a 3))
;; Used to give this:
;; (atleast 0)'((lambda ((atleast 3)'a) (atleast 0)'(drop (atleast 3)'a 3)) (atleast 3)'7)
;; But now produces:
   ((lambda ((atleast 3)'a) (drop (atleast 3)'a 3)) (atleast 3)'7)
)


;;
;; Some circularity tests:
;;

("drop test with simple circularity, with a definite width. Should fail. (Expecting TYPE MISMATCH: expected type 10 doesn't match with type of src 11.)"
  (let ((a 11'1234))
    (bitxor (drop a 1) a)
  )
  #f
)

("Testing drop in loop, with 'a' definite, should fail. (Expecting TYPE MISMATCH: expected type 10 doesn't match with type of src 11.)"
  (let loop ((i 89) (a 11'1234))
     (if (zero? i)
         a
         (loop (- i 1) (drop a 1))
     )
  )
  #f
)


("drop test with simple circularity, with nondefinite width. (Expecting TOO MANY TYPE RESOLVING PASSES.)"
  (let ((a 1234))
    (bitxor (drop a 1) a)
  )
  #f
)

("Testing drop in loop, with 'a' nondefinite. (Expecting TOO MANY TYPE RESOLVING PASSES.)"
  (let loop ((i 89) (a 123))
     (if (zero? i)
         a
         (loop (- i 1) (drop a 1))
     )
  )
  #f
)

