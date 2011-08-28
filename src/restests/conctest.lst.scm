

;; Run as (run-typeresolve-testset "/home/karttu/bream/src/restests/conctest.lst.scm" 100)
;; (For example).

("Testing conc2"
  (conc2 1 2)
  (atleast 3)'(conc2 (atleast 1)'1 (atleast 2)'2)
)


("Testing conc, sum of widths 1, 2 and 3 should be 6."
  (conc 1 2 4)
  (atleast 6)'(conc2 (atleast 1)'1 (atleast 5)'(conc2 (atleast 2)'2 (atleast 3)'4))
)

;;
;; Test these cases and some variations:
;;
;;    etype           t1             t2
;;     DEF            DEF            DEF   Should hold: etype = t1+t2
;;
;;     DEF0           DEF1          INDEF   types-match(DEF0,DEF1+INDEF)
;;                                         i.e. INDEF must be less than
;;                                                DEF0-DEF1, and DEF1<=DEF0
;;                                         we can infer that INDEF=DEF0-DEF1
;;     DEF0          INDEF          DEF2   (as above, mutatis mutandis).
;;
;;     DEF           INDEF          INDEF   types-match(DEF,INDEF+INDEF)
;;     (Note that this case should match only when the sum of t1's and t2'
;;      atleast-widths is <= etype's definite width.
;;      However, if their sum would be exactly etype's definite width,
;;      and if we used this occassion for fixing either or both of them
;;      (to definite ones), then it would probably introduce subtle
;;      call-tree search-order dependencies
;;      to our type-resolving process, or the overall success might
;;      require backtracking, which we don't want to do now.)
;;
;;    INDEF           DEF            DEF    types-match(INDEF,t1+t2)
;;
;;    INDEF           DEF1          INDEF2  types-match(INDEF,DEF1+INDEF2)
;;                                          (we cannot infere any definite
;;                                           widths from this).
;;    INDEF          INDEF1          DEF2  (as above, mutatis mutandis)
;;
;;
;;    INDEF          INDEF          INDEF   should match always, because
;;                                          all values can still grow.
;;
;;


("Testing case DEF = DEF+DEF"
  7'(conc 4'9 3'5)
  7'(conc2 4'9 3'5)
)

("Testing case DEF != DEF+DEF (Expecting: TYPE MISMATCH: expected type 6 doesn't sum to the widths of types 4 and 3)"
  6'(conc 4'9 3'5)
  #f
)

("Testing case DEF != DEF+DEF (Expecting: TYPE MISMATCH: expected type 8 doesn't sum to the widths of types 4 and 3)"
  8'(conc 4'9 3'5)
  #f
)


("Testing case DEF0 < DEF1+INDEF (Expecting: TYPE MISMATCH: expected type 6 doesn't sum to the widths of types 4  and (atleast 3))"
  6'(conc 4'9 5)
  #f
)

("Testing case DEF0 < DEF1+INDEF (Expecting: TYPE MISMATCH: expected type 6 doesn't sum to the widths of types 4 and (atleast 3))"
  (let ((a 4'9) (b 5))
     6'(conc a b)
  )
  #f
)


("Testing case DEF0 = INDEF+DEF2 (should succeed)"
  7'(conc 5 4'9)
  7'(conc2 3'5 4'9)
)

("Testing case DEF0 = DEF1+INDEF (in let, with b's width definitivized.)"
  (let ((a 4'9) (b 5))
     7'(conc a b)
  )
  7'((lambda (4'a 3'b) 7'(conc2 4'a 3'b)) 4'9 3'5)
)

("Testing case DEF0 > INDEF1+DEF2 (should succeed.)"
 8'(conc 9 3'5)
 8'(conc2 5'9 3'5)
)

("Testing case DEF0 > DEF1+INDEF (should succeed)"
  (let ((a 4'9) (b 5))
     8'(conc a b)
  )
  8'((lambda (4'a 4'b) 8'(conc2 4'a 4'b)) 4'9 4'5)
)

("Testing case DEF1 < INDEF1 + INDEF2 (Expecting: TYPE MISMATCH: expected type 9  doesn't sum to the widths of types (atleast 4) and (atleast 6))"
  (let ((a 12) (b 33))
     9'(conc a b)
  )
 #f
)

("Testing case DEF1 = INDEF1 + INDEF2"
  (let ((a 32) (b 8))
     10'(conc a b)
  )
  10'((lambda ((atleast 6)'a (atleast 4)'b) 10'(conc2 (atleast 6)'a (atleast 4)'b)) (atleast 6)'32 (atleast 4)'8)
)


("Testing case DEF1 > INDEF1 + INDEF2"
  (let ((a 11) (b 61))
     20'(conc a b)
  )
  20'((lambda ((atleast 4)'a (atleast 6)'b) 20'(conc2 (atleast 4)'a (atleast 6)'b)) (atleast 4)'11 (atleast 6)'61)
)


("Testing case DEF1 = INDEF1 + INDEF1"
  (let ((a 25))
     10'(conc a a)
  )
  10'((lambda ((atleast 5)'a) 10'(conc2 (atleast 5)'a (atleast 5)'a)) (atleast 5)'25)
)


("Testing case DEF1 > INDEF1 + INDEF1"
  (let ((a 25))
     20'(conc a a)
  )
  20'((lambda ((atleast 5)'a) 20'(conc2 (atleast 5)'a (atleast 5)'a)) (atleast 5)'25)
)


("Testing case DEF1 > INDEF1 + INDEF1 (but odd, so with more sophisticated analysis this would fail immediately)"
  (let ((a 25))
     19'(conc a a)
  )
  19'((lambda ((atleast 5)'a) 19'(conc2 (atleast 5)'a (atleast 5)'a)) (atleast 5)'25)
)


("Testing case INDEF < DEF1+DEF2 (should succeed)"
  (let ((a 5'25) (b 4'9))
     (conc a b)
  )
  9'((lambda (5'a 4'b) 9'(conc2 5'a 4'b)) 5'25 4'9)
)

("Testing case INDEF < DEF1+DEF2 (should succeed)"
  (let ((a 5'25) (b 4'9))
     (let loop ((c 63))
        (loop (conc a b))
     )
  )
  ((lambda (5'a 4'b) (<let-named> loop ((9'c 9'63)) (loop 9'(conc2 5'a 4'b)))) 5'25 4'9)
)


("Testing case INDEF = DEF1+DEF2 (should succeed)"
  (let ((a 5'25) (b 4'9))
     (let loop ((c 257))
        (loop (conc b a))
     )
  )
  ((lambda (5'a 4'b) (<let-named> loop ((9'c 9'257)) (loop 9'(conc2 4'b 5'a)))) 5'25 4'9)
)


("Testing case INDEF > DEF1+DEF2 (Expecting: TYPE MISMATCH: expected type 9 doesn't match with type of src (atleast 10))"
  (let ((a 5'25) (b 4'9))
     (let loop ((c 1023))
        (loop (conc b a))
     )
  )
 #f
)



("Testing case INDEF < INDEF1+DEF2 (should succeed)"
  (let ((a 25) (b 4'9))
     (conc a b)
  )
 (atleast 9)'((lambda ((atleast 5)'a 4'b) (atleast 9)'(conc2 (atleast 5)'a 4'b)) (atleast 5)'25 4'9)
)


("Testing case INDEF < DEF1+INDEF2 (should succeed)"
  (let ((a 5'25) (b 9))
     (let loop ((c 63))
        (loop (conc a b))
     )
  )
  ((lambda (5'a (atleast 4)'b) (<let-named> loop (((atleast 9)'c (atleast 9)'63)) (loop (atleast 9)'(conc2 5'a (atleast 4)'b)))) 5'25 (atleast 4)'9)
)


("Testing case INDEF = DEF1+INDEF2 (should succeed)"
  (let ((a 25) (b 4'9))
     (let loop ((c 257))
        (loop (conc b a))
     )
  )
  ((lambda ((atleast 5)'a 4'b) (<let-named> loop (((atleast 9)'c (atleast 9)'257)) (loop (atleast 9)'(conc2 4'b (atleast 5)'a)))) (atleast 5)'25 4'9)
)


("Testing case INDEF > DEF1+INDEF2 (should succeed)"
  (let ((a 5'25) (b 9))
     (let loop ((c 1023))
        (loop (conc a b))
     )
  )
  ((lambda (5'a (atleast 4)'b) (<let-named> loop (((atleast 10)'c (atleast 10)'1023)) (loop (atleast 10)'(conc2 5'a (atleast 4)'b)))) 5'25 (atleast 4)'9)
)



("Testing case INDEF < INDEF1+INDEF2 (should succeed)"
  (let ((a 0) (b 0))
     (conc a b)
  )
  (atleast 2)'((lambda ((atleast 1)'a (atleast 1)'b) (atleast 2)'(conc2 (atleast 1)'a (atleast 1)'b)) (atleast 1)'0 (atleast 1)'0)
)

("Testing case INDEF < INDEF1+INDEF2+INDEF3+INDEF4+INDEF5 (should succeed)"
  (let ((a 0) (b 0) (c 0) (d 0) (e 0))
     (conc a b c d e)
  )
  (atleast 5)'((lambda ((atleast 1)'a (atleast 1)'b (atleast 1)'c (atleast 1)'d (atleast 1)'e) (atleast 5)'(conc2 (atleast 1)'a (atleast 4)'(conc2 (atleast 1)'b (atleast 3)'(conc2 (atleast 1)'c (atleast 2)'(conc2 (atleast 1)'d (atleast 1)'e))))) (atleast 1)'0 (atleast 1)'0 (atleast 1)'0 (atleast 1)'0 (atleast 1)'0)
)


("Testing case INDEF < INDEF1+INDEF2 (should succeed)"
  (let ((a 25) (b 9))
     (conc a b)
  )
  (atleast 9)'((lambda ((atleast 5)'a (atleast 4)'b) (atleast 9)'(conc2 (atleast 5)'a (atleast 4)'b)) (atleast 5)'25 (atleast 4)'9)
)

("Testing case INDEF < INDEF1+INDEF2 (should succeed)"
  (let ((a 25) (b 9))
     (let loop ((c 63))
        (loop (conc a b))
     )
  )
  ((lambda ((atleast 5)'a (atleast 4)'b) (<let-named> loop (((atleast 9)'c (atleast 9)'63)) (loop (atleast 9)'(conc2 (atleast 5)'a (atleast 4)'b)))) (atleast 5)'25 (atleast 4)'9)
)

("Testing case INDEF = DEF1+INDEF2 (should succeed)"
  (let ((a 25) (b 9))
     (let loop ((c 257))
        (loop (conc b a))
     )
  )
  ((lambda ((atleast 5)'a (atleast 4)'b) (<let-named> loop (((atleast 9)'c (atleast 9)'257)) (loop (atleast 9)'(conc2 (atleast 4)'b (atleast 5)'a)))) (atleast 5)'25 (atleast 4)'9)
)


("Testing case INDEF > INDEF1+INDEF2 (should succeed)"
  (let ((a 25) (b 9))
     (let loop ((c 1023))
        (loop (conc a b))
     )
  )
  ((lambda ((atleast 5)'a (atleast 4)'b) (<let-named> loop (((atleast 10)'c (atleast 10)'1023)) (loop (atleast 10)'(conc2 (atleast 5)'a (atleast 4)'b)))) (atleast 5)'25 (atleast 4)'9)
)


("Testing case INDEF < DEF1+DEF2+DEF3+DEF4+DEF5 (should succeed, with r set to width 15)"
  (let ((a 1'1) (b 2'2) (c 3'5) (d 4'9) (e 5'17))
     (let loop ((r 0))
        (loop (conc a b c d e))
     )
  )
  ((lambda (1'a 2'b 3'c 4'd 5'e) (<let-named> loop ((15'r 15'0)) (loop 15'(conc2 1'a 14'(conc2 2'b 12'(conc2 3'c 9'(conc2 4'd 5'e))))))) 1'1 2'2 3'5 4'9 5'17)
)


("Testing case INDEF < DEF1+DEF2+DEF3+DEF4+DEF5 (should succeed, with r set to width 15)"
  (let ((a 1'1) (b 2'2) (c 3'5) (d 4'9) (e 5'17))
     (let loop ((r 0))
        (loop (conc e d b c a))
     )
  )
  ((lambda (1'a 2'b 3'c 4'd 5'e) (<let-named> loop ((15'r 15'0)) (loop 15'(conc2 5'e 10'(conc2 4'd 6'(conc2 2'b 4'(conc2 3'c 1'a))))))) 1'1 2'2 3'5 4'9 5'17)
)


("Testing case DEF0 = DEF1+DEF2+DEF3+DEF4+DEF5 (should succeed)"
  (let ((a 1'1) (b 2'2) (c 3'5) (d 4'9) (e 5'17))
     (let loop ((r 15'0))
        (loop (conc a b c d e))
     )
  )
  ((lambda (1'a 2'b 3'c 4'd 5'e) (<let-named> loop ((15'r 15'0)) (loop 15'(conc2 1'a 14'(conc2 2'b 12'(conc2 3'c 9'(conc2 4'd 5'e))))))) 1'1 2'2 3'5 4'9 5'17)
)


("Testing case DEF0 = DEF1+DEF2+INDEF3+DEF4+DEF5 (should succeed, with c set to width 3. Currently takes 6 passes.)"
  (let ((a 1'1) (b 2'2) (c 1) (d 4'9) (e 5'17))
     (let loop ((r 15'0))
        (loop (conc a b c d e))
     )
  )
  ((lambda (1'a 2'b 3'c 4'd 5'e) (<let-named> loop ((15'r 15'0)) (loop 15'(conc2 1'a 14'(conc2 2'b 12'(conc2 3'c 9'(conc2 4'd 5'e))))))) 1'1 2'2 3'1 4'9 5'17)
)


("Testing case DEF0 = DEF1+INDEF2+DEF3+INDEF4+DEF5 (should succeed)"
  (let ((a 1'1) (b 1) (c 3'5) (d 1) (e 5'17))
     (let loop ((r 15'0))
        (loop (conc a b c d e))
     )
  )
  ((lambda (1'a (atleast 1)'b 3'c (atleast 1)'d 5'e) (<let-named> loop ((15'r 15'0)) (loop 15'(conc2 1'a 14'(conc2 (atleast 1)'b (atleast 9)'(conc2 3'c (atleast 6)'(conc2 (atleast 1)'d 5'e))))))) 1'1 (atleast 1)'1 3'5 (atleast 1)'1 5'17)
)


;;
;; Then some pathologies and non-pathologies alike:
;; 


("Testing some simple circularity. (Expecting TOO MANY TYPE RESOLVING PASSES)"
  (let ((a 5))
     (bitxor a (conc 1 (bitnot a)))
  )
  #f
)

("Testing some simple circularity. (Expecting TOO MANY TYPE RESOLVING PASSES)"
  (let ((a 5))
     (if (== a (conc 1 (bitnot a)))
         3'5
         3'7
     )
  )
  #f
)


("Testing some simple circularity. (Expecting TYPE MISMATCH: expected type 3 doesn't sum to the widths of types (atleast 1) and 3)"
  (let ((a 3'5))
     (bitxor a (conc 1 (bitnot a)))
  )
  #f
)


("Testing some simple circularity. (Expecting TYPE MISMATCH: expected type 3 doesn't sum to the widths of types 1 and 3)"
  (let ((a 3'5))
     (bitxor a (conc 1'0 (bitnot a)))
  )
  #f
)



("Testing some simple expression with concs. Should succeed."
  (let ((a 1))
     (rednor (bitxor (conc a 1'0) (conc 1'0 a)))
  )
  1'((lambda ((atleast 1)'a) 1'(rednor (atleast 2)'(bitxor (atleast 2)'(conc2 (atleast 1)'a 1'0) (atleast 2)'(conc2 1'0 (atleast 1)'a)))) (atleast 1)'1)
)


("Testing runaway conc in loop, with 'a' definite, should fail. (Expecting TYPE MISMATCH: expected type 1 doesn't sum to the widths of types 1 and 1)"
  (let loop ((i 89) (a 1'1))
     (if (zero? i)
         a
         (loop (- i 1) (conc 1'1 a))
     )
  )
  #f
)


("Testing runaway conc in loop, with 'a' indefinite, should fail. (Expecting TOO MANY TYPE RESOLVING PASSES)"
  (let loop ((i 89) (a 1))
     (if (zero? i)
         a
         (loop (- i 1) (conc 1'1 a))
     )
  )
  #f
)


("Testing runaway conc in loop, with 'a' and 'b' definite, should fail. (Expecting TYPE MISMATCH: expected type 1 doesn't sum to the widths of types 1 and 2)"
  (let loop ((i 144) (a 1'1) (b 2'2))
       (if (zero? i)
           a
           (loop (- i 1) (conc 1'1 b) (conc a a))
       )
  )
  #f
)


("Testing runaway conc in loop, with 'a' and 'b' indefinite, should fail. (Expecting TOO MANY TYPE RESOLVING PASSES)"
  (let loop ((i 144) (a 1) (b 1))
       (if (zero? i)
           a
           (loop (- i 1) (conc 1'1 b) (conc a a))
       )
  )
  #f
)

("Testing runaway conc in loop, with 'a' and 'b' indefinite, should fail. (Expecting TOO MANY TYPE RESOLVING PASSES)"
  (let loop ((i 144) (a 3) (b 3))
       (if (zero? i)
           a
           (loop (- i 1) (conc b b) (conc a a))
       )
  )
  #f
)


("Testing runaway conc in loop with 'a' and 'b' indefinite, should fail. (Expecting TOO MANY TYPE RESOLVING PASSES)"
  (let loop ((a 3) (b 3))
       (if (zero? (rednor (bitxor a b)))
           1'0
           (loop (conc a 1'0) (conc 1'0 b))
       )
  )
  #f
)


("Testing conc in loop, this should work."
  (let loop ((i 233) (a 0))
       (if (zero? i)
           a
           (loop (- i 1) (conc i i))
       )
  )
  (atleast 16)'(<let-named> loop (((atleast 8)'i (atleast 8)'233) ((atleast 16)'a (atleast 16)'0)) (atleast 16)'(if boolean'(zero? (atleast 8)'i) (atleast 16)'a (atleast 16)'(loop (atleast 8)'(- (atleast 8)'i (atleast 8)'1) (atleast 16)'(conc2 (atleast 8)'i (atleast 8)'i))))
)

