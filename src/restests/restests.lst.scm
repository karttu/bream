
;; Run as
;(run-typeresolve-testset "/home/karttu/bream/src/restests/restests.lst.scm" 1)
;; (For example).

("IF-Test"
 (if (= 32 256) (bitxor 1 4) (if (zero? 3) (+ 2 8) (- 16 2)))
 (atleast 5)'(if boolean'(= (atleast 9)'32 (atleast 9)'256)
                         (atleast 5)'(bitxor (atleast 5)'1 (atleast 5)'4)
                         (atleast 5)'(if boolean'(zero? (atleast 2)'3)
                                         (atleast 5)'(+ (atleast 5)'2
                                                        (atleast 5)'8)
                                         (atleast 5)'(- (atleast 5)'16
                                                        (atleast 5)'2)))
)

("Testing named let, almost gcd."
 (let almost-gcd ((a 1) (18'b 2)) (if (zero? b) a (almost-gcd b (- a b))))

 18'(<let-named> almost-gcd ((18'a 18'1) (18'b 18'2)) 18'(if boolean'(zero? 18'b) 18'a 18'(almost-gcd 18'b 18'(- 18'a 18'b))))

;; What we would actually like sometime in future, is:
;; (looping-or: 18)'(<let-named> almost-gcd ((18'a 18'1) (18'b 18'2)) (looping-or: 18)'(if boolean'(zero? 18'b) 18'a surely-looping'(almost-gcd 18'b 18'(- 18'a 18'b))))

)


("Testing shifting in the loop"
    (let loop ((i 11) (s 12'1))
        (if (zero? i) s (loop (- i 1) (<< s 1)))
    )
    12'(<let-named> loop (((atleast 4)'i (atleast 4)'11) (12's 12'1)) 12'(if boolean'(zero? (atleast 4)'i) 12's 12'(loop (atleast 4)'(- (atleast 4)'i (atleast 4)'1) 12'(<< 12's (atleast 1)'1))))
)

("SIMPLE (+ 0)"
  (+ 0)
  (atleast 1)'(+ (atleast 1)'0)
)


("SIMPLE (+ 0 .. 15)"
  (+ 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
  (atleast 5)'(+ (atleast 5)'0 (atleast 5)'1 (atleast 5)'2 (atleast 5)'3 (atleast 5)'4 (atleast 5)'5 (atleast 5)'6 (atleast 5)'7 (atleast 5)'8 (atleast 5)'9 (atleast 5)'10 (atleast 5)'11 (atleast 5)'12 (atleast 5)'13 (atleast 5)'14 (atleast 5)'15 (atleast 5)'16)
)

;; Some quite uninspired tests:

("SIMPLE bit-operations test"
  (let ((a 1) (b (bitnot 2)) (c 4) (d 0) (e 63))
    (bitor (bitxor a (bitand b (bitnot c)))
           (bitxnor d e)
    )
  )
 (atleast 6)'((lambda ((atleast 6)'a (atleast 6)'b (atleast 6)'c (atleast 6)'d (atleast 6)'e) (atleast 6)'(bitor (atleast 6)'(bitxor (atleast 6)'a (atleast 6)'(bitand (atleast 6)'b (atleast 6)'(bitnot (atleast 6)'c))) (atleast 6)'(bitxnor (atleast 6)'d (atleast 6)'e))) (atleast 6)'1 (atleast 6)'(bitnot (atleast 6)'2) (atleast 6)'4 (atleast 6)'0 (atleast 6)'63)
)


("Testing let"
 (let ((a 1) (b (bitnot 2)) (c 4) (d 0))
    (if (= a (bitnot c))
        (bitand a b)
        (- d)
    )
 )
 (atleast 3)'((lambda ((atleast 3)'a (atleast 3)'b (atleast 3)'c (atleast 3)'d) (atleast 3)'(if boolean'(= (atleast 3)'a (atleast 3)'(bitnot (atleast 3)'c)) (atleast 3)'(bitand (atleast 3)'a (atleast 3)'b) (atleast 3)'(- (atleast 3)'d))) (atleast 3)'1 (atleast 3)'(bitnot (atleast 3)'2) (atleast 3)'4 (atleast 3)'0)
)


("This should fail, because of type mismatch: expected type 1 doesn't match with type of src (atleast 2): (atleast 2)'(rednor (atleast 1)'(- (atleast 1)'d))"
 (let ((a 1) (b (bitnot 2)) (c 4) (d 0))
    (if (= a (bitnot c))
        (bitand a b)
        (rednor (- d))
    )
 )
 #f
)

("Some testing with <boolean-etype-and-equitype-args>, takes 3 (= 2+1) passes to resolve."
    (!= 0 1 2 5 9)
    boolean'(!= (atleast 4)'0 (atleast 4)'1 (atleast 4)'2 (atleast 4)'5 (atleast 4)'9)
)


("More inspiring, let* ladder-test. Is type-resolved in 13 (12+1) passes."
 (let* ((a 0)
        (b a)
        (c b)
        (d c)
        (e d)
        (f e)
        (g f)
        (h g)
        (i h)
        (j i)
       )
    (+ j 2'3) ;; Forces eventually all j, i, h, g, f, e, up to 'a' to
;;               definite width 2, up the ladder of nested lambda's
;;               the let* will be expanded to.
 )
 2'((lambda (2'a) 2'((lambda (2'b) 2'((lambda (2'c) 2'((lambda (2'd) 2'((lambda (2'e) 2'((lambda (2'f) 2'((lambda (2'g) 2'((lambda (2'h) 2'((lambda (2'i) 2'((lambda (2'j) 2'(+ 2'j 2'3)) 2'i)) 2'h)) 2'g)) 2'f)) 2'e)) 2'd)) 2'c)) 2'b)) 2'a)) 2'0)
)


("let loop-test. Is type-resolved in 3 passes."
 (let loop ((a 0) (b 0) (c 0) (d 0) (e 0) (f 0) (g 0) (h 0) (i 0) (j 0))
      (loop 2'3 a b c d e f g h i)
 )
 (<let-named> loop ((2'a 2'0) (2'b 2'0) (2'c 2'0) (2'd 2'0) (2'e 2'0) (2'f 2'0) (2'g 2'0) (2'h 2'0) (2'i 2'0) (2'j 2'0)) (loop 2'3 2'a 2'b 2'c 2'd 2'e 2'f 2'g 2'h 2'i))
)


("let loop-test. Is type-resolved in 12 passes."
 (let loop ((a 0) (b 0) (c 0) (d 0) (e 0) (f 0) (g 0) (h 0) (i 0) (j 0))
      (loop b c d e f g h i j 2'3)
 )
 (<let-named> loop ((2'a 2'0) (2'b 2'0) (2'c 2'0) (2'd 2'0) (2'e 2'0) (2'f 2'0) (2'g 2'0) (2'h 2'0) (2'i 2'0) (2'j 2'0)) (loop 2'b 2'c 2'd 2'e 2'f 2'g 2'h 2'i 2'j 2'3))
)


("let-test, with <boolean-etype-and-equitype-args> cascaded. Type-resolved in 13 passes. Starting inferring forward from 'a', each variable's resolving takes one pass, plus some extra."
 (let ((a 0) (b 0) (c 0) (d 0) (e 0) (f 0) (g 0) (h 0) (i 0) (j 0))
   (logand (< 0 a 2'3)
           (< 0 b a)
           (< 0 c b)
           (< 0 d c)
           (< 0 e d)
           (< 0 f e)
           (< 0 g f)
           (< 0 h g)
           (< 0 i h)
           (< 0 j i)
   )
 )
 boolean'((lambda (2'a 2'b 2'c 2'd 2'e 2'f 2'g 2'h 2'i 2'j) boolean'(logand boolean'(< 2'0 2'a 2'3) boolean'(< 2'0 2'b 2'a) boolean'(< 2'0 2'c 2'b) boolean'(< 2'0 2'd 2'c) boolean'(< 2'0 2'e 2'd) boolean'(< 2'0 2'f 2'e) boolean'(< 2'0 2'g 2'f) boolean'(< 2'0 2'h 2'g) boolean'(< 2'0 2'i 2'h) boolean'(< 2'0 2'j 2'i))) 2'0 2'0 2'0 2'0 2'0 2'0 2'0 2'0 2'0 2'0)
)


("let-test, with <boolean-etype-and-equitype-args> cascaded. Type-resolved in 22 passes. Starting inferring backward, from 'j', each variable's resolving taking two passes."
 (let ((a 0) (b 0) (c 0) (d 0) (e 0) (f 0) (g 0) (h 0) (i 0) (j 0))
   (logand (< 0 a b)
           (< 0 b c)
           (< 0 c d)
           (< 0 d e)
           (< 0 e f)
           (< 0 f g)
           (< 0 g h)
           (< 0 h i)
           (< 0 i j)
           (< 0 j 2'3)
   )
 )
 boolean'((lambda (2'a 2'b 2'c 2'd 2'e 2'f 2'g 2'h 2'i 2'j) boolean'(logand boolean'(< 2'0 2'a 2'b) boolean'(< 2'0 2'b 2'c) boolean'(< 2'0 2'c 2'd) boolean'(< 2'0 2'd 2'e) boolean'(< 2'0 2'e 2'f) boolean'(< 2'0 2'f 2'g) boolean'(< 2'0 2'g 2'h) boolean'(< 2'0 2'h 2'i) boolean'(< 2'0 2'i 2'j) boolean'(< 2'0 2'j 2'3))) 2'0 2'0 2'0 2'0 2'0 2'0 2'0 2'0 2'0 2'0)
)



("Some unresolvable circularity, expecting TOO MANY TYPE RESOLVING PASSES."
 (let* ((a 0)
        (b a)
        (c (conc a b))
        (d (conc c b))
       )
    (+ c d)
 )
 #f
;; (atleast 23)'((lambda ((atleast 1)'a) (atleast 23)'((lambda ((atleast 1)'b) (atleast 23)'((lambda ((atleast 23)'c) (atleast 23)'((lambda ((atleast 23)'d) (atleast 23)'(+ (atleast 23)'c (atleast 23)'d)) (atleast 23)'(conc2 (atleast 22)'c (atleast 1)'b))) (atleast 22)'(conc2 (atleast 1)'a (atleast 1)'b))) (atleast 1)'a)) (atleast 1)'0)
)


("conc with Fibonacci numbers, takes just two passes, because no backward inferences."
 (let* ((a 0)
        (b a)
        (c (conc b a))
        (d (conc c b))
        (e (conc d c))
        (f (conc e d))
        (g (conc f e))
        (h (conc g f))
        (i (conc h g))
        (j (conc i h))
       )
    (+ j 0)
 )
(atleast 55)'((lambda ((atleast 1)'a) (atleast 55)'((lambda ((atleast 1)'b) (atleast 55)'((lambda ((atleast 2)'c) (atleast 55)'((lambda ((atleast 3)'d) (atleast 55)'((lambda ((atleast 5)'e) (atleast 55)'((lambda ((atleast 8)'f) (atleast 55)'((lambda ((atleast 13)'g) (atleast 55)'((lambda ((atleast 21)'h) (atleast 55)'((lambda ((atleast 34)'i) (atleast 55)'((lambda ((atleast 55)'j) (atleast 55)'(+ (atleast 55)'j (atleast 55)'0)) (atleast 55)'(conc2 (atleast 34)'i (atleast 21)'h))) (atleast 34)'(conc2 (atleast 21)'h (atleast 13)'g))) (atleast 21)'(conc2 (atleast 13)'g (atleast 8)'f))) (atleast 13)'(conc2 (atleast 8)'f (atleast 5)'e))) (atleast 8)'(conc2 (atleast 5)'e (atleast 3)'d))) (atleast 5)'(conc2 (atleast 3)'d (atleast 2)'c))) (atleast 3)'(conc2 (atleast 2)'c (atleast 1)'b))) (atleast 2)'(conc2 (atleast 1)'b (atleast 1)'a))) (atleast 1)'a)) (atleast 1)'0)
)



("conc with Fibonacci numbers, takes 19 passes, with all variables eventually inferred to definite widths, after we start unraveling the recursive relation backwards, with j's width first set to definite value 55, and then forcing also i to definite width 55-21 = 34."
 (let* ((a 0)
        (b a)
        (c (conc b a))
        (d (conc c b))
        (e (conc d c))
        (f (conc e d))
        (g (conc f e))
        (h (conc g f)) ;; g and h known from below, we can infer f
        (i (conc h g)) ;; i and h known from below, we can infer g
        (j (conc i h)) ;; j definite 544, and also i set by drop, we can infer h.
;;                        (this happens on PASS 4).
;; The propagations upward take two passes each.
       )
    (- i (drop (+ j 55'0) 21))
 )
 34'((lambda (1'a) 34'((lambda (1'b) 34'((lambda (2'c) 34'((lambda (3'd) 34'((lambda (5'e) 34'((lambda (8'f) 34'((lambda (13'g) 34'((lambda (21'h) 34'((lambda (34'i) 34'((lambda (55'j) 34'(- 34'i 34'(drop 55'(+ 55'j 55'0) 21))) 55'(conc2 34'i 21'h))) 34'(conc2 21'h 13'g))) 21'(conc2 13'g 8'f))) 13'(conc2 8'f 5'e))) 8'(conc2 5'e 3'd))) 5'(conc2 3'd 2'c))) 3'(conc2 2'c 1'b))) 2'(conc2 1'b 1'a))) 1'a)) 1'0)
)



("Almost same as above, but the eventual widths are doubled."
 (let* ((a 0)
        (b a)
        (c (conc b a))
        (d (conc c b))
        (e (conc d c))
        (f (conc e d))
        (g (conc f e))
        (h (conc g f))
        (i (conc h g))
        (j (conc i h))
       )
    (- i (drop (+ j 110'0) 42))
 )
 68'((lambda (2'a) 68'((lambda (2'b) 68'((lambda (4'c) 68'((lambda (6'd) 68'((lambda (10'e) 68'((lambda (16'f) 68'((lambda (26'g) 68'((lambda (42'h) 68'((lambda (68'i) 68'((lambda (110'j) 68'(- 68'i 68'(drop 110'(+ 110'j 110'0) 42))) 110'(conc2 68'i 42'h))) 68'(conc2 42'h 26'g))) 42'(conc2 26'g 16'f))) 26'(conc2 16'f 10'e))) 16'(conc2 10'e 6'd))) 10'(conc2 6'd 4'c))) 6'(conc2 4'c 2'b))) 4'(conc2 2'b 2'a))) 2'a)) 2'0)
)


("Same as above, but with crucial -1 difference to the other constant, produces, after 10 passes, a TYPE MISMATCH: expected type 13 doesn't sum to the widths of types 15 and (atleast 3) of src: 13'(conc2 15'e (atleast 3)'d)"
 (let* ((a 0)
        (b a)
        (c (conc b a))
        (d (conc c b))
        (e (conc d c))
        (f (conc e d))
        (g (conc f e))
        (h (conc g f))
        (i (conc h g))
        (j (conc i h))
       )
    (- i (drop (+ j 110'0) 41))
 )
 #f
)

