
;; Run as
;(run-typeresolve-testset "/home/karttu/bream/src/restests/wirmtest.lst.scm" 1)
;; (For example).

;; Edited    Aug 24 2011 by karttu.
;;   Added <equitype-etype-and-args> wrappers around integer arguments
;;   for each toplevel wirm-macro invocations, as to avoid any optimizations
;;   now present in the new experimental code in expwirms.scm
;;

("revbits-test"
 (revbits (<equitype-etype-and-args> 11))
 4'(((expdepth 1)'lambda ((atleast 4)'b)
       4'(conc2 1'(bit (atleast 4)'b (atleast 1)'0)
                3'(((expdepth 2)'lambda (3'b)
                      3'(conc2 1'(bit 3'b (atleast 1)'0)
                               2'(((expdepth 3)'lambda (2'b)
                                     2'(conc2 1'(bit 2'b (atleast 1)'0)
                                              1'(((expdepth 4)'lambda (1'b)
                                                      1'b
                                                 )
                                                 1'(bits 2'b (atleast 1)'1 1)
                                                )
                                       )
                                  )
                                  2'(bits 3'b (atleast 2)'2 1)
                                 )
                        )
                   )
                   3'(bits (atleast 4)'b (atleast 2)'3 1)
                  )
         )
   )
   (atleast 4)'(<equitype-etype-and-args> (atleast 4)'11)
  )
)


("ew-test 1"
  (+ 11 (ew2))
  (atleast 4)'(+ (atleast 4)'11 (atleast 4)'4)
;; Before the optimization in the expander-handler for (lambda):
;;(atleast 4)'(+ (atleast 4)'11 (atleast 4)'(((expdepth 1)'lambda () (atleast 4)'4)))
)


("ew-test 2"
  (+ (ew2) 65537)
  (atleast 17)'(+ (atleast 17)'17 (atleast 17)'65537)
;; Before the optimization in the expander-handler for (lambda):
;;(atleast 17)'(+ (atleast 17)'(((expdepth 1)'lambda () (atleast 17)'17)) (atleast 17)'65537)
)

("inc test 0"
  (inc (<equitype-etype-and-args> 1'0))
  1'(((expdepth 1)'lambda (1'a) 1'(bitnot 1'a))
         1'(<equitype-etype-and-args> 1'0)
    )
)

("inc test 1"
  (inc (<equitype-etype-and-args> 1))
  (atleast 1)'(((expdepth 1)'lambda ((atleast 1)'a)
                   (atleast 1)'(bitnot (atleast 1)'a))
                  (atleast 1)'(<equitype-etype-and-args> (atleast 1)'1)
              )
)

("inc test 2"
  (inc (<equitype-etype-and-args> 2'2))
  2'(((expdepth 1)'lambda (2'a)
         2'((lambda (1'b) 2'(conc2 1'(bitxor 1'(bit 2'a (atleast 1)'1)
                                             1'(rednor 1'b)
                                     )
                                     1'b
                            )
            )
            1'(((expdepth 2)'lambda (1'a) 1'(bitnot 1'a))
               1'(bits 2'a (atleast 1)'0 0)
              )
           )
     )
     2'(<equitype-etype-and-args> 2'2)
    )
)


("inc test 5"
  (inc (<equitype-etype-and-args> 3'5))
  3'(((expdepth 1)'lambda (3'a)
        3'((lambda (2'b) 3'(conc2 1'(bitxor 1'(bit 3'a (atleast 2)'2)
                                            1'(rednor 2'b)
                                    )
                                    2'b
                           )
           )
           2'(((expdepth 2)'lambda (2'a)
                  2'((lambda (1'b) 2'(conc2 1'(bitxor 1'(bit 2'a (atleast 1)'1)
                                                      1'(rednor 1'b)
                                              )
                                              1'b
                                     )
                     )
                     1'(((expdepth 3)'lambda (1'a) 1'(bitnot 1'a))
                        1'(bits 2'a (atleast 1)'0 0)
                       )
                    )
              )
              2'(bits 3'a (atleast 1)'1 0)
             )
          )
     )
     3'(<equitype-etype-and-args> 3'5)
    )
)

("inc test 11"
  (inc (<equitype-etype-and-args> 4'11))
  4'(((expdepth 1)'lambda (4'a)
       4'((lambda (3'b) 4'(conc2 1'(bitxor 1'(bit 4'a (atleast 2)'3)
                                           1'(rednor 3'b)
                                   )
                                   3'b
                          )
          )
          3'(((expdepth 2)'lambda (3'a)
               3'((lambda (2'b) 3'(conc2 1'(bitxor 1'(bit 3'a (atleast 2)'2)
                                                   1'(rednor 2'b)
                                           )
                                           2'b
                                  )
                  )
                  2'(((expdepth 3)'lambda (2'a)
                       2'((lambda (1'b)
                             2'(conc2 1'(bitxor 1'(bit 2'a (atleast 1)'1)
                                                1'(rednor 1'b)
                                        )
                                        1'b
                               )
                          )
                          1'(((expdepth 4)'lambda (1'a) 1'(bitnot 1'a))
                             1'(bits 2'a (atleast 1)'0 0)
                            )
                         )
                     )
                     2'(bits 3'a (atleast 1)'1 0)
                    )
                 )
             )
             3'(bits 4'a (atleast 2)'2 0)
            )
         )
     )
     4'(<equitype-etype-and-args> 4'11)
    )
)

("zxt-test 1"
  (+ 31 (zxt 3))
  #f
)

("zxt-test 3"
  (+ 31 (zxt 2'3))
  #f
)


("sxt-test 1"
  (+ 31 (sxt 3))
  #f
)

("sxt-test 3"
  (+ 31 (sxt 2'3))
  #f
)

("zxt-test 4"
  (+ 31 (zxt-alt 2'3))
  5'(+ 5'31
       5'(((expdepth 1)'lambda (2'a)
               5'(((expdepth 2)'lambda (3'a)
                       5'(((expdepth 3)'lambda (4'a)
                               5'(((expdepth 4)'lambda (5'a) 5'a)
                                  5'(conc2 1'0 4'a)
                                 )
                          )
                          4'(conc2 1'0 3'a)
                         )
                  )
                  3'(conc2 1'0 2'a)
                 )
          )
          2'3
         )
    )
)



("zxt-test 2"
  (+ 31 (zxt-alt 3))
  (atleast 5)'(+ (atleast 5)'31
                 (atleast 5)'(((expdepth 1)'lambda ((atleast 2)'a)
                   (atleast 5)'(((expdepth 2)'lambda ((atleast 3)'a)
                     (atleast 5)'(((expdepth 3)'lambda ((atleast 4)'a)
                       (atleast 5)'(((expdepth 4)'lambda ((atleast 5)'a)
                                           (atleast 5)'a
                                    )
                                    (atleast 5)'(conc2 1'0 (atleast 4)'a)
                                   )
                                  )
                                  (atleast 4)'(conc2 1'0 (atleast 3)'a)
                                 )
                                )
                                (atleast 3)'(conc2 1'0 (atleast 2)'a)
                               )
                              )
                              (atleast 2)'3
                             )
              )
)


("zxt-test 5"
  (+ 5'31 (zxt 3))
  5'(+ 5'31
       5'(((expdepth 1)'lambda (2'a)
            5'(conc2 1'0
                     4'(((expdepth 2)'lambda (2'a)
                          4'(conc2 1'0
                                   3'(((expdepth 3)'lambda (2'a)
                                        3'(conc2 1'0
                                                 2'(((expdepth 4)'lambda (2'a)
                                                      2'a
                                                    )
                                                    2'a
                                                   )
                                          )
                                      )
                                      2'a
                                     )
                            )
                        )
                        2'a
                       )
              )
          )
          2'3
         )
    )
)




("zxt-test 6"
  (+ 5'31 (zxt-alt 3))
  5'(+ 5'31
       5'(((expdepth 1)'lambda (2'a)
               5'(((expdepth 2)'lambda (3'a)
                       5'(((expdepth 3)'lambda (4'a)
                               5'(((expdepth 4)'lambda (5'a) 5'a)
                                  5'(conc2 1'0 4'a)
                                 )
                          )
                          4'(conc2 1'0 3'a)
                         )
                  )
                  3'(conc2 1'0 2'a)
                 )
          )
          2'3
         )
    )
)


("zxt-test 7"
  (+ 5'31 (zxt 2'3))
  5'(+ 5'31
       5'(((expdepth 1)'lambda (2'a)
            5'(conc2 1'0
                     4'(((expdepth 2)'lambda (2'a)
                          4'(conc2 1'0
                                   3'(((expdepth 3)'lambda (2'a)
                                        3'(conc2 1'0
                                                 2'(((expdepth 4)'lambda (2'a)
                                                      2'a
                                                    )
                                                    2'a
                                                   )
                                          )
                                      )
                                      2'a
                                     )
                            )
                        )
                        2'a
                       )
              )
          )
          2'3
         )
    )
)


("zxt-test 8"
  (+ 5'31 (zxt-alt 2'3))
  5'(+ 5'31
       5'(((expdepth 1)'lambda (2'a)
               5'(((expdepth 2)'lambda (3'a)
                       5'(((expdepth 3)'lambda (4'a)
                               5'(((expdepth 4)'lambda (5'a) 5'a)
                                  5'(conc2 1'0 4'a)
                                 )
                          )
                          4'(conc2 1'0 3'a)
                         )
                  )
                  3'(conc2 1'0 2'a)
                 )
          )
          2'3
         )
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


("sxt-test 2"
  (+ (<equitype-etype-and-args> 31) (sxt-alt (<equitype-etype-and-args> 3)))
  (atleast 5)'(+ (atleast 5)'(<equitype-etype-and-args> (atleast 5)'31)
                 (atleast 5)'(((expdepth 1)'lambda ((atleast 2)'a)
                   (atleast 5)'(((expdepth 2)'lambda ((atleast 3)'a)
                     (atleast 5)'(((expdepth 3)'lambda ((atleast 4)'a)
                       (atleast 5)'(((expdepth 4)'lambda ((atleast 5)'a)
                                           (atleast 5)'a
                                    )
                                    (atleast 5)'(conc2 1'(bit (atleast 4)'a
                                                              (atleast 2)'3
                                                         )
                                                         (atleast 4)'a
                                                )
                                   )
                                  )
                                  (atleast 4)'(conc2 1'(bit (atleast 3)'a
                                                            (atleast 2)'2
                                                       )
                                                       (atleast 3)'a
                                              )
                                 )
                                )
                                (atleast 3)'(conc2 1'(bit (atleast 2)'a
                                                          (atleast 1)'1
                                                     )
                                                     (atleast 2)'a
                                            )
                               )
                              )
                              (atleast 2)'(<equitype-etype-and-args>
                                               (atleast 2)'3
                                          )
                             )
              )
)


("sxt-test 4"
  (+ (<equitype-etype-and-args> 31) (sxt-alt (<equitype-etype-and-args> 2'3)))
  5'(+ 5'(<equitype-etype-and-args> 5'31)
       5'(((expdepth 1)'lambda (2'a)
               5'(((expdepth 2)'lambda (3'a)
                       5'(((expdepth 3)'lambda (4'a)
                               5'(((expdepth 4)'lambda (5'a) 5'a)
                                  5'(conc2 1'(bit 4'a (atleast 2)'3) 4'a)
                                 )
                          )
                          4'(conc2 1'(bit 3'a (atleast 2)'2) 3'a)
                         )
                  )
                  3'(conc2 1'(bit 2'a (atleast 1)'1) 2'a)
                 )
          )
          2'(<equitype-etype-and-args> 2'3)
         )
    )
)


("sxt-test 5"
;;(+ 5'31 (sxt 3)) ;; Would generate too many passes without this one?
  (+ (<equitype-etype-and-args> 5'31) (sxt (<equitype-etype-and-args> 3)))
  5'(+ 5'(<equitype-etype-and-args> 5'31)
       5'(((expdepth 1)'lambda (2'a)
            5'(conc2 1'(bit 2'a (atleast 1)'1)
                     4'(((expdepth 2)'lambda (2'a)
                          4'(conc2 1'(bit 2'a (atleast 1)'1)
                                   3'(((expdepth 3)'lambda (2'a)
                                        3'(conc2 1'(bit 2'a (atleast 1)'1)
                                                 2'(((expdepth 4)'lambda (2'a)
                                                      2'a
                                                    )
                                                    2'a
                                                   )
                                          )
                                      )
                                      2'a
                                     )
                            )
                        )
                        2'a
                       )
              )
          )
          2'(<equitype-etype-and-args> 2'3)
         )
    )
)


("sxt-test 6"
  (+ (<equitype-etype-and-args> 5'31) (sxt-alt (<equitype-etype-and-args> 3)))
  5'(+ 5'(<equitype-etype-and-args> 5'31)
       5'(((expdepth 1)'lambda (2'a)
               5'(((expdepth 2)'lambda (3'a)
                       5'(((expdepth 3)'lambda (4'a)
                               5'(((expdepth 4)'lambda (5'a) 5'a)
                                  5'(conc2 1'(bit 4'a (atleast 2)'3) 4'a)
                                 )
                          )
                          4'(conc2 1'(bit 3'a (atleast 2)'2) 3'a)
                         )
                  )
                  3'(conc2 1'(bit 2'a (atleast 1)'1) 2'a)
                 )
          )
          2'(<equitype-etype-and-args> 2'3)
         )
    )
)

("sxt-test 7"
;;(+ 5'31 (sxt 2'3))
  (+ (<equitype-etype-and-args> 5'31) (sxt (<equitype-etype-and-args> 2'3)))
  5'(+ 5'(<equitype-etype-and-args> 5'31)
       5'(((expdepth 1)'lambda (2'a)
            5'(conc2 1'(bit 2'a (atleast 1)'1)
                     4'(((expdepth 2)'lambda (2'a)
                          4'(conc2 1'(bit 2'a (atleast 1)'1)
                                   3'(((expdepth 3)'lambda (2'a)
                                        3'(conc2 1'(bit 2'a (atleast 1)'1)
                                                 2'(((expdepth 4)'lambda (2'a)
                                                      2'a
                                                    )
                                                    2'a
                                                   )
                                          )
                                      )
                                      2'a
                                     )
                            )
                        )
                        2'a
                       )
              )
          )
          2'(<equitype-etype-and-args> 2'3)
         )
    )
)

("sxt-test 8"
;;(+ 5'31 (sxt-alt 2'3))
  (+ (<equitype-etype-and-args> 5'31) (sxt-alt (<equitype-etype-and-args> 2'3)))
  5'(+ 5'(<equitype-etype-and-args> 5'31)
       5'(((expdepth 1)'lambda (2'a)
               5'(((expdepth 2)'lambda (3'a)
                       5'(((expdepth 3)'lambda (4'a)
                               5'(((expdepth 4)'lambda (5'a) 5'a)
                                  5'(conc2 1'(bit 4'a (atleast 2)'3) 4'a)
                                 )
                          )
                          4'(conc2 1'(bit 3'a (atleast 2)'2) 3'a)
                         )
                  )
                  3'(conc2 1'(bit 2'a (atleast 1)'1) 2'a)
                 )
          )
          2'(<equitype-etype-and-args> 2'3)
         )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

