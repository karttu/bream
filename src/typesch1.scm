
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                 ;;
;; BREAM / typesch1.scm                                            ;;
;;   --  Our first scheme for the type system.                     ;;
;;       In principle, typreslv.scm (The type resolver), should    ;;
;;       work to some degree independently of the exact details    ;;
;;       of these types.                                           ;;
;;       Also, srsctrct.scm should refer to these types opaquele.  ;;
;;                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Unless otherwise mentioned, all the files in this code tree are
;; Copyright (C) 2010-2011 Antti Karttunen, subject to the terms of the GPL v2.
;; See the file COPYING for more information.  Contact the author for
;; further developments at the address: <his-firstname>.<his-surname>@gmail.com
;;

;;
;; Antti Karttunen ("karttu") started writing this module at May 26 2011.
;; by transferring a half of srcstrct.scm here.
;;

;;
;; Edited May 25-26 2011 by karttu.
;;   New "regularized" type-scheme with binary masks.
;;
;; Edited    May 29 2011 by karttu.
;;   Some corrections to types-match-with-loops? although
;;   currently typresolv.scm will not effectively set any
;;   looping-bit on for any type. (See comments there.)
;;
;;
;; Edited    Jun 02 2011 by karttu.
;;   Added type-with-dec1-width and type-with-inc1-width.
;;
;;
;; Edited    Jul 20 2011 by karttu.
;;   Added get-lambda-expansion-level and set-lambda-expansion-level
;;   the new accessors for the needs of loop unrolling and recursive macro
;;   expansion (there will be an integer kept in expanded lambda-form's
;;   lambda, with "special"-type.) Also type-checker type-special? added.
;;
;; Edited    Jul 29 2011 by karttu.
;;   Renamed
;;     get-lambda-expansion-level to get-lambda-loop-unrolling-level
;;   and
;;     set-lambda-expansion-level to set-lambda-loop-unrolling-level
;;   Added also predicates lambda-not-special?,
;;   lambda-left-by-loop-unrolling? and lambda-left-by-wirm-expansion?
;;   with which we can tell different kind of (generated) lambda's from
;;   each other.
;;   Also added type-cast-nondefinite for clearing the definiteness-bit.
;;
;; Edited    Jul 30 2011 by karttu.
;;   Added "external representations" for special types
;;   "lambda-unrolling-level" and "lambda-expansion-level",
;;   (unrolled n) and (expdepth n) respectively.
;;   Note that (expdepth 1) is currently output as boolean,
;;   as they are both implemented as types with "special" mask.
;;
;; Edited    Jul 31 2011 by karttu.
;;   Swapped the type-masks for types special (before 00, now 01) and
;;   input (before 01, now 00), so that for example, the typeresolver
;;   for forms like conc2 or bits now work better.
;;   (However, this further invalidated some inactive loop-type code,
;;   and the comments & mask-bit-diagrams below are now out-of-date).
;;

;; Edited    Aug 20 2011 by karttu.
;;   Added type-1bit-output.
;;

;; The new type scheme is like below:
;;
;; bits X-4    bit-3        (bit-2,bit-1)            bit-0
;;  width      0: definite  00: mismatch, boolean    0: no loops
;;                width         or an eternal loop
;;
;;             1: ATLEAST                            1: contains a loop
;;                width                                 somewhere down
;;                                                      a call tree
;;
;;                          01: input signal (i.e. a normal funarg)
;;
;;                          10: output signal
;;
;;                          11: inout signal
;;

;; The lowermost bit (bit-0) tells whether there has been met
;; any tail-recursive loops somewhere down the call tree.
;; E.g. <loop-tail-call> has 00001 as its type, because it
;; definitely loops, and never returns anything. Same with
;; if, whose both branches loop.
;; The next bits, bits 1-2, tell the signal type, i.e.
;; either a normal 01 (input signals to verilog functions, and also
;; regs), 10 (for output signals), 11 (for maybe someday fully implemented
;; inout signals), and 00 for special cases, of which we have:

;; Have to CHANGE this: the default type should be
;; 00010, (i.e. 2), input-sig with indefinite width >= 0,
;; and 00000 (0), i.e. "boolean/special" of indefinite width 0
;; should stand as "unresolvable/type-mismatch" marker.
;; (Or maybe 8, Boolean of definite width 0?)

;;    WDTTL
;; ...00000 = type mismatch, unresolvable (i.e. special of indefinite width>=0)
;; ...00010 = normal input type, of width <=0, i.e. default unresolved
;;
;; ...11000 = boolean (i.e. #t or #f) (Also "Boolean vectors" are possible
;;                                     if width > 1).
;; ...00001 = a certain loop (this is what <loop-tail-call> returns?)
;;
;; ...00011 = loops in one branch, the other branch still unresolved.
;; ...10011 = loops in one branch, other branch inp.signal of width(>=1)
;;            ("backward confluence" of 10010 and 00001 in if?)
;;
;;
;; ...01000 = (Special "Boolean of definite width 0". Reserved for future use.)

;; ...01010 = SPECIAL: input of definite width 0, used in wirm-expansions?
;; ...11010 = input signal of width 1.
;; ...10010 = input signal of nondefinite width>=1 (type-anynonzerowidth-in)

(define type-unresolved-completely 0)
;; (define type-unresolved-input ???) ;; i.e. input signal of width(<=0)
;; (define type-anywidth-integer-in type-unresolved-completely) ;; XXX - ???

(define (type-completely-unresolved? tmt) (eq? type-unresolved-completely tmt))

(define type-default-unresolved type-unresolved-completely)

(define type-certainly-loops 1) ;; Invent a new value for this!
(define type-maskpos-sigmask 2)
(define type-maskpos-definite 8)
(define type-maskpos-width 16)

(define (type-sigmask tm) (modulo (quotient tm type-maskpos-sigmask) 4))

(define type-sigmask-special 1) ;; Was: 0
(define type-sigmask-input   0) ;; Was: 1
(define type-sigmask-output  2)
(define type-sigmask-inout   3)


(define (type-definite-width w)
   (+ type-maskpos-definite (* w type-maskpos-width))
)

(define (type-atleast-width w) (* w type-maskpos-width))

(define (type-s-of-definite-width w)
   (+ (type-definite-width w) (+ (* type-sigmask-special type-maskpos-sigmask)))
)

(define (type-s-of-atleast-width w)
   (+ (type-atleast-width w) (+ (* type-sigmask-special type-maskpos-sigmask)))
)


(define (type-i-of-definite-width w)
   (+ (type-definite-width w) (+ (* type-sigmask-input type-maskpos-sigmask)))
)

(define (type-i-of-atleast-width w)
   (+ (type-atleast-width w) (+ (* type-sigmask-input type-maskpos-sigmask)))
)


(define (type-o-of-definite-width w)
   (+ (type-definite-width w) (+ (* type-sigmask-output type-maskpos-sigmask)))
)

(define (type-o-of-atleast-width w)
   (+ (type-atleast-width w) (+ (* type-sigmask-output type-maskpos-sigmask)))
)


(define (type-io-of-definite-width w)
   (+ (type-definite-width w) (+ (* type-sigmask-inout type-maskpos-sigmask)))
)

(define (type-io-of-atleast-width w)
   (+ (type-atleast-width w) (+ (* type-sigmask-inout type-maskpos-sigmask)))
)



(define (type-special? tmt) (eq? type-sigmask-special (type-sigmask tmt)))
(define (type-input? tmt)   (eq? type-sigmask-input   (type-sigmask tmt)))
(define (type-output? tmt)  (eq? type-sigmask-output  (type-sigmask tmt)))
(define (type-inout? tmt)   (eq? type-sigmask-inout   (type-sigmask tmt)))

(define (type-width-definite? tmt) (odd? (quotient tmt type-maskpos-definite)))
(define (type-width-nondefinite? tmt) (even? (quotient tmt type-maskpos-definite)))

;; (define (type-boolean? tmt)
;;  (and (eq? type-sigmask-special (type-sigmask tmt))
;;       (eq? 1 (type-width-of tmt))
;;       (type-width-definite? tmt)
;;  )
;; )

(define (type-cast-nonlooping tmt) (* 2 (quotient tmt 2)))

(define (type-cast-nondefinite tmt)
   (if (type-width-definite? tmt)
       (- tmt type-maskpos-definite)
       tmt
   )
)


(define (type-cast-definite tmt)
   (if (type-width-nondefinite? tmt)
       (+ tmt type-maskpos-definite)
       tmt
   )
)

(define (type-overwrite-with-definite-width tmt new-width)
   (type-simple-combine
      (modulo tmt type-maskpos-width) ;; Keep type, looping & definitess flags
      (type-definite-width new-width) ;; and or them with new definite width.
   )
)


(define (type-simple-combine t1 t2) (int-or t1 t2))

(define (type-cast-allow-looping tmt)
        (type-simple-combine tmt type-certainly-loops)
)


(define (type-inherit-loopiness-from t1 t2)
    (type-simple-combine t1 (modulo t1 2))
)



(define type-1bit-input (type-i-of-definite-width 1))
(define type-1bit-output (type-o-of-definite-width 1))


(define type-boolean (type-s-of-definite-width 1)) ;Was 24 when specmask was 00

(define type-anynonzerowidth-in (type-i-of-atleast-width 1)) ; Was 18 (old way)


(define (type-is-boolean? tmt) (eq? type-boolean tmt))

(define (type-potentially-looping? tmt) (odd? tmt))

;; Let this return always #f until we rethink the looping-type system
;; and find a good value for constant type-certainly-loops:
(define (type-certainly-looping? tmt) #f) ;; WAS (eq? type-certainly-loops tmt)


;; Note that both ordinary input wires, boolean (input signals/funargs)
;; and one-bit output signals have all width 1:
(define (type-width-of tmt) (quotient tmt type-maskpos-width))

(define get-lambda-loop-unrolling-level type-width-of)
(define get-lambda-wirm-expansion-level type-width-of)

(define (set-lambda-loop-unrolling-level lev) (type-s-of-definite-width lev))
(define (set-lambda-wirm-expansion-level lev) (type-s-of-atleast-width lev))


(define lambda-not-special? type-completely-unresolved?)
(define lambda-left-by-loop-unrolling? type-width-definite?)
(define lambda-left-by-wirm-expansion? type-width-nondefinite?)

(define (type-overwrite-width tmt new-width)
   (+ (modulo tmt type-maskpos-width) ;; Keep type, looping & definitess flags
      (* new-width type-maskpos-width) ;; just change the width
   )
)

;; Shouldn't be applied to zero-widths:
(define (type-with-dec1-width tmt)
   (type-overwrite-width tmt (- (type-width-of tmt) 1))
)

(define (type-with-inc1-width tmt)
   (type-overwrite-width tmt (+ 1 (type-width-of tmt)))
)



(define (untyped s) (type-et-elem type-default-unresolved s)) ;; untyped doesn't mean non-typed or detyped!

;; output [is equivalent to (output atleast 1)]
;; (output 1)
;; (output atleast 3)
;; (inout 3)
;; 5 [is equivalent to (in 5)]
;; (atleast 5) [is equivalent to (in atleast 5)]
;;
;;

(define (type-ext->bin ht)
   (cond ((or (not ht) (null? ht)) type-unresolved-completely)
         ((eq? 'surely-looping ht) type-certainly-loops)
         ((eq? 'boolean ht) type-boolean)
         ((eq? 'output ht) (type-o-of-atleast-width 1))
         ((integer? ht) (type-i-of-definite-width ht))
         ((list? ht)
             (case (first ht)
                 ((looping-or:)
                     (type-cast-allow-looping (type-ext->bin (second ht)))
                 )
                 ((atleast) (type-i-of-atleast-width (second ht)))
                 ((output)
                     (if (eq? 'atleast (second ht))
                         (type-o-of-atleast-width (third ht))
                         (type-o-of-definite-width (second ht))
                     )
                 )
                 ((inout)
                     (if (eq? 'atleast (second ht))
                         (type-io-of-atleast-width (third ht))
                         (type-io-of-definite-width (second ht))
                     )
                 )
                 ((expdepth) (set-lambda-wirm-expansion-level (second ht)))
                 ((unrolled) (set-lambda-loop-unrolling-level (second ht)))
                 (else (format #f "Unknown-type-specifier:~s" ht))
             )
         )
         (else (format #f "Unknown-type-specifier:~s" ht))
   )
)


;; Note that (expdepth 1) is currently output as boolean

(define (type-bin->ext tmt)
   (cond ((not (integer? tmt)) (format #f "Weird:(~s)" tmt))
         ((type-completely-unresolved? tmt) #f)
         ((type-certainly-looping? tmt) 'surely-looping)
         ((type-potentially-looping? tmt)
             (list 'looping-or: (type-bin->ext (type-cast-nonlooping tmt)))
         )
         ((type-is-boolean? tmt) 'boolean)
         ((type-special? tmt) ;; other special. Convention-specific kludges.
             (if (lambda-left-by-wirm-expansion? tmt)
                 (list 'expdepth (type-width-of tmt))
                 (list 'unrolled (type-width-of tmt))
             )

;;           (if (type-width-nondefinite? tmt)
;;               (list 'special 'atleast (type-width-of tmt))
;;               (list 'special (type-width-of tmt))
;;           )
         )
         ((type-input? tmt)
             (if (type-width-nondefinite? tmt)
                 (list 'atleast (type-width-of tmt))
                 (type-width-of tmt)
             )
         )
         ((type-output? tmt)
             (if (type-width-nondefinite? tmt)
                 (list 'output 'atleast (type-width-of tmt))
                 (list 'output (type-width-of tmt))
             )
         )
         ((type-inout? tmt)
             (if (type-width-nondefinite? tmt)
                 (list 'inout 'atleast (type-width-of tmt))
                 (list 'inout (type-width-of tmt))
             )
         )
         (else (format #f "Unknown-type-with-mask:~s" tmt))
   )
)



;; Like types-match? but also allows matching of "certain loops"
;; and "potential loops" against any other type:
;; (Resulting that the looping-bit will be set also in that other type).

(define (types-match-with-loops? t1 t2)
    (cond ((or (type-certainly-looping? t1) (type-certainly-looping? t2))
;; Then just set that "looping" bit in the other type.
                (type-simple-combine t1 t2)
          )
          ((types-match? (type-cast-nonlooping t1) (type-cast-nonlooping t2))
            => (lambda (tm)
                   (type-inherit-loopiness-from
                       (type-inherit-loopiness-from tm t1)
                       t2
                   )
               )
          )
          (else #f)
    )
)


;; Checks whether type-masks t1 and t2 match.
;; If not, returns #f,
;; otherwise returns either t1 or t2, or some refinement
;; of them.

(define (types-match? t1 t2)
    (define TYPES-DO-NOT-MATCH #f)
    (cond ((type-completely-unresolved? t1) t2)
          ((type-completely-unresolved? t2) t1)
          ((not (eq? (type-sigmask t1) (type-sigmask t2))) TYPES-DO-NOT-MATCH)

;; If both are ATLEAST-widths, then check which one is larger,
;; and increment the smaller one accordingly:
          ((and (type-width-nondefinite? t1) (type-width-nondefinite? t2))
              (cond ((< (type-width-of t1) (type-width-of t2))
                            (type-overwrite-width t1 (type-width-of t2))
                    )
                    ((> (type-width-of t1) (type-width-of t2))
                            (type-overwrite-width t2 (type-width-of t1))
                    )
                    (else t1) ;; Equal atleast-widths.
              )
          )

;; If both have definite widths, they must agree:
          ((and (type-width-definite? t1) (type-width-definite? t2))
              (if (eq? (type-width-of t1) (type-width-of t2))
                  t1
                  TYPES-DO-NOT-MATCH
              )
          )

;; If only the other one has definite width, then use it, unless the other
;; one is larger (whether definite or atleast-width):
          ((type-width-definite? t1)
              (if (> (type-width-of t2) (type-width-of t1))
                  TYPES-DO-NOT-MATCH
                  t1
              )
          )

          ((type-width-definite? t2)
              (if (> (type-width-of t1) (type-width-of t2))
                  TYPES-DO-NOT-MATCH
                  t2
              )
          )

          (else (error "types-match? called with type-masks: " t1 t2))
    )
)

