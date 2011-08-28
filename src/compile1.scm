
;;
;; Bream project - Compiling/Translating a subset of Scheme-language
;;                 to a synthesizable Verilog-code.
;; Copyright (C) 2010-2011 by Antti Karttunen.
;;

;;
;; Unless otherwise mentioned, all the files in this code tree are
;; Copyright (C) 2010-2011 Antti Karttunen, subject to the terms of the GPL v2.
;; See the file COPYING for more information.  Contact the author for further
;; developments at the address <his-firstname>.<his-surname>@gmail.com
;;

;;
;; bream/compile1.scm - module which so far contains almost all the code,
;;                      to be organized, separated and cleansed later.
;;

;;
;; Antti Karttunen ("karttu") started writing this module Aug 10 2010.
;;
;; Edited    Aug 25 2010 by karttu.
;;   Major rewrite from loop2fsm.scm.
;;   Uses now structures comsig and coc.
;;
;; Edited    Aug 26 2010 by karttu.
;;   compile-function-call now tries to form the _ready and _startable
;;    signals as they should. (needs to be cleaned and improved!)
;;    Also a rudimentary version of the instantiation bindings
;;    for non-combinational functions.
;;
;; Edited Aug 27-28 2010 by karttu.
;;    seq-if, etc.
;;
;; Edited    Sep 02 2010 by karttu.
;;    Compiling of loops (compile-named-let) is now starting to get some shape.
;;
;;
;; Edited    Sep 04 2010 by karttu.
;;    Compiling of loops getting further meat around its bones.
;;    Compiled form seq-if first version.
;;
;; Edited    Sep 05 2010 by karttu.
;;    Macros define-rewritten-form and define-compiled-form
;;    which makes adding of new functionality to compiler
;;    more flexible. (That is, one doesn't anymore need to poke
;;    stuff into compile function itself.)
;;    Also, added the first rudimentary code for separate calling conventions
;;    and levels.
;;
;;    Ideally, we should always (regardless of the version of Bream system)
;;    get exactly the same target code out
;;    from the same input code, whenever the calling conventions
;;    and few other parameters like macro-expansion-style, etc. (ideally,
;;    these should be orthogonal vis-a-vis each other) are set
;;    to same values at the beginning of source file.
;;    At least we need to store all utility functions required
;;    by compiling forms to a separate file, and if some of them
;;    are changed in the future, diverge to a separate file,
;;    and those kind of source files should be loaded into
;;    an environment associated with the calling convention
;;    and other settings.
;;
;;    However, to be really perfect, we would probably need our
;;    own Scheme-interpreter, which we knew would work identically
;;    on every platform.
;;
;; Edited    Sep 06 2010 by karttu.
;;    compile-fundef starting to get some more shape.
;;    Still a lots to do. And a lots of dirty code still around.
;;
;; Edited Sep 10-11 2010 by karttu.
;;    Added coc-pop-whole-list-of-extras!
;;    Realized that many parts require extensive rewriting.
;;    Logic in <generic-funcall> is probably leaky.
;;
;; Edited    Sep 12 2010 by karttu.
;;    Added level0code->verilog to test-aux, to see some real Verilog-code
;;    the first time. Corrected a few bugs, numerous still abound.
;;
;; Edited    Sep 15 2010 by karttu.
;;   Replaced all "V_" with ":_".
;;
;; Edited    Sep 18 2010 by karttu.
;;   Moved all rewritten forms (cond, let, or, etc.) to their own
;;   module expsynta.scm. Generalized the dispatching functionality
;;   (See the new module dispatch.scm), and changed the argument
;;   order of many functions from (coc named src) to (src coc named).
;;
;;   Commented out ready_found_code and startable slots of comsig-structure,
;;   as now the _startable and _started signals are explicitly pushed
;;   into wiredefs by the compiler for <noncombinational-call>
;;   This makes the code somewhat cleaner (eh?).
;;
;;   Should readyexpr and uses_ready_wire still be kept as a part
;;   of comsig structure? (Probably, for a moment).
;;
;; Edited    Sep 19 2010 by karttu.
;;   Function arguments now handled somewhat better.
;;   Also, produced code cleansed a lot from unnecessary wires,
;;   including literals (i.e. integers), and constant-valued (1)
;;   ready-wires.
;;
;;   Added the compiled form for and2, with
;;   some to-be-cleansed experimentation with the new function
;;   drop-comsig-and-returns-its-combcode, which allows creation
;;   of kind of "virtual wires", which are popped out of
;;   existence almost as quickly as they were created
;;   (and only their combcode remains.)
;;
;; Edited    Sep 22 2010 by karttu.
;;   More stuff to add-func-args-to-sigdefs. Now allows an optional
;;   with specifier :width after any formal argument name
;;   in the function definition. Not clean yet,
;;   and have to fix also coc-defined-functions-argnames later.
;;
;;
;;
;;
;; Edited    Oct 10 2010 by karttu.
;;    Transferred read-source-file! to a new module readdefs.scm
;;    (with some minor changes).
;;
;;
;; Edited Oct 28-29 2010 by karttu.
;;    Renamed find-comsigname to form-wirename
;;    and coc-defined-functions-argnames to coc-defined-functions-fargs
;;
;;    In general, changed the code to reflect the usage of
;;    type-annotated src-expressions, and indeed many of the
;;    tests seem to work, at least superficially.
;;    However, the accessors expr-*
;;    defined in srcstrct.scm need to be rewritten, as currently
;;    the code is extremely brittle and ugly.
;;
;;
;; Edited    Nov 04 2010 by karttu.
;;    Accessors expr-* renamed to t-* and accessors dt-* renamed to t-u-*
;;    All cc stuff renamed coc (= compiling context, not current continuation!)
;;
;; Edited    Nov 20 2010 by karttu.
;;    Cleaned (refactored) some dispatching-code and moved it to dispatch.scm
;;    Now loads also typreslv.scm.
;;
;; Edited    Nov 21 2010 by karttu.
;;    Function coc-defined-functions-fargs changed slightly, reflecting
;;    the changed funsrc-info structure in readdefs.scm
;;    Changed let-named to <let-named>.
;;    Renamed coc-find-defined-function-by-its-name
;;    as coc-find-funinfo-struct-by-funname
;;
;; Edited    May 15 2011 by karttu.
;;   Back to the work, after six months!
;;   Transferred tail-recursion conversion from compile-fundef to expsynta.scm
;;   Added new elements already-compiled-items and to-be-compiled-items
;;   to coc. Function level0code-for-noncomb-fun changed.
;;
;; Edited    May 16 2011 by karttu.
;;   Moved coc-find-funinfo-struct-by-funname (and two associated functions)
;;   to typreslv.scm and renamed it as trc-find-funinfo-struct-by-funname
;;
;; Edited    May 19 2011 by karttu.
;;   Transferred elements already-compiled-items and to-be-compiled-items
;;   together with warning-printer, error-printer and several other
;;   "more toplevel slots" to a new "more top-level structure"
;;   called toc.
;;
;; Edited    May 20 2011 by karttu.
;;   Renamed add-func-args-to-sigdefs as coc-add-func-args-to-sigdefs
;;   and removed its third argument.
;;
;; Edited    May 22 2011 by karttu.
;;   Moves symbol-with-num and list-to-string to utilits1.scm
;;
;; Edited    May 29 2011 by karttu.
;;   Changed all references to 'conbits' to 'conc' and 'conc2'.
;;
;; Edited    Jun 01 2011 by karttu.
;;   Some minor changes to compile-fundef-to-level0-code, etc.
;;   Now works with the type-resolver.
;;
;; Edited    Jun 02 2011 by karttu.
;;   result_rdy needs to be result_ready in instantation. Corrected.
;;   Added (:_blockcomment (module-info-text (funinfo-moi fis)))
;;   to function compilation. Also, in instantatiation, declare-extfun's
;;   are also checked.
;;
;;   Later that day:
;;   Added rudimentary compiling implementation for \\ just for testing
;;   some ideas. I'm still not sure at what level it should be handled.
;;   Rewriting compile-and-bind-named-exprs-in-emode with a couple of
;;   new kludges.
;;
;; Edited    Jul 22 2011 by karttu.
;;   Added a small check into level0code-for-noncomb-fun so that
;;   it doesn't try to compile any define-wirm definitions,
;;   if accidentally left into code by type-resolver / wirm expander.
;;   (Which should'nt normally happen unless there is a bug somewhere).
;;
;;
;; Edited    Aug 11 2011 by karttu.
;;   \\ renamed to drop. So (\\ x n) is now (drop x n)
;;

;; Edited    Aug 20 2011 by karttu.
;;   Removed toc-compile-toplevel-call & compile-toplevel-call-to-level0-code,
;;   because compile-fundef-to-level0-code can now handle toplevel-calls
;;   when they have been wrapped into a special <define-toplevel-call> wrapper.
;;

;; Edited    Aug 21 2011 by karttu.
;;   Added element reginit to comsig. This is for making sure that
;;   loop state register is initialized to st_loop_ready in the
;;   produced code.
;;   Removed the reference to
;;   *XXX-BREAM-DEFAULT-WIDTH-UNTIL-RESOLVING-FULLY-IMPLEMENTED*
;;   instead, now comsig-width-checked now checks for unresolved signals.
;;   seq-if now takes heed of (coc-startsignal coc) in cases where
;;   the test-expression is combinational. Need to polish it more.
;;

;; Edited    Aug 24 2011 by karttu.
;;   Added second startsignal to coc-structure, so now it has elements
;;   startsignal1 and startsignal2 instead of just startsignal.
;;   The second one (usually assigned to ST_LOOP_WAITING state) is used
;;   for ensuring that starting of non-immediate calls with purely
;;   combinational starting conditions are not started prematurely.
;;   Removed the Aug-21 kludges from seq-if.

;; Edited    Aug 26 2011 by karttu.
;;   Made sure that in level0code-for-noncomb-fun
;;   the startsignal2 is not printed out in Verilog code as #f,
;;   in case it is missing. (E.g. when compiling pseudocombinational functions)
;;   Added *c and *c2 to a list of combinational functions.
;;

(load-option 'format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define *compiled-forms* (list (list '(regular 0.1 draft) (list (list (list))))))

(define (coc-list-of-compiled-forms-in-use coc)
  (list-of-forms-by-dic (coc-calling-convention-in-use coc)
                        (coc-compiled-forms coc)
  )
)

(define-syntax define-compiled-form
   (syntax-rules ()
     ((define-compiled-form (CACO formname src coc named) body ...)
        (attach! (cons (quote formname) (lambda (src coc named) body ...))
                 (list-of-forms-by-dic (quote CACO) *compiled-forms*)
        )
     )
   )
)


(define-syntax define-compiled-form-with-final-branch-decorator
   (syntax-rules ()
     ((define-compiled-form-with-final-branch-decorator (CACO formname src coc named) body ...)
        (attach! (cons (quote formname)
                       (lambda (src2 coc2 named2)
                               (final-branch-decorator
                                       src2
                                       coc2
                                       named2
                                       (lambda (src coc named) body ...)
                               )
                       )
                 )
                 (list-of-forms-by-dic (quote CACO) *compiled-forms*)
        )
     )
   )
)

(define (final-branch-decorator src coc name orig-fun-def)

  (define (compile-iterative-form-final-branch coc wirename-or-expr)
    (expr-wait-wrapped 
         (get-readyexpr wirename-or-expr coc)
         `(:_n== ,(loop-resultreg-name (coc-innermost-loop coc))
                          ,wirename-or-expr
                 ,(loop-state-name (coc-innermost-loop coc)) ST_LOOP_READY
          )
    )
  )


    (if (coc-imod coc)
;; If in iterative mode (inside loop), it means that this expression
;; should be returned as a value of the whole loop, if the tests match:

        (let* ((branchname (coc-form-branch-label coc)) 
               (final-branch-wname (orig-fun-def src coc branchname))
              )
           (compile-iterative-form-final-branch coc final-branch-wname)
        )

;; otherwise, in expressive mode, just compile it, and return the name of (comsig) wire created:
        (orig-fun-def src coc name)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (resolve-arg-widths fun_with_args) ;; XXX - not used, remove!
   (map (lambda (x) 16) ;; For now we just return a constant 16 for all args.
        (cdr fun_with_args)
   )
)


;; XXX - TODO: (bitor a) now produces (| a) in Verilog-code,
;; which is interpreted same as (redor a), which is NOT  was
;; intended, if a's width > 1. Probably we cannot allow Verilog-versions
;; of these function names (&, |, ^, ^~, ~^) as they are ambiguous
;; in unary use. Also, forms like (bitor a), (bitand a), (bitxor a),
;; (bitxnor a) should be expanded simply as a in Verilog.
;; This should probably handled by lev0veri.scm
;;
;; Also, bits does not allow literal arguments, as forms like
;; (72 [6:1]) are illegal in Verilog.
;; (We have to create a separate wire for 72).

(define *combinational_functions*
        '(
;; Literal indetifiers:
          b ;; Binary
          d ;; Decimal
          h ;; Hexadecimal
          o ;; Octal

;; Arithmetic:
          + ;; Add or Unary plus.
          +w ;; Add or Unary plus with wrap-arounf.
          +c ;; Add or Unary plus with space for carry-bit reserved.
          - ;; Subtract or Unary minus.
          * ;; Multiply.
          *c ;; Mult with full space reserved for product. (XXX - Better name?)
          *c2 ;; Strictly two argument variant of *c.

;; Logical: (Should these be equivalent to Scheme not, and & or?)
          lognot ;; In Verilog: ! ;; Logical negation
          logand ;; In Verilog: && ;; Logical and
          logor  ;; In Verilog: || ;; (string->symbol "||") ;; Logical or.

;; Relational:
          > ;; Greater than
          < ;; Less than
          >= ;; Greater than or equal.
          <= ;; Less than or equal.

;; Equality:
          =  ;; Equality as well. In Bream.
          == ;; Equality. In Verilog. Which one to use?
          != ;; Inequality. (What do === and !== mean?)

;; Bit-wise
          bitnot ~      ;; Bitwise-negation (complement). Unary
;; For these arity >= 2, as all are associative and commutative.
;; The operands should all be of the same width:
          bitand &      ;; Bitwise-and
          bitor         ;; "|" ;; (string->symbol "|") ;; Bitwise-or
          bitxor  ^     ;; Bitwise-xor
          bitxnor ^~ ~^ ;; Bitwise-xnor, i.e. Bitwise-equal.

;; Reduction: (unary!)
          redand  ;; & ;; Reduction and
          rednand ;; ~& ;; Reduction nand
          redor   ;; |  ;; Reduction or
          rednor  ;; ~| ;; Reduction nor
          redxor  ;; ^  ;; Reduction xor
          redxnor ;; ^~ or ~^ ;; Reduction xnor

;; Shifts:
          shr >>  ;; >> Right shift.
          shl <<  ;; << Left shift.
          asr >>> ;; >>> Arithmetic shift right.
          asl <<< ;; <<< Arithmetic shift left.

;; Conditional:
          ?:      ;;

;; Concatenation:
          conc ;; { } (cat in Verilisp) One or more arguments.
          conc2

;; Subrange of bits:
          bits ;; ternary: (bits binstr high_inclusive low_inclusive)
;; both high_ and low_inclusive must resolve to integers at compile-time.

;; One bit:
          bit ;; Dyadic. The second arg does not need to be constant in Verilog

;; Reference a vector.
          ref ;; Dyadic. The second arg does not need to be constant in Verilog
;; Note: both (bit x n) and (ref x n) compile to Verilog-form x[n].

;; My own additions:
          zero?    ;; Alias for rednor, "~|" or for (0==x)
          0==      ;;
          nonzero? ;; Alias for redor, "|" or for (0!=x)
          0!=      ;;
          even?    ;; Alias for ~(1&x) or (0==(bit x 0))
          odd?     ;; Alias for (1&x)

          1+      ;; Alias for 1+
          -1+     ;; Alias for -1

          pow2?   ;; How to define niftily, combinationally?
;; This should do it:
;;   (define (pow2? x) (logand (redor x) (rednor (bitand x (-1+ x)))))

          drop ;; From Handel-C, where (\\ x n) = (bits x (-1+ (WIDTH x)) n)
;;                (We use a name that has less potential for trouble.)
;;                   so it's not exactly same as (>> x n). Used in recursive
;;                   macros. We should have WIDTH-1 for idiom (-1+ (WIDTH ..))

          SXT     ;; Sign eXTend
          ZXT     ;; Zero eXTend

          W       ;; WIDTH.  Just for testing now, these are here.
          W-1     ;; (and WIDTH minus one)

         )
)


(define *map_to_usable_names*
        '(
          (+ . "plus")
          (+w . "plusw")
          (+c . "plusc")
          (- . "minus")
          (* . "mul")
          (*c . "mulc")
          (*c2 . "mulc2")
          (/ . "quotient")
          (% . "remainder")
          (! . "lognot")
          (&& . "logand")
          (> . "gt")
          (< . "lt")
          (>= . "gte")
          (<= . "lte")
          (== . "eq")
          (!= . "neq")
          (~ . "bitnot")
          (^ . "bitxor")
          (^~ . "bitxnor")
          (~^ . "bitxnor")
          (>> . "shr")
          (<< . "shl")
          (>>> . "asr")
          (<<< . "asl")
          (?: . "cif") ;; As "combinational if", or something like that.
          (zero? . "zerop")
          (nonzero? . "nonzerop")
          (0== . "zerop")
          (0!= . "nonzerop")
          (even? . "evenp")
          (odd?  . "oddp")
          (1+  . "add1")
          (-1+ . "sub1")
          (\\ . "drop")
          (w-1 . "wminusone")
          (w+1 . "wplusone")
          (w/2 . "wpertwo")
         )
)


;; Note that #t and #f reduce to 1 and 0 when simulating Asenne/MuySimple in
;; Scheme-environment. ???

(define (combinational? funname) (memq funname *combinational_functions*))

;; Note: combinational functions need neither clock nor startsignal,
;; nor do they generate ready-signals.
;; Their result is ready before the next clock edge,
;; and automatically reflects the value of their inputs at the beginning
;; of the current cycle.

;; For all purely combinational functions, the associated _rdy signal is set to
;; constant 1. (Which might get in turn optimized away by Verilog compiler).




(define Emod #f) ;; Emod for "Expressive mode", different from "Iterative mode"
(define Imod #t) ;; Imod for "Iterative mode"


;; Use generate-uninterned-symbol

;; (format #t "Huikkaa ~a lihaa\n" (generate-uninterned-symbol "mursu"))
;; --> Huikkaa #[uninterned-symbol 18 mursu5] lihaa
;;
;; (format #t "Hai: ~a\n" (symbol->string (generate-uninterned-symbol "mursu")))
;; --> Hai: mursu6

(define (interned? s) (intern-soft (symbol->string s)))

(define (generate-interned-symbol s) (symbol-append (generate-uninterned-symbol s)))


;;
;; In general, anything that looks like this:
;;
;;  (foo sig1 sig2 ... sign)
;;
;; produces a "data signal" (or "data bus" or "axon"?),
;; with a unique name formed from "foo".
;; "comsig" = a "computable signal" or "computed signal" ?
;; "compunit" = a "computation unit"? (with its output wire(s) ?)
;;
;; sig1, sig2, ..., sign have been previously reduced to
;; similar "comsigs".
;;
;; (let ((varname init)) ..) ((lambda (varname) ...) init)
;; etc. are just ways to control the naming of comsigs.
;; (i.e. the name is generated from varname, not from the
;;  function/special form init that is invoked to compute it.)
;; Also, Bream-code in the body of these forms can refer to such
;; "named comsigs" with the name given, thus allowing fan-out
;; of the said comsig.
;;
;;


;; signal-widths propagate from leaves towards the root,
;; (and usually _ready signals as well?)
;; while such auxiliary signals as _startable
;; might depend also on the surrounding forms, i.e. there
;; the information may flow either way, from the
;; leaves towards the root, as in case of (foo (bal1 xxx) ...)
;;  wire  foo_startable = bal1_started&bal1_rdy&
;; but also often from the surrounding form towards the leaves,
;; as in
;;  wire leaf_call_startable = whole_function_blunt_start;
;;    or  seq_left_side_rdy, tms.
;;

;; Which function creates the sequential code for tail-recursive functions?
;; (the definition for sequential ifs & looping constructs itself?)
;; Make this Forth-way! (Chuck Guruchef: The Forth way!)

(define-structure (comsig keyword-constructor)
    wirename  ;; The unique wire name of the comsig.
;;               Should be a valid indentifier for Verilog.

    width     ;; Its width. #f if still unresolved.

    sigtype   ;; 'lit 'reg or 'wire ;; Compulsory!

;; XXX - Delete:
;;  argsigs   ;; List giving the comsigs (structures) needed as arguments
              ;; for combcode or noncombcode.
              ;; Compulsory! Should be '() if no argsigs involved.

    source    ;; Bream source code from which this was compiled. For debugging!

    combcode  ;; Combinational Level0-code for the comsig.
;; For combinational functions, we usually generate
;; Verilog-statement: wire wirename = (Level0code->Verilog combcode);
;;               Leave as #f if no combinational code is generated for this.

    reginit   ;; initialization form for registers, #f if not used.

    noncombcode ;;
;; E.g. if it's an ordinary call of non-combinational function,
;;  e.g. (foo arg_a_sig arg_b_sig) we will have:
;;
;;            (foo_invocation
;;              (start (bitand name_startable (bitnot name_started)))
;;              (farg_a arg_a_sig)
;;              (farg_b arg_b_sig)
;;              (result wirename)
;;              (result_ready readysig)
;;            )
;; Otherwise #f.


    readyexpr ;; Combinational function (bitand ...) assigned to name_rdy wire.
              ;;
              ;; This is 1 if comsig is always ready,
              ;; e.g. when fun is combinational function.
              ;;
              ;; It is <wirename>_ready for non-combinational leaf-calls,
              ;; and (bitand <wirename>_ready <wirename>_started)
              ;; for non-combinational non-leaf-calls.
              ;;
              ;; It is also <wirename>_ready for other kind of calls
              ;; (e.g. ternary-if)
              ;; if they have <wirename>_ready signal created,
              ;; with the ready-expression stored there.
              ;;
              ;; Alternatively, the ready-expression can be stored directly
              ;; here if no separate <wirename>_ready signal is wanted.
              ;;
              ;; (bitand test_ready (?: (rednor test then_ready else_ready)))
              ;; for ternary-ifs. Etc.

    uses_ready_wire
              ;; Here we have three cases (is this a bit kludgy, eh?):
              ;;
              ;;   #f (default) --> No separate <wirename>_ready wire
              ;;                    is created. However, readyexpr
              ;;                    may still be a non-trivial logical
              ;;                    expression, usable by surrounding
              ;;                    forms.
              ;;
              ;;   #t
              ;;                --> wire <wirename>_ready is created in
              ;;                    Verilog-code (without assignment),
              ;;                    and it is bound to the result_ready
              ;;                    signal of the noncombinational function
              ;;                    instantiation.
              ;;
              ;;   <level0-combinational-expression> 
              ;;                --> We have a separate <wirename>_ready wire
              ;;                    created for, and assigned to
              ;;                    <level0-combinational-expression>.
              ;;                    (I.e. not really needed, but makes
              ;;                     reading of the generated Verilog-code
              ;;                     easier.)
              ;;
              ;;   In both of the latter cases, readyexpr is either set to
              ;;   <wirename>_ready or expression that contains it.
              ;;   (That is, we have a kind of indirection here.)

;; The corresponding registers/wires are now added to wiredefs explicitly:

;;  ready_found_code ;;
              ;; If non-#f, then a register named name_rdy_found is created,
              ;; with the normal clear-to-zero code added to blunt start,
              ;; and separate Level-0 sequential code supplied in
              ;; into this field later added under
              ;; always @(posedge CLK) loop.
              ;; Needed by such special forms as FASTEST, SLOWEST, OR-F, AND-S
              ;; COND-F, etc.

;;  startable ;; #f if this comsig doesn't need a separate startable signal.
              ;; (e.g. if it's wholly combinational).

              ;;
              ;; Otherwise it's a combinational function assigned to
              ;; name_startable wire. (bitand (?: etc.))
              ;;  In that case, also name_started register is created
              ;;  (automatically cleared to zero in the blunt start
              ;;   and with loop restarts, if inside the loop body)
              ;;  and the sequential code:
              ;;  (:_if name_startable (:_n= name_started 1))
              ;;  under the always @(posedge CLK) loop.
              ;;  And the starting signal of fun is assigned as:
              ;;  (.start (bitand name_startable (bitnot name_started)))
)


(define (comsig-is-register? cs)
  (and (comsig? cs) (eq? (comsig-sigtype cs) 'reg))
)


(define (comsig-format out cs)
  (cond ((comsig? cs)
          (format out "(~s ~s ~a = ~s;\n        [noncombcode=~s]\n       [source=~s]\n        [readyexpr=~s, uses_ready_wire=~s, is-constant=~s]\n)\n"
                  (comsig-sigtype cs)
                  (comsig-width cs)
                  (comsig-wirename cs)
                  (comsig-combcode cs)
                  (comsig-noncombcode cs)
                  (comsig-source cs)
                  (comsig-readyexpr cs)
                  (comsig-uses_ready_wire cs)
                  (comsig-readyexpr-constant? cs)
          )
        )
        (else (format out "Not a comsig: ~s" cs))
  )
;; (if out (format out "\n")) ;; And additional newline for us on console...
;;  cs
)


(define-structure (loop (keyword-constructor)) ;; loopstructure, loop-structure
    registers ;; names of the loop registers. (or are they signal names?)
    resultreg-name ;;
    state-name ;;
)



;; COC stands for Compiling Context, not Current Continuation. (ill-chosen...)

(define-structure (coc (keyword-constructor) (copier))
    imod     ;; Are we (still) in iterative mode?

    startsignal1 ;; The name of start signal 1. This is transient signal.

    startsignal2 ;; The name of start signal 2. This is transient signal.

    nsu ;; "not startable until"
;; Name of the signal which can delay the starting of the contained forms.
;; Can be set by such forms as EVENTUALLY, SEQ, SEQ-IF, SEQ-OR and SEQ-AND.
;; Must be persistent, not transient signal.
;; However, as long as this stays #f, it means that an ordinary
;; (start <startsignal1>) binding is made for non-combinational
;; leaf call instantations, where <startsignal1> is the element
;; specified above.

    wiredefs ;; wires, i.e. comsig-structures defined so far.

    list-of-auxregs-to-be-reset ;; Contains names of all auxiliary registers
;; (mostly _started for non-leaf non-comb calls) to be reset at every start
;; (and also at every restarting of the loop, if inside the loop body).

    list-of-extras-for-waiting-state ;; Contains statements like
;;  `(:_if ,startable-name (:_n= ,started-name 1))

    names2sigs ;; varnames to comsig names mapping

    labels2loops ;; assoc list of (label . loopstructure)
;;    where we can loop back. (currently #f or a list of one element)

;; Transferred to trc in typreslv.scm:
;;  defined-functions ;; a list of defined functions to which we can refer to.

    branch-code ;; Branching code built for iterative ifs.

    paramdefs ;; List of parameters to be added into the beginning of module.

    parent-toc ;; A pointer to "top-oc" structure (defined below).

    compiled-forms

;; These three are copied from parent-toc:
    calling-convention-in-use

    error-printer

    warning-printer

    dbg-show-wiredefs?
)


(define (toc-make-fresh-coc toc)
   (make-coc 'imod Emod
             'startsignal1 'start ; (Use the function starting signal)
             'nsu #f ;; By default, use above.
             'wiredefs (list (list (list)))
             'list-of-auxregs-to-be-reset (list (list))
             'list-of-extras-for-waiting-state (list (list))
             'names2sigs (list) ;;
             'labels2loops (list) ;; Empty.
             'branch-code (list)
             'paramdefs (list (list (list)))
             'compiled-forms *compiled-forms* ;; XXX - move to toplevel.scm ?
             'calling-convention-in-use (toc-calling-convention-in-use toc)
             'error-printer (toc-error-printer toc)
             'warning-printer (toc-warning-printer toc)
             'dbg-show-wiredefs? (toc-coc-dbg-show-wiredefs? toc)
             'parent-toc toc
   )
)

(define (coc-form-branch-label coc)
   (apply symbol-append (cons 'branch_ (reverse (coc-branch-code coc))))
)



(define (coc-add-fullname.fundef-to-be-compiled-list! coc fullname fundef fis)
  (toc-add-fullname.fundef-to-be-compiled-list! (coc-parent-toc coc)
                                                fullname
                                                fundef
                                                fis
  )
)


(define (coc-find-comsig-by-varname coc src-name)
   (cond ((assq (t-elem src-name) (coc-names2sigs coc)) => cdr)
         (else
            (error
 "coc-find-comsig-by-varname: Unmapped variable referenced: " src-name
 " not in: " (coc-names2sigs coc)
            )
         )
   )
)


(define (coc-find-comsig-by-wirename coc comsigs-wirename)
   (cond ((assq comsigs-wirename (coc-wiredefs coc)) => cdr)
         (else (error
 "coc-find-comsig-by-wirename: could not locate the computing signal with wire name: "
                   comsigs-wirename
               )
         )
   )
)


(define (coc-pop-whole-list-of-extras! coc)
  (let loop ((extras (list)))
        (cond ((null? (car (coc-list-of-extras-for-waiting-state coc))) extras)
              (else (loop (cons (pop! (coc-list-of-extras-for-waiting-state coc))
                                extras
                          )
                    )
              )
        )
  )
)


(define (coc-pop-whole-list-of-auxregs-to-be-reset! coc)
  (let loop ((popped (list)))
        (cond ((null? (car (coc-list-of-auxregs-to-be-reset coc))) popped)
              (else (loop (cons (pop! (coc-list-of-auxregs-to-be-reset  coc))
                                popped
                          )
                    )
              )
        )
  )
)


(define-syntax with-reset-list-of-extras
  (syntax-rules ()
     ((with-reset-list-of-extras coc body ...)
        (let* ((saved-list-of-extras (coc-list-of-extras-for-waiting-state coc))
               (body-result
                 (let ()
                   (set-coc-list-of-extras-for-waiting-state! coc (list (list)))
                   body ...
                 )
                )
               )
            (set-coc-list-of-extras-for-waiting-state! coc saved-list-of-extras)
            body-result
        )
     )
  )
)


(define-syntax with-reset-list-of-auxregs
  (syntax-rules ()
     ((with-reset-list-of-auxregs coc body ...)
        (let* ((saved-list-of-auxregs (coc-list-of-auxregs-to-be-reset coc))
               (body-result
                 (let ()
                   (set-coc-list-of-auxregs-to-be-reset! coc (list (list)))
                   body ...
                 )
                )
               )
            (set-coc-list-of-auxregs-to-be-reset! coc saved-list-of-auxregs)
            body-result
        )
     )
  )
)



;; Why this works with let* but not with let, I do not understand!
;; (Well, because with let, the second initialization is executed before
;; the first, in MIT/GNU Scheme?)
;; (We don't actually use this, as this was just a rehearsal for above ones.)

(define-syntax with-saved-items
   (syntax-rules ()
     ((with-saved-items () body ...)
          (let () body ...)
     )

     ((with-saved-items (item1 rest ...) body ...)
        (with-saved-items (rest ...)
          (let* ((saved-item1 item1)
                 (body-result (let () body ...))
                )
             (set! item1 saved-item1)
             body-result
          )
        )
     )
   )
)



;;
;; Note: if our default scheme causes problems
;; (e.g. if startable-signal is/contains a very complicated combinational
;; expression, which is ready just before the raising clock edge???)
;; then we can make _started register(s) two bits wide,
;; and the code below something like (i.e. two bit wide shift register):
;;
;; (:_n= ,started-name (conc (bit ,started_name 0) ,startable-name))
;;
;; in which case the function invocation would be, generating (often)
;; 2 cycles long start-signals:
;;
;; (start (bitand ,startable-name (bitnot (redand ,started_name))))

;; Or aternatively:
;;
;; (:_n= ,started-name (conc (bit ,started_name 0)
;;                   (bitand (bitnot (bit ,started_name 1)) ,startable-name)))
;;
;; with the start-signal of the respective non-combinational
;; function invocation then bound just as: (introduces an extra delay as well)
;;
;; (start (bit ,started-name 1)) ;, Why not bit-0 ? This refers to simple reg.
;;
;; (Please do not believe an iota what I write here... I have to test this.)
;;


(define (store-new-comsig-to-wiredefs coc comsig)
  (cond ((comsig? comsig)
          (attach! (cons (comsig-wirename comsig) comsig) (coc-wiredefs coc))
        )
  )
  comsig
)


(define-syntax make-and-store-comsig-and-returns-its-name
  (syntax-rules ()
   ((make-and-store-comsig-and-returns-its-name coc ready-when indirected?
             humpty dumpty ...)
      (let ((newcs (make-comsig humpty dumpty ...))
            (ready-when* (optimize-combinational-level0-code ready-when coc))
           )
         (cond ((eq? indirected? 'noncombcall)
                 (set-comsig-uses_ready_wire! newcs #t)
                 (set-comsig-readyexpr! newcs ready-when*)
               )

               (indirected?
                 (set-comsig-uses_ready_wire! newcs ready-when*)
                 (set-comsig-readyexpr! newcs (cs-wirename_ready newcs))
               )

               (else
                 (set-comsig-uses_ready_wire! newcs #f)
                 (set-comsig-readyexpr! newcs ready-when*)
               )
         )

         (store-new-comsig-to-wiredefs coc newcs)
         (comsig-wirename newcs)
      )
   )
  )
)


;; This is opposite to push-new-wire-to-wiredefs!
;; (We should do some assertions here, e.g. that no other comsig
;; refers to this wire and that its ready-expr is 1, etc.)
(define (drop-comsig-and-returns-its-combcode coc signame)
   (let* ((cs (coc-find-comsig-by-wirename coc signame))
          (ptr-to-wires ;; Find its location, so that we can apply some surgery
             ((member-procedure (lambda (p cs) (equal? cs (cdr p))))
                 cs (coc-wiredefs coc)
             )
          )
          (combcode (comsig-combcode cs))
         )
      (pop! ptr-to-wires) ;; Effectively delete the comsig-structure.
      combcode ;; and just return its combcode.
   )
)



;; For pushing auxiliary wires to a common pool of wire-definitions:

(define (push-new-wire-to-wiredefs! coc signame combcode)
    (make-and-store-comsig-and-returns-its-name coc 1 #f
                   'wirename (if (pair? signame) (first signame) signame)
                   'width (if (pair? signame) (second signame) #f)
                   'sigtype 'wire
                   'combcode combcode
    )
)

(define (push-new-reg-with-init-to-wiredefs! coc signame reginit)
   (make-and-store-comsig-and-returns-its-name coc 1 #f
                   'wirename (if (pair? signame) (first signame) signame)
                   'width (if (pair? signame) (second signame) #f)
                   'sigtype 'reg
                   'reginit reginit
   )
)

;; Currently we consider registers to be always ready,
;; even although that is not strictly true. They are ready
;; only after loop has initialized.

(define (push-new-reg-to-wiredefs! coc signame)
   (push-new-reg-with-init-to-wiredefs! coc signame #f)
)



;; XXX - Not used:
(define (add-comsig-to-wiredefs-and-return-its-wirename coc comsig)
      (store-new-comsig-to-wiredefs coc new-comsig)
      (comsig-wirename new-comsig)
)


(define-syntax define-compiling-function ;; XXX --- Not used!
  (syntax-rules ()
   ((define-compiling-function (funname coc wirename src) e0 ...)
      (define (funname coc wirename src)
          (store-new-comsig-to-wiredefs coc ((lambda () e0 ...)))
      )
   )
  )
)




;; If src is symbol (i.e. Bream-variable name), then the corresponding
;; comsig should exist already, in which case we return an object
;; of type comsig, instead of an ordinary symbol.

(define (form-wirename src coc name)
   (cond ((t-is-symbol? src) (coc-find-comsig-by-varname coc src))
         ((texp? name) (comsigname-from-varname (t-elem name)))
         (name (comsigname-from-varname name)) ;; Non-typed name.
         ((t-is-pair? src)
           (if (symbol? (t-u-first src))
               (comsigname-from-funname (t-u-first src))
               #f ;; Something weird, e.g. a lambda-call, which doesn't create
           )      ;; a new comsig for itself.
         )
         (else ;; Usually (number? src)
             (comsigname-from-literal coc (t-elem src))
         )
   )
)


(define (coc-innermost-loop coc) (cdar (coc-labels2loops coc)))

;; Hide the ugly side-effecting code (those _mutators_!)
;; inside these call-with- forms, using copy-coc as our
;; little helper.

;; NOTE! copy-coc should copy structure coc only shallowly,
;; as we expect (attach! new-wire-definition (coc-wiredefs coc))
;; still to work (as expected, i.e. that all the attached items
;; will be eventually seen in the original, toplevel coc),
;; regardless of where in the call tree those attachments were made.
;;
;; At least this works in MIT/GNU Scheme 7.7. Define your own
;; copier, if you are not sure.



(define (call-with-imod-set-to coc emod-or-imod lambda_coc)
   (let ((new-coc (copy-coc coc)))
     (set-coc-imod! new-coc emod-or-imod)
     (lambda_coc new-coc)
   )
)

(define (compile-with-emod src coc name)
    (call-with-imod-set-to coc Emod (lambda (coc) (compile src coc name)))
)

(define (compile-with-imod src coc name)
    (call-with-imod-set-to coc Imod (lambda (coc) (compile src coc name)))
)


(define (call-with-new-startsignals coc new-startsignal1 new-startsignal2
                                    lambda_coc
        )
   (let ((new-coc (copy-coc coc)))
     (set-coc-startsignal1! new-coc new-startsignal1)
     (set-coc-startsignal2! new-coc new-startsignal2)
     (lambda_coc new-coc)
   )
)

(define (call-with-nsu coc newnsu lambda_coc)
   (let ((new-coc (copy-coc coc)))
     (set-coc-nsu! new-coc newnsu)
     (lambda_coc new-coc)
   )
)

;; Used by sequential special forms. Something like:
;; (compile-with-nsu coc #f `(bitand ,sig1 ,sig2) subexpr1)
(define (compile-with-nsu src coc name newnsu)
   (call-with-nsu coc newnsu
      (lambda (coc) (compile src coc name))
   )
)


;; Like before, but and the new wire with the previous value of
;; nsu, if present.

(define (compile-with-extra-nsu src coc name newnsu)
   (compile-with-nsu src
                     coc
                     name
                     (if (coc-nsu coc) `(bitand ,newnsu ,(coc-nsu coc)) newnsu)
   )
)


(define (call-with-additional-bindings coc more-names2sigs lambda_coc)
   (let ((new-coc (copy-coc coc)))
     (set-coc-names2sigs! new-coc ;; Apply side-effects only to the new copy!
                         (append more-names2sigs (coc-names2sigs new-coc))
     )
     (lambda_coc new-coc)
   )
)

(define (compile-with-more-bindings src coc name more-names2sigs)
   (call-with-additional-bindings coc more-names2sigs
      (lambda (coc) (compile src coc name))
   )
)


(define (call-with-new-loop-structure coc new-loop-label new-loop-structure lambda_coc)
   (let ((new-coc (copy-coc coc)))
     (set-coc-labels2loops!
             new-coc ;; Apply side-effects only to the new copy!
             (cons (cons new-loop-label new-loop-structure)
                   (coc-labels2loops coc)
             )
     )
     (lambda_coc new-coc)
   )
)


(define (call-with-branch-code-added coc branch-selector-code lambda_coc)
   (let ((new-coc (copy-coc coc)))
     (set-coc-branch-code! new-coc ;; Apply side-effects only to the new copy!
                    (if branch-selector-code
                        (cons branch-selector-code (coc-branch-code new-coc))
                        (coc-branch-code new-coc)
                    )
     )
     (lambda_coc new-coc)
   )
)

(define (compile-with-branch-code-added src coc name branch-selector-code)
   (call-with-branch-code-added coc branch-selector-code
      (lambda (coc) (compile src coc name))
   )
)


;; For example:
;; (define coc (make-coc 'nsu 'top_start 'wiredefs (list (list))
;;         'names2sigs (list (cons 'n 'n)))
;; )
;;
;; (call-with-additional-bindings coc '((muu . muu12) (maa . maa23))
;;     (lambda (coc) (coc-names2sigs coc))
;; )
;;  --> ((muu . muu12) (maa . maa23) (n . n))
;;
;; (coc-names2sigs coc) --> ((n . n))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is for the function arguments as well as tail call arguments:
(define (compile-unnamed-exprs-in-emode coc exprs)
   (map (lambda (expr) (compile-with-emod expr coc #f)) exprs)
)


;; XXX - Clean up, also how this interacts with <terminal-argument>
;; (Maybe this should be called there, not in the next function?!)
(define (create-new-wire-for-literal src coc tname)
  (let ((wid (determine-sig-width tname src)))
      (make-and-store-comsig-and-returns-its-name coc
                   1
                   #f
                   'wirename (form-wirename src coc tname)
                   'width wid
                   'sigtype 'wire
                   'source src
                   'combcode (list 'd wid (t-elem src))
      )
  )
)




(define (compile-and-bind-named-exprs-in-emode coc tnames exprs)
   (map (lambda (tname initexpr)         
          (cons (t-elem tname)
;; XXX - Just a quick fix now for literals:
                (if (t-is-integer? initexpr)
                    (create-new-wire-for-literal initexpr coc tname)
                    (let ((compexpr (compile-with-emod initexpr coc tname)))
                       (coc-find-comsig-by-wirename coc compexpr)
                    )
                )
          )
        )
        tnames
        exprs
   )
)


(define (compile-and-bind-named-exprs-in-emode-not-this-way coc tnames exprs)
   (map (lambda (tname initexpr)         
          (cons (t-elem tname)
                (let ((compexpr (compile-with-emod initexpr coc tname)))
;; XXX - Just a quick fix now for literals:
                   (if (integer? compexpr)
                       (create-new-wire-for-literal compexpr coc tname)
                       (coc-find-comsig-by-wirename coc compexpr)
                   )
                )
          )
        )
        tnames
        exprs
   )
)




(define (push-regs-in-emode! coc tnames)
   (map (lambda (tname)
          (let ((name (t-elem tname)))
            (cons name
                  (coc-find-comsig-by-wirename
                        coc
                        (push-new-reg-to-wiredefs! coc
                               (list (comsigname-from-varname name)
                                     (t-width tname)
                               )
;; WAS:
;;                                 (if (t-type-of-definite-width? tname)
;;                                     (list (comsigname-from-varname name)
;;                                           (t-width tname)
;;                                     )
;; XXX -- At this time we should know the width?:
;;                                     (comsigname-from-varname name)
;;                                 )
                        )
                  )
            )
          )
        )
        tnames
   )
)


(define (determine-sig-width named src)
    (or (and (texp? named) (t-width named))
        (and ;; (t-type-of-definite-width? src) ;; XXX - Also atleast-widths
             (t-width src)
        )
    )
)

;; Here we allocate a new signal name for each function argument,
;; (Not anymore... Would break the instantiation.
;;  XXX -- be careful with name conflicts, if user uses names
;;  ending with _started or _startable as his/her funarg names!)
;; and we also set each one's width with the value taken from argwidths.
;; Some kind of solution: start program generated special signal names
;; with underscore (_), and do not allow initial _ in user's code.
;; (Drawback: sorts related signals to different places in any generated
;; Verilog compilation dumps?)

(define (coc-add-func-args-to-sigdefs coc argnames)
   (map (lambda (tfargname)
          (let* ((wirename (t-elem tfargname))
;; Currently just farg's name. Cannot rename wires now with:
;;   (comsigname-from-varname wirename)
                 (width (t-width tfargname))
                 (funarg-wire
                    (make-and-store-comsig-and-returns-its-name coc 1 #f
                         'wirename wirename

                         'width width

;; XXX - inout.
                         'sigtype (if (type-output? (t-type tfargname))
                                      'wire-output
                                      'wire-input
                                  )
                    )
                 )
                )

             (cons wirename ;; was: fargname
                 (coc-find-comsig-by-wirename
                      coc
                      funarg-wire
                 )
             )
          )
        )
        argnames
   )
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (generate-new-wire-name var)
   (generate-interned-symbol (symbol-append (symbol var) '_))
)

(define generate-new-var-name generate-new-wire-name) ;; So far it's same.


(define (generate-new-inst-name funname)
   (generate-interned-symbol (symbol-append (symbol funname) '_))
)


(define comsigname-from-varname generate-new-wire-name)

(define (comsigname-from-funname funname)
  (generate-new-wire-name
    (cond ((assq funname *map_to_usable_names*) => cdr)
          (else funname)
    )
  )
)

(define (comsigname-from-literal coc lit) ;; XXX -- Just something for now.
  (generate-new-wire-name (format #f "lit_~a" lit))
)


(define-compiled-form ((regular 0.1 draft) begin src coc named)
  ((coc-error-printer coc) ;; We make our statement(s!)
 "compile: begin not supported! Use either par or seq: "
                    src
  )
)

(define-compiled-form ((regular 0.1 draft) lambda src coc named)
  ((coc-error-printer coc)
 "compile: Creation of lambda-forms not implemented yet: "
                    src
  )
)

(define-compiled-form ((regular 0.1 draft) letrec src coc named)
  ((coc-error-printer coc)
 "compile: Compiling of letrec not implemented yet: "
                    src
  )
)


(define-compiled-form ((regular 0.1 draft) quote src coc named)
  ((coc-error-printer coc)
 "compile: Compiling of quoted expressions not implemented yet: "
                    src
  )
)

(define-compiled-form ((regular 0.1 draft) quasiquote src coc named)
  ((coc-error-printer coc)
 "compile: Compiling of quasiquoted (backquote) expressions not implemented: "
                    src
  )
)

(define-compiled-form ((regular 0.1 draft) unquote src coc named)
  ((coc-error-printer coc)
 "compile: Compiling of unquote (comma) not implemented: "
                    src
  )
)


(define-compiled-form ((regular 0.1 draft) (<generic-funcall>) src coc named)
  ((coc-error-printer coc)
 "compile: First-class functions not implemented yet. Don't know what to do with form: "
     src
  )
)


(define-compiled-form ((regular 0.1 draft) (lambda) src coc named)
  (let ((fargs (t-u-second (t-first src))) ;; (cadar src)
        (cargs (t-carglist src))
        (bodyexpr (body-with-no-implicit-progn
                          (cdr (t-rest (t-first src)))
                          coc
                          "lambda"
                  )
        )
       )
     (compile-with-more-bindings
                              bodyexpr
                              coc #f
                              (compile-and-bind-named-exprs-in-emode coc fargs
                                                                        cargs
                              )
     )
  )
)




;; compile-named-let -- One of the core functions here, handles
;; the compiling of loops. Creates a finite state machine
;; wrapped around the body code of the loop, with four states
;; listed below:

;; From every state we can go just to the next state (labeled in Gray code
;; order), except that
;; from ST_LOOP_WAITING we can also go back to ST_LOOP_RESTARTED
;; (when we are looping.)


(define-compiled-form ((regular 0.1 draft) <let-named> src coc named)

  (define STATE-PARAMETERS-FOR-LOOPS
    (list '(ST_LOOP_READY         0) ;; 2'b00 <---+
;;                                        |       |
;;                                        V       |
          '(ST_LOOP_INITS         1) ;; 2'b01     |
;;                                        |       |
;;                                        V       |
          '(ST_LOOP_RESTARTED     3) ;; 2'b11     |
;;                                        ^       |
;;                                        |       |
;;                                        V       |
          '(ST_LOOP_WAITING       2) ;; 2'b10 ----+

    )
  )
  (define STATE-PARAMETERS-WIDTH-FOR-LOOPS 2)

  (let* ((looplabel (t-u-second src))
         (vars_et_inits_typed (t-third src))
         (vars_et_inits (t-elem vars_et_inits_typed))
         (bodyexpr (body-with-no-implicit-progn (cddr (t-rest src))
                                                coc
                                                "named let"
                   )
         )
         (fargs (map t-first vars_et_inits))
         (cargs (map t-second vars_et_inits))
        )
     (cond 
       ((and #f (not (coc-imod coc)))
           (error
             (format #f
 "compile-named-let: Cannot define a named let in expressive mode (with name ~s). Please divide the code to separate toplevel functions: "
                     looplabel
             )
             src
           )
       )
       ((pair? (coc-labels2loops coc)) ;; Not the first label?
           (error
             (format #f
 "compile-named-let: Cannot define a new named let in iterative mode (with name ~s, in addition to existing ~s). Nested loops not implemented yet, please divide the code to separate toplevel functions: "
                     looplabel (car (first (coc-labels2loops coc)))
             )
             src
           )
       )
       (else
         (let* ((startsignal (coc-startsignal1 coc))
                (loopname (symbol-append 'loop_ looplabel))
                (loopstate-name (push-new-reg-with-init-to-wiredefs!
                                      coc
                                      (list (symbol-append loopname '_state)
                                            STATE-PARAMETERS-WIDTH-FOR-LOOPS
                                      )
                                      'ST_LOOP_READY
                                )
                )

;; Do not push this one to wires, because it will be used as a wirename
;; of this whole comsig definition we will be returning as a result:
                (loopresultreg-name (comsigname-from-varname
                                      (symbol-append loopname '_result))
                )

                (restarted-sig (push-new-wire-to-wiredefs!
                                   coc
                                   (list (symbol-append loopname '_restarted)
                                         1
                                   )
                                   `(== ST_LOOP_RESTARTED ,loopstate-name)
                               )
                )

                (waiting-sig (push-new-wire-to-wiredefs!
                                   coc
                                   (list (symbol-append loopname '_waiting)
                                         1
                                   )
                                   `(== ST_LOOP_WAITING ,loopstate-name)
                               )
                )

                (inits-compiled (compile-unnamed-exprs-in-emode coc cargs))
                (more-bindings (push-regs-in-emode! coc fargs))
                (loopregs (map cdr more-bindings))
                (inits-ready-when (readyexpr-from-subexprs inits-compiled coc))

                (loopregs-with-their-inits
                    (append-map! (lambda (loopreg initexpr)
                                      (list (comsig-wirename loopreg)
                                            initexpr
                                      )
                                 )
                                 loopregs
                                 inits-compiled
                    )
                )

               )

              (with-reset-list-of-auxregs coc
                (with-reset-list-of-extras coc
                  (let*

                     ((bodycode ;; code compiled from the body of the loop.
                         (call-with-new-loop-structure coc
                              looplabel
                              (make-loop 
                                     'resultreg-name loopresultreg-name
                                     'state-name loopstate-name ;;
                                     'registers loopregs
                              )
                              (lambda (coc)
                                 (call-with-new-startsignals coc restarted-sig
                                                                 waiting-sig
                                    (lambda (coc)
                                      (call-with-imod-set-to coc Imod
                                        (lambda (coc)
                                           (compile-with-more-bindings
                                                         bodyexpr
                                                         coc #f more-bindings
                                           )
                                        )
                                      )
                                    )
                                  )
                              )
                         )
                      )
      
      ;; These must be _AFTER_ we have compiled the bodycode:
      ;; 
      
                      (auxregs-to-be-reset
                           (coc-pop-whole-list-of-auxregs-to-be-reset! coc)
                      )
      
                      (auxresets (append-map! (lambda (regname) (list regname 0))
                                              auxregs-to-be-reset
                                 )
                      )
      
                      (extras-for-waiting-state (coc-pop-whole-list-of-extras! coc))
      
                      (code
      ;; If start-signal is raised, then regardless of what state we are in,
      ;; we go to ST_LOOP_INITS. (Note: inits should use the same startsignal!)
      ;; The startsignal is usually the starting signal of the _whole function_.
                        `(:_always ((posedge clk))
                            (:_if ,startsignal
                              (:_n== ,loopstate-name ST_LOOP_INITS ,@auxresets)
;; auxresets contains the Level0-code for reseting of all _started and
; _ready_found registers needed inside the loop body.

;; Otherwise, check the state we are in:
                              (:_case ,loopstate-name

                                (ST_LOOP_READY (:_begin)) ;; Do nothing.
      
                                (ST_LOOP_INITS
                                    ,(expr-wait-wrapped inits-ready-when
                                          `(:_n== ,loopstate-name ST_LOOP_RESTARTED
                                                ,@loopregs-with-their-inits
                                           )
                                     )
                                )

;; From ST_LOOP_RESTARTED we transfer immediately to ST_LOOP_WAITING:
;; And we also clear all the auxiliary registers like _started, etc.
;; They will be zero at the first cycle of ST_LOOP_WAITING state,
;; i.e. one cycle LATER than what the combinational and non-combinational
;; leaf-calls started at. However, we still should have enough time
;; to not mess up things. (20 ns).
;; (If not, then we have to insert an additional idle state, ugh!)

                                (ST_LOOP_RESTARTED
                                     (:_n== ,loopstate-name ST_LOOP_WAITING
                                            ,@auxresets
                                     )
                                )
      
;; In ST_LOOP_WAITING state we check the remaining stuff in bodycode,
;; from which we can transfer either back to ST_LOOP_RESTARTED
;; of ST_LOOP_READY, depending on whether we loop or return.

                                (ST_LOOP_WAITING
                                     (:_begin ,@extras-for-waiting-state
                                              ,bodycode
                                     )
                                )
                              )
                            )
                         )
                      )
                     )
      
                   (for-each
                       (lambda (param-def)
                         (add-param-to-paramdefs-if-not-already-there! coc param-def)
                       )
                       STATE-PARAMETERS-FOR-LOOPS
                   )
      
                   (make-and-store-comsig-and-returns-its-name coc
                         `(== ,loopstate-name ST_LOOP_READY) #t
                         'wirename loopresultreg-name
                         'width (determine-sig-width #f bodyexpr)
                         'sigtype 'reg
                         'source src
                         'noncombcode code
                   )
                  ) ;; let*
                )
             )
         )
       )
     )
  )
)


(define (add-param-to-paramdefs-if-not-already-there! coc param-def)
  (let ((old-params (coc-paramdefs coc)))
    (cond ((assq (car param-def) old-params)
            =>
            (lambda (name.olddef)
              (cond ((not (equal? (cdr name.olddef) (cdr param-def)))
                       (begin
                         ((coc-warning-printer coc)
                             (format #f
 "The parameter ~s: old definition ~s superseded with a new definition: ~s"
                                     (car name.olddef)
                                     (cdr name.olddef)
                                     (cdr param-def)
                             )
                         )
                         (set-cdr! name.olddef (cdr param-def)) ;; Quick!
                       )
                    )
                    (else #t) ;; It was already there, with the same definition
              )
            )
          )
          (else (attach! param-def old-params))
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;::::;
;;                                                                    ;;
;; Code for computing a combinational ready-expression from a given   ;;
;; list of code.                                                      ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;::::;


(define (is-constant? x) (and (integer? x) x)) ;; For the moment.

(define (comsig-readyexpr-constant? cs)
  (case (comsig-uses_ready_wire cs)
     ((#f #t) (is-constant? (comsig-readyexpr cs)))
     (else (is-constant? (comsig-uses_ready_wire cs)))
  )
)

;; Return #t if this wire is always ready (i.e. it is a combinational expression).
(define (comsig-readyexpr-constant-1? cs)
  (eq? (comsig-readyexpr-constant? cs) 1)
)


;; This returns the constant value (integer) of readyexpr associated
;; with comsig, if it is constant, otherwise returns the expression
;; (which is either a combinational expression or a wirename).
(define (comsig-readyexpr* cs)
   (or (comsig-readyexpr-constant? cs) (comsig-readyexpr cs))
)


(define (comsig-readyexpr*-by-wirename coc wirename)
    (comsig-readyexpr* (coc-find-comsig-by-wirename coc wirename))
)


(define (bream-literal? x) (integer? x))

(define (get-readyexpr s coc)
    (cond ((bream-literal? s) 1) ;; Literals are immediately ready.
          ((not (pair? s)) ;; If a wire-name, it is already computed for it:
              (comsig-readyexpr*-by-wirename coc s)
          )
          (else
             (optimized-and (map (lambda (x) (get-readyexpr x coc)) (cdr s)) coc)
          )
    )
)

(define (readyexpr-from-subexprs subs coc) (get-readyexpr (cons 1 subs) coc))



;; (READY? literal) = immediately. (literal = 1, for example). Immediately = 1 ???
;; (READY? wire_from_outside) = wire_from_outside_ready; (???)
;; (READY? (comb-fun X)) = (READY? X)
;; (READY? (comb-fun X Y)) = (LOGAND (READY? X) (READY? Y))
;; 
;; (READY? (seq-fun literal_or_wire1 literal_or_wire2 ...)) = (READY? seq-fun)
;;  = (LOGAND (STARTED? seq-fun) (READY? seq-fun))
;;  = (LOGAND (STARTED? starting_time) (READY? seq-fun))
;; If under SEQ, SEQ-AND or SEQ-OR form, then the (STARTED? seq_fun)
;; depends on the completion of the preceding function in that form.
;; 
;; (READY? (seq2fun (seq1fun (X))) = (LOGAND (STARTED? seq2fun) (READY? seq2fun))
;; 
;; (READY? (seq3fun (seq2fun (seq1fun (X)))) = (LOGAND (STARTED? seq3fun) (READY? seq3fun))
;; (READY? (seq-fun X Y Z))
;; 




;; These are unnecessary now:

;; Collect the list of comsig-structures that are contained in the
;; transitive closure of cs. The result contains always at least cs itself.

(define (comsig-call-trees-leaves cs)
    (cons cs (append-map! comsig-call-trees-leaves (comsig-argsigs cs)))
)

;; (Currently)
;; comsig is noncombinational (in itself) IF AND ONLY IF there is some
;; function instantation code added into its noncombcode slot:

(define (comsig-itself-noncombinational? cs) ;; If it is, return it back.
  (and cs (not (null? cs)) (pair? (comsig-noncombcode cs)))
)

;; Recursive function which examines whether any comsig in
;; the transitive closure of cs contains an invocation of
;; noncombinational function:

(define (comsig-involves-noncombinational-call? cs)
 (there-exists? (comsig-call-trees-leaves cs) comsig-itself-noncombinational?)
)

(define (comsig-list-contains-noncombinational-call? subexprs)
  (there-exists? subexprs comsig-involves-noncombinational-call?)
)


(define (comsig-all-noncombinational-calls cs)
  (keep-matching-items (comsig-call-trees-leaves cs)
          comsig-itself-noncombinational?
  )
)


(define (comsig-itself-combinational? cs)
   (and (not (comsig-itself-noncombinational? cs)) cs)
)


(define (comsig-fully-combinational? cs) ;; I.e. no noncomb calls anywhere.
   (and (not (comsig-involves-noncombinational-call? cs)) cs)
)


;;
;; To make debugging of the generated Verilog-code easier,
;; it would be nice if purely combinational expressions were NOT
;; destructured to their most elementary parts.
;; For every non-combinational function, we MUST generate
;; its own signal, tied to the result-output of an instantiation
;; of the said function.
;;

(define (wirename_ready signame) (symbol-append signame '_ready))

(define (cs-wirename_ready cs) (wirename_ready (comsig-wirename cs)))

(define (wirename_startable signame) (symbol-append signame '_startable))

(define (cs-wirename_startable cs) (wirename_startable (comsig-wirename cs)))

(define (wirename_started signame) (symbol-append signame '_started))

(define (cs-wirename_started cs) (wirename_started (comsig-wirename cs)))

;; The Verilisp-way:
;;  (funname
;;      (start start or (bitand wirename_startable (bitnot wirename_started)))
;;      (name d d_)
;;      (name c (cat c0 c1))
;;      (result_rdy wirename_ready)
;;      (result wirename)
;;  )
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;::::;


;; Note: exprs-to-add can be also (), in which case nothing is added.


(define (add-expressions-to-anded-signals coc exprs-to-add signals)
  (optimize-combinational-level0-code
    (cond ((not (pair? signals)) (cons 'bitand (cons signals exprs-to-add)))
          ((not (eq? (first signals) 'bitand))
             (error
 "add-expressions-to-anded-signals expects that its second argument is a list beginning with symbol bitand, not an expression like: "
                signals
             )
          )
          (else (cons 'bitand (append (cdr signals) exprs-to-add)))
    )
    coc
  )
)



(define (optimized-and signals coc)
  (optimize-combinational-level0-code (cons 'bitand signals) coc)
)



;; One of the core-functions follows.

;;
;; It's nice that compile-unnamed-exprs-in-emode
;; returns a list of wirenames (which all refer to
;; comsig-structures), as then we can refer to their
;; _started, _ready, and other fields. (e.g. _funtype, _rettype)
;;
;; However, it's not nice to debug Verilog-code, which
;; is cluttered with wire-names for every possible sub-expression,
;; even if just simple combinational logic.
;;
;; So, we could have an option, that if some of the lists
;; in the list returned by compile-unnamed-exprs-in-emode
;; are lists themselves, they are understood as combinational
;; expressions, for whose subexpressions no separate
;; wires needs to be generated.
;;
;; However, we might have combinational logic, which includes
;; a call or two to some non-combinational function, among its
;; leaves.
;;

;; So it's better to leave (leaves again!) the pruning of
;; unnecessary wires for a later, "optimization stage".
;; How is that done?
;;
;; Among any expression, any subexpression which is a not explicitly
;; named wire (i.e. there's no fan-out) whose involved function is
;; combinational, (i.e. it has no _startable signal, and its _ready
;; signal is not tied to result_ready signal of some noncombinational
;; instantiation), can be replaced with its Level0-code?
;;

;;
;; So why we need a standardized wire-representation now?
;; (So that each subexpression has an unique wire-name,
;; with which its comsig-structure can be fetched, which
;; contains further, important information about that
;; wire/subexpression.)
;;

;; So... (compile ...) returns a list containing non-symbols
;; only when in iterative mode, never when called for example
;; through compile-with-emod (which compile-unnamed-exprs-in-emode uses).



(define-compiled-form-with-final-branch-decorator ((regular 0.1 draft) <generic-funcall> src coc named)
   (if (combinational? (t-u-first src))
       (dispatch-to-compiled-form '(<combinational-funcall>) src coc named)
       (dispatch-to-compiled-form '(<noncombinational-funcall>) src coc named)
   )
)



(define (combfun_renamer sym) sym) ;; XXX - A leftover, eradicate this!


;; XXX - Todo: preferably don't separate all sub-expressions of a combinational
;; expression into separately named wires, but keep its structure together.
;; (Unless of course the thing is explicitly named by specifying named).
;;
;; (Might use drop-comsig-and-returns-its-combcode here, or not.)

(define-compiled-form ((regular 0.1 draft) <combinational-funcall> src coc named)
   (let ((subs-compiled (compile-unnamed-exprs-in-emode coc (t-carglist src))))

      (make-and-store-comsig-and-returns-its-name coc
                   (readyexpr-from-subexprs subs-compiled coc)
                   (if named #t #f) ;; Create a separate ready-wire if named.
                   'wirename (form-wirename src coc named)
                   'width (determine-sig-width named src)
                   'sigtype 'wire
                   'source src
                   'combcode (cons (combfun_renamer (t-u-first src)) subs-compiled)
      )
   )
)


;; XXX - A lot's to do!

;; Width-information for arguments & result can flow from two sources:
;;
;; (1) Explicitly defined with width-specifiers in the function definition.
;;
;;     If the call arguments are of lesser width, they are zero-extended
;;     to the required width, or sign-extended if (SXT expr) is used.
;;     (should we require also ZXT to be present, and flag an error otherwise?)
;;
;;     (The width-resolver can also make destructive changes to code,
;;      e.g. if it sees SXT or ZXT or drop, it can replace it with
;;      appropriate piece of code, like (bits ...) ?)
;;
;;     If the call arguments are of greater width, an error is generated?
;;
;;     Also the result width can be explicitly specified in the source
;;     of the invoked function, or it can follow as an implication
;;     from the explicitly specified arguments widths.
;;
;; (2) By width-resolver from the sub-expressions of the call-arg expression.
;;
;;     In this case, the corresponding formal argument is set to the same
;;     resolved width.
;;
;;     Also, the result width usually follows from the resolved arg widths,
;;     and is not clear before the invoked function has been compiled
;;     (or at least has been run through width-resolver).
;;

(define (level0code-for-noncomb-fun coc src subexprs wirename startable-signal)
   (let* ((callees-fis (t-callees-fis src))
          (callees-fundef (t-callees-fundef src))
          (name_et_fargs (t-callees-name-and-fargs src)) ;; Of the callee!
          (fullname (t-callees-fullname src))
          (funshortname (t-u-first name_et_fargs))
          (fargs (t-rest name_et_fargs))
;;        (funname (t-u-first src))
;;        (fargs (coc-defined-functions-fargs coc funname))
          (farg_subexp_pairs
                (map (lambda (fargname subexpr)
                          (list (detyped fargname)
                                subexpr ;; a wire name, that is.
                          )
                     )
                     fargs subexprs
                )
          )
          (inst-name (generate-new-inst-name funshortname))
         )

    (begin
;;    (if (not (eq? (funinfo-deftype callees-fis) 'declare-extfun))
;;        (coc-add-fullname.fundef-to-be-compiled-list!
;;                                  coc fullname callees-fundef callees-fis
;;        )
;;    )

      (case (funinfo-deftype callees-fis)
        ((define <define-expanded>)
            (coc-add-fullname.fundef-to-be-compiled-list!
                                    coc fullname callees-fundef callees-fis
            )
        )
        ((declare-extfun)
            '() ;; Do nothing.
        )
        (else
             (error
;;           ((coc-logging-printer2 coc) coc 0
"level0code-for-noncomb-fun: (Bug in type-resolver, syntax or macro expander?) Don't know what to do at compile time with function of definition type: "
                    (funinfo-deftype callees-fis)
;;              src
;;           )
             )
        )
      )

     `(:_instantiate ,fullname ,inst-name
         (clk clk) ;; The heartbeat, the suture!
         (start ,(if (not (eq? 1 startable-signal))
                    `(bitand ,(or (coc-startsignal2 coc) 1) ; Ignore #f's - XXX
                             ,(wirename_startable wirename)
                             (bitnot ,(wirename_started wirename))
                     )
;; Otherwise, if there were no specific delays, then this
;; function is started exactly the same time as the surrounding
;; "toplevel" function or loop is.
                     (coc-startsignal1 coc)
                 )
         )
         ,@farg_subexp_pairs
         (result_ready ,(wirename_ready wirename))
         (result ,wirename)
      )
    )
   )
)


(define-compiled-form ((regular 0.1 draft) <noncombinational-funcall> src coc named)
   (define (level0stmt-for-started-regs startable-name started-name)
       `(:_if ,startable-name (:_n= ,started-name 1))
   )

   (let* ((thiswirename (form-wirename src coc named))

          (subs-compiled (compile-unnamed-exprs-in-emode coc (t-carglist src)))

          (subexprs-ready (readyexpr-from-subexprs subs-compiled coc))

          (expr-for-startable-wire ;; Will be 1, if not needed.
                  (add-expressions-to-anded-signals
                        coc
                        (if (coc-nsu coc)
                            (list (coc-nsu coc))
                            (list) ;; Nothing if no special signal defined.
                        )
                        subexprs-ready
                  )
          )

          (expr-for-readyexpr
             (cond ((not (eq? 1 expr-for-startable-wire))
                      (optimized-and (list (wirename_ready thiswirename)
                                           (wirename_started thiswirename)
                                     )
                                     coc
                      )
                   )
                   (else ;; Otherwise, we use just <wirename>_ready.
                      (wirename_ready thiswirename)
                   )
             )
          )

         )


      (if (not (equal? 1 expr-for-startable-wire))
          (let ((startable-name (wirename_startable thiswirename))
                (started-name (wirename_started thiswirename))
               )
             (attach! started-name (coc-list-of-auxregs-to-be-reset coc))

             (attach! (level0stmt-for-started-regs startable-name started-name)
                      (coc-list-of-extras-for-waiting-state coc)
             )

             (push-new-reg-to-wiredefs! coc (list started-name 1))

             (push-new-wire-to-wiredefs! coc (list startable-name 1)
                                            expr-for-startable-wire
             )
          )
      )

      (make-and-store-comsig-and-returns-its-name coc
                   expr-for-readyexpr
                   'noncombcall
                   'wirename thiswirename
                   'width (determine-sig-width named src)
                   'sigtype 'wire
                   'source src
                   'noncombcode
                            (level0code-for-noncomb-fun
                                coc src subs-compiled thiswirename
                                expr-for-startable-wire
                            )
      )
   )
)


(define compile-test-expr compile-with-emod) ;; XXX -- add "type checking" later, i.e. should allow only "predicates" like zero?, ==, !=, etc.
;; Type-checking now done in typereslv.scm


(define (expr-wait-wrapped ready-expr rest)
  (if (equal? 1 ready-expr)
      rest
      `(:_if ,ready-expr ,rest)
  )
)


(define (if-wait-wrapped test-expr-ready test-expr then-expr else-expr)
 (expr-wait-wrapped test-expr-ready
                                 `(:_if ,test-expr ,then-expr ,else-expr)
 )
)


;; Note: if ternary-if has combinational test-expression,
;; and the other branch (then or else) is combinational,
;; and the other branch is non-combinational, and the whole
;; if-expression's result is used as an argument for
;; another non-combinational function (possibly through
;; intervening combinational expressions), then we have to insert
;; delay1 call to combinational branch of if, because otherwise
;; we cannot reliably know/create _startable and _started signals
;; for the surrounding non-combinational form, as there must be
;; at least one cycle between the restarting of loop and its
;; activation.
;; (Optimization: we can insert the delay1 at the "thinner end"
;; of the combinational then/else-expression. E.g. if there is ~|
;; or ++ or ** on the other hand.)

;; The same is true for forms like OR-F, FASTEST, etc?
;; Or do they (already) always require at least one cycle?


(define-compiled-form ((regular 0.1 draft) if src coc named)
   (if (coc-imod coc)
       (compile-iterative-if src coc named)
       (compile-ternary-if src coc named)
   )
)

(define (compile-ternary-if src coc named)
 (let* ((thiswirename (form-wirename src coc named))
        (test-expr (compile-test-expr (t-second src) coc 'tif_test))
        (then-expr (compile (t-third src) coc #f))
        (else-expr (compile (t-fourth src) coc #f))
        (test-expr-ready (get-readyexpr test-expr coc))
        (then-expr-ready (get-readyexpr then-expr coc))
        (else-expr-ready (get-readyexpr else-expr coc))
        (combcode (list '?: test-expr then-expr else-expr))
        (ready-when
            `(bitand ,test-expr-ready
                     (?: (0!= ,test-expr)
                         ,then-expr-ready
                         ,else-expr-ready
                     )
             )
        )
       )

      (make-and-store-comsig-and-returns-its-name coc ready-when #t
                   'wirename thiswirename
                   'width (determine-sig-width named src)
                   'sigtype 'wire
                   'source src
                   'combcode combcode
      )
 )
)



;; We have both ordinary iterative if and sequential iterative if.
;; The former works as usual, the latter includes also
;; 

(define (compile-iterative-if src coc name)
  (let* ((test-expr (compile-test-expr (t-second src) coc 'if_test))
         (test-expr-ready (get-readyexpr test-expr coc))
         (then-expr (compile-with-branch-code-added (t-third src) coc #f 'T))
         (else-expr (compile-with-branch-code-added (t-fourth src) coc #f 'E))
        )
      (if-wait-wrapped test-expr-ready test-expr then-expr else-expr)
   )
)


;; Same as above, but this is not speculative, meaning that
;; the then- and else-branches must wait for the
;; completion of test-branch, before they even start.
;; (That is, if they are noncombinational. With combinational
;; expressions it doesn't matter, they "start" anyway immediately.)

;; Important: only the other branch should be started (THEN or ELSE,
;; but NOT BOTH). That's why we ensure that there will be created a
;; separate wire for the test-expression (by naming it),
;; which is thus used in not_startable_until condition of
;; those branches.

(define-compiled-form ((regular 0.1 draft) seq-if src coc named)
 (let* ((thiswirename (form-wirename src coc named))
        (test-expr (compile-test-expr (t-second src) coc 'seqif_test))
        (test-expr-ready (get-readyexpr test-expr coc))
        (then-expr
          (call-with-branch-code-added coc (and (coc-imod coc) 'T)
             (lambda (coc)
                 (compile-with-extra-nsu
                            (t-third src)
                            coc #f
                            `(bitand ,test-expr-ready (redor ,test-expr))
                 )
             )
          )
        )

        (else-expr
          (call-with-branch-code-added coc (and (coc-imod coc) 'E)
             (lambda (coc)
                 (compile-with-extra-nsu
                            (t-fourth src)
                            coc #f
                            `(bitand ,test-expr-ready (rednor ,test-expr))
                 )
             )
          )
        )

       )

      (if (coc-imod coc)

          (if-wait-wrapped test-expr-ready test-expr then-expr else-expr)

          (let* ((then-expr-ready (get-readyexpr then-expr coc))
                 (else-expr-ready (get-readyexpr else-expr coc))
                 (combcode (list '?: test-expr then-expr else-expr))
                 (ready-when
                     `(bitand ,test-expr-ready
                              (?: (0!= ,test-expr)
                                  ,then-expr-ready
                                  ,else-expr-ready
                              )
                      )
                 )
                )
            (make-and-store-comsig-and-returns-its-name coc ready-when #t
                     'wirename thiswirename
                     'width (determine-sig-width named src)
                     'sigtype 'wire
                     'source src
                     'combcode combcode
            )
          )
      )
 )
)

;;
;; (AND2 a b) waits for either a or b to return zero (false),
;;            in which case returns immediately zero,
;;            otherwise, when both are ready and none returned
;;            zero, return the value of b.
;;
;;            (Equally, if a is ready and returns non-zero,
;;             return the value of b, when it's ready.)
;;


(define-compiled-form-with-final-branch-decorator ((regular 0.1 draft) and2 src coc named)
 (let* ((thiswirename (form-wirename src coc named))
        (left-expr (compile-with-emod (t-second src) coc 'and2_left))
        (right-expr (compile-with-emod (t-third src) coc 'and2_right))
        (left-expr-ready (get-readyexpr left-expr coc))
        (right-expr-ready (get-readyexpr right-expr coc))

        (ready-when
;; This just for testing now also here. (Note that it is idempotent function,
;; which, when applied twice produces the same result as when applied once):
          (optimize-combinational-level0-code
            `(bitor (bitand ,left-expr-ready (0== ,left-expr))
                    (bitand ,right-expr-ready (0== ,right-expr))
                    (bitand ,left-expr-ready ,right-expr-ready)
             )
             coc
          )
        )

        (right-expr* (if (eq? 1111 ready-when) ;; No need to chk right-expr twice
;; XXX -- Don't call it now, buggy code, test2a would fail !
;; XXX -- We should NOT drop named expressions that are used elsewhere!
                         (drop-comsig-and-returns-its-combcode coc right-expr)
                         right-expr
                     )
        )

        (combcode `(?: (0== ,left-expr) ,left-expr ,right-expr*))

       )

          (make-and-store-comsig-and-returns-its-name coc
                  ready-when
                  #t
                  'wirename thiswirename
                  'width (determine-sig-width named src)
                  'sigtype 'wire
                  'source src
                  'combcode combcode
          )
 )
)



(define-compiled-form ((regular 0.1 draft) seq2 src coc named)
 (let* ((thiswirename (form-wirename src coc named))
        (left-expr (compile-with-emod (t-second src) coc 'seq2))
        (left-expr-ready (get-readyexpr left-expr coc))
        (right-expr (compile-with-extra-nsu
                            (t-third src)
                            coc #f
                            left-expr-ready
                    )
        )
       )

      (if (coc-imod coc)

;;        `(:_if ,left-expr-ready ,right-expr)
          (expr-wait-wrapped left-expr-ready right-expr)

          (make-and-store-comsig-and-returns-its-name coc
                     (get-readyexpr right-expr coc)
                     #t
                     'wirename thiswirename
                     'width (determine-sig-width named src)
                     'sigtype 'wire
                     'source src
                     'combcode right-expr
          )
      )
 )
)


(define-compiled-form-with-final-branch-decorator ((regular 0.1 draft) par2 src coc named)
 (let* ((thiswirename (form-wirename src coc named))
        (left-expr (compile-with-emod (t-second src) coc #f))
        (right-expr (compile-with-emod (t-third src) coc #f))
        (left-expr-ready (get-readyexpr left-expr coc))
        (right-expr-ready (get-readyexpr right-expr coc))
       )

          (make-and-store-comsig-and-returns-its-name coc
                     `(bitand ,left-expr-ready ,right-expr-ready)
                     #t
                     'wirename thiswirename
                     'width (determine-sig-width named src)
                     'sigtype 'wire
                     'source src
                     'combcode right-expr
          )
 )
)




(define-compiled-form-with-final-branch-decorator ((regular 0.1 draft) <terminal-argument> src coc named)

   (define (compile-literal src coc name) `(d ,(t-width src) ,(t-elem src)))

   (cond ((t-is-pair? src) #f) ;; Not handled here.
         ((t-is-symbol? src)
;; XXX - Another ugly kludge here:
            (let ((cs-or-literal (coc-find-comsig-by-varname coc src)))
               (if (comsig? cs-or-literal)
                   (comsig-wirename cs-or-literal)
                   cs-or-literal
               )
            )
         )
         (else (compile-literal src coc named)) ;; E.g. () and numbers, also #f
   )
)



;; XXX - Rudimentary implementation for testing. I'm still not sure,
;;       at what level this should be handled:
;; In principle (drop x n) is equal to (bits x (-1+ (WIDTH x)) n)
;; (So we might implement this as a WIRM-macro later.)

(define-compiled-form ((regular 0.1 draft) drop src coc named)
 (let* ((sub1width (t-width (arg-first src)))
        (sub1expr (compile (arg-first src) coc #f))
        (sub2expr (compile (arg-second src) coc #f))
        (subs-compiled (list sub1expr sub2expr))
        (combcode (list 'bits sub1expr (- sub1width 1) sub2expr))
       )

      (make-and-store-comsig-and-returns-its-name coc
                   (readyexpr-from-subexprs subs-compiled coc)
                   (if named #t #f) ;; Create a separate ready-wire if named.
                   'wirename (form-wirename src coc named)
                   'width (determine-sig-width named src)
                   'sigtype 'wire
                   'source src
                   'combcode combcode

      )
 )
)



;; The argument named refers to a variable name in Bream code.
;; That is, it is usually either a var-name for init-forms
;; of let or a formal argument of a lambda call.
;; If specified, then it will be used for forming the wire / register name.
;; Otherwise the wirename is formed from the function / special form
;; name involved.
;; To get meaningful names for the sub-expressions,
;; named SHOULD NOT be propagated from compile-* functions back to compile.



(define (dispatch-to-compiled-form form-name src coc named)
   (generic-dispatch form-name

                   src

                   (coc-list-of-compiled-forms-in-use coc)

                   first

                   (lambda (x) x)

                   (lambda (src . rest) (error "Unknown compiled form: " form-name))

                   (lambda (src . rest) (error "Unknown compiled form: " form-name))

                   coc named
   )
)


;; IMPORTANT: Neither compile nor any of the rewriting/compiling forms
;; it calls should never return #f.


;;
;; Note: this function returns always either a list of level0-code
;; (if in I-mode)
;; or a symbol generated for the wirename. (in E-mode).
;;
;; The created comsig-structure can in latter case be fetched with:
;; (coc-find-comsig-by-wirename coc comsig-wirename)
;;
;; (because it was just physically added to the front of coc-wiredefs list).
;;

;; IMPORTANT - XXX: Maybe we SHOULD NOT allow using any of the reserved
;; keywords like let*, cond, etc. as a loop label.
;; (Even although MIT Scheme 7.7 allows that, and even we allow it now.)

(define (compile src coc named)
    (cond

;; Syntactically expanded forms: cond, let, let*, seq, par, etc.
;; Not anymore handled here:
;;        ((compile-dispatch-if-rewritten-form src coc named))

;; Everything else: let, if, seq-if, seq2, par2, (lambda), etc..
          ((dispatch-to-generic-form src coc
                     (coc-list-of-compiled-forms-in-use coc) named
          ))

          (else
             (error "compile: We shouldn't be here! src=" src)
          )
    ) ;; outer cond
)


;; :_n== is our "multiple assignment" operator, which is rewritten
;; to a series of Verilog <= assignments:

;;
;; If wirename is a register (as often is, in the loop ending branches)
;; (or a combinational function of two registers, say (+ a b))
;; then it is already ready at the same clock cycle as the
;; test-expression which evaluated true on this branch,
;; and the assignments are performed immediately.

;; XXX - Todo!!!: Also special forms like par2, and, and-s
;; should be valid in a final (result-returning) branch of a loop.



;; Note: later we have to give names to all expressions
;; at the assignments, at each branch of the tail-recursive loops,
;; so that we can refer to their ready-signals. This is not enough:

;; Now we give names to assignments (using branch code),
;; and wrap with an if-statement checking for logand of their ready-signals.
;; (XXX - Todo, optimize unnecessary wires off. Too much clutter, maybe?)

(define-compiled-form ((regular 0.1 draft) <loop-tail-call> src coc name)
   (let* ((destlooplabel (t-u-first src))

          (varname-base (coc-form-branch-label coc))

          (loopstructure (cdr (assq destlooplabel (coc-labels2loops coc))))

          (subs-compiled (compile-unnamed-exprs-in-emode coc (t-carglist src)))

          (wait-for (readyexpr-from-subexprs subs-compiled coc))

         )

      (if (coc-imod coc) ;; Still in iterative mode? (Hope so...)

          (expr-wait-wrapped 
               wait-for
               `(:_n==
                     ,(loop-state-name loopstructure) ST_LOOP_RESTARTED
                     ,@(append-map!
                         (lambda (loopreg subexpr)
                            (list (comsig-wirename loopreg)
                                  subexpr
                            )
                         )
                         (loop-registers loopstructure)
                         subs-compiled
                       )
                )
          )

          ((coc-error-printer coc)
               (format #f
 "compile: Cannot call/jump to label ~s in expressive mode: " destlooplabel
               )
               src
          )
      )
   )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coc-dbg-dump-wiredefs coc)
   (if (coc-dbg-show-wiredefs? coc)
       (for-each (lambda (cs)
                    (cond ((pair? cs) (comsig-format #t (cdr cs)))
                          (else (format #t "~s\n" cs))
                    )
                 )
                 (reverse (coc-wiredefs coc))
       )
   )
)



(define (toc-compile-fundef toc fullname src fis)
   (let ((compiled-code
             (compile-fundef-to-level0-code toc fullname src fis)
        ))
     (call-with-output-file (toc-fullname->out-module-name toc fullname)
       (lambda (out)
          (level0code->verilog compiled-code out
                              *BREAM-CALLING-CONVENTION*
          )
       )
     ) 
   )
)


;; compile-fundef will return a list of Level0-code containing
;; a module definition for the function whose definition is given
;; in src.


(define (compile-fundef-to-level0-code toc fullname src fis)
  (generate-uninterned-symbol 0) ;; Clear the counter.
  (let* ((deftype (t-u-first src))
         (toplevel-call? (eq? deftype '<define-toplevel-call>))
         (name_et_fargs (t-second src))
         (fargs (t-rest name_et_fargs))
         (coc (toc-make-fresh-coc toc))
         (body (body-with-no-implicit-progn (cdr (t-rest src))
                                            coc
                                            "function body"
               )
         )
         (reswidth (t-width body)) ;; Should be filled by type-resolver.
         (initial-bindings (coc-add-func-args-to-sigdefs coc fargs))
         (body-sig-name (compile-with-more-bindings body coc
                                                    'funbody
                                                    initial-bindings
                        )
         )
        )
    (begin
      (attach! fullname (toc-already-compiled-items (coc-parent-toc coc)))
      (coc-dbg-dump-wiredefs coc)
      (let* ((body-ready-expr (get-readyexpr body-sig-name coc))

;; These must be _AFTER_ we have compiled the bodycode:
;; 
             (auxregs-to-be-reset
                       (coc-pop-whole-list-of-auxregs-to-be-reset! coc)
             )

             (auxresets (append-map! (lambda (regname) (list regname 0))
                                            auxregs-to-be-reset
                        )
             )

             (extras-for-waiting-state (coc-pop-whole-list-of-extras! coc))

             (param-defs (level0-param-defs coc))
             (wire-and-reg-defs (level0-all-defs coc))
            )

              `(:_module ,fullname ((input clk)
                                    (input start)
                                    ,@(level0-funargs coc)
                                    ,@(if (not toplevel-call?)
                                          `((output (,reswidth result)))
                                          (list)
                                      )
                                    (output result_ready)
                                   )

                    (:_blockcomment ,@(module-info-text (funinfo-moi fis)))

                    (:_blockcomment (Parameter definitions: ,(length param-defs)))
                    ,@param-defs

                    (:_blockcomment (Wire and register definitions: ,(length wire-and-reg-defs)))
                    ,@wire-and-reg-defs

                    ,@(level0-noncomb-instantations coc)

                    ,@(if (not toplevel-call?)
                          `((:_assign result ,body-sig-name))
                          (list)
                      )

                    (:_assign result_ready
                            (bitand ,body-ready-expr (bitnot start))
                    )

;; If start-signal is raised, then regardless of what state we are in,
;; we reset all the auxiliary registers and go to ST_FUN_COMPUTING.
;; The startsignal here is the starting signal of the _whole function_.

                    ,@(if (or (not (null? auxresets))
                              (not (null? extras-for-waiting-state))
                          )
                          `((:_always ((posedge clk))
                               (:_if start
                                  (:_begin (:_n== ,@auxresets))
                                  (:_begin ,@extras-for-waiting-state)
                               )
                           ))
                          (list) ;; Otherwise nil, nothing!
                      )
               ) ;; module end.

      )
    )
  )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (comsig-width-checked cs)
   (or (comsig-width cs)
       (error
           (format #f
"Could not infer the width of signal ~s. Please add explicit width-annotation for it."
                   (comsig-wirename cs)
           )
       )
   )
)


(define (level0-param-def param-def) (cons ':_parameter param-def))

(define (level0-param-defs coc)
    (map level0-param-def (delete '(()) (reverse (coc-paramdefs coc))))
)


(define (level0-reg-or-wire-def cs)
   (if (eq? 'reg (comsig-sigtype cs))
       (level0-reg-def cs)
       (level0-wire-def cs)
   )
)

(define (level0-wire-def cs)
   (if (equal? 1 (comsig-width cs))
       `(:_wire ,(comsig-wirename cs)
                 ,@(if (comsig-combcode cs)
                       (list (comsig-combcode cs))
                       (list)
                   )
        )

       `(:_wire (,(comsig-width-checked cs)
                  ,(comsig-wirename cs)
                  ,@(if (comsig-combcode cs)
                        (list (comsig-combcode cs))
                        (list)
                    )
                )
        )
   )
)


(define (level0-reg-def cs)
   (if (equal? 1 (comsig-width cs))
       `(:_reg ,(comsig-wirename cs)
               ,@(if (comsig-reginit cs)
                     (list (comsig-reginit cs))
                     (list)
                 )
        )

       `(:_reg (,(comsig-width-checked cs)
                ,(comsig-wirename cs)
                ,@(if (comsig-reginit cs)
                      (list (comsig-reginit cs))
                      (list)
                  )
               )
        )
   )
)



(define (level0-wire-def-for-ready-expr cs)
  (if (comsig-readyexpr-constant? cs)
      (list) ;; We don't need to print anything for this one!

      (case (comsig-uses_ready_wire cs)
        ((#f) (list)) ;; Doesn't use, return nil, nada, nothing, tipota.
        ((#t) `(:_wire ,(cs-wirename_ready cs))) ;; No initialization.
        (else `(:_wire ,(cs-wirename_ready cs) ,(comsig-uses_ready_wire cs)))
      )
  )
)

(define (level0-all-defs coc)
   (append-map!
        (lambda (cs)
            (cond ((not (comsig? cs)) (list))
                  ((case (comsig-sigtype cs)

                     ((reg wire)
                       (delete '()
                         (append
                            (list (level0-reg-or-wire-def cs)
                                  (level0-wire-def-for-ready-expr cs)
                            )
                         )
                       )
                     )

                     ((wire-input wire-inout wire-output)
                       (list) ;; Skip these at this time!
                     )

                     (else ;, E.g. 'lit ? We should have cleared them already!
                       ((coc-warning-printer coc)
                          (format #f
"level0-wire-defs: there should not be anymore comsigs of type ~s present in wiredefs list: "
                                  (comsig-sigtype cs)
                          )
                          (comsig-format #f cs)
                       )
                       (list)
                     )
                  ))
            )
        )

        (map cdr (reverse (coc-wiredefs coc)))
   )
)


(define (level0-noncomb-instantations coc)
   (map comsig-noncombcode
        (keep-matching-items (map cdr (reverse (coc-wiredefs coc)))
                             comsig-itself-noncombinational?
        )
   )
)


(define (comsig-wire-funarg? cs)
 (and (comsig? cs)
      (case (comsig-sigtype cs)
         ((wire-input)  `(input  (,(comsig-width cs) ,(comsig-wirename cs))))
         ((wire-output) `(output (,(comsig-width cs) ,(comsig-wirename cs))))
         ((wire-inout)  `(inout  (,(comsig-width cs) ,(comsig-wirename cs))))
         (else #f)
      )
 )
)


(define (level0-funargs coc)
  (map comsig-wire-funarg?
       (keep-matching-items (map cdr (reverse (coc-wiredefs coc)))
                            comsig-wire-funarg?
       )
  )
)




;;;;;;;;;;;;;;;;;;;;;;;;;
