
;;
;; bream/lev0veri.scm - module which maps "Level0"-code to Verilog.
;;

;;
;; Unless otherwise mentioned, all the files in this code tree are
;; Copyright (C) 2010-2011 Antti Karttunen, subject to the terms of the GPL v2.
;; See the file COPYING for more information.  Contact the author for
;; further developments at the address: <his-firstname>.<his-surname>@gmail.com
;;

;;
;; Antti Karttunen ("karttu") started writing this module Sep 11-12 2010.
;;
;; This module implements "Level0" hardware description language,
;; which offers essentially simple one-to-one mapping from more Scheme-friendly
;; S-expressionified syntax to a synthesizable subset of Verilog.
;;
;; Many of the forms are based on Faulkner's Verilisp language,
;; available from:
;; http://home.comcast.net/~faulkner612/programming/verilisp/index.html
;;
;;
;;

;;
;; Edited    Sep 15 2010 by karttu.
;;   Factored some code. Added case and function.
;;   Replaced all "V_" with ":_".
;;

;;
;; Edited    Sep 18 2010 by karttu.
;;   Uses now generic dispatching code from dispatch.scm
;;   (Argument order of forms changed from (oc i src) to (src oc i)
;;
;; Edited    Oct 28 2010 by karttu.
;;   Added one more kludgous argument for generic-dispatch.
;;
;; Edited    May 25 2011 by karttu.
;;   Added the current year to the copyright notice.
;;
;; Edited    May 29 2011 by karttu.
;;   Changed all instances of 'conbits' to 'conc' and 'conc2'.
;;
;; Edited    Jun 02 2011 by karttu.
;;   Added the missing "endcase" to the case.
;;   Made blockcomment NOP, in case it is given no argument at all.
;;
;; Edited    Aug 20 2011 by karttu.
;;   Now blockcomment checks also whether cdr is #f.
;;
;; Edited    Aug 23 2011 by karttu.
;;   Added discard-d-width-spec-if-present so that there will be no
;;   unnecessary decimal-specifiers among range-limits of bits.
;;
;; Edited    Aug 25 2011 by karttu.
;;   Well, added it also to converters for bit and ref.
;;
;; Edited    Aug 26 2011 by karttu.
;;   Added *c -> "*" mapping. Ditto for *c2.
;;

(define (level0code->verilog level0code output-port calling-convention)
  (let ((oc (create-oc calling-convention output-port 4)))
     (dispatch-to-level0-form level0code oc 0)
  )
)



(define *level0-forms*
   (list (list '(regular 0.1 draft) (list (list (list))))
         (list '(discarded 0.1) (list (list (list))))
   )
)


(define-structure (oc (keyword-constructor) (copier))
    calling-convention-in-use

    level0-forms

    out

    ic ;; Indentation Constant, usually 4 or 8.

    nli-write-out
    nli-write-out-nl
    nli-write-string
    nli-write-string-nl

    write-out
    write-string

    write-strings
    write-strings-with-spaces
)


(define (out-indent n-blanks outport)
    (if (> n-blanks 0)
        (begin (write-string " " outport)
               (out-indent (- n-blanks 1) outport)
        )
    )
)


;; (create-oc *BREAM-CALLING-CONVENTION* (current-output-port) 4)

(define (create-oc caco outport indentation-constant)
    (make-oc 'calling-convention-in-use caco
             'level0-forms *level0-forms*
             'out outport
             'ic indentation-constant

             'nli-write-out
                (lambda (oc i s)
                   (begin
                      (newline (oc-out oc))
                      (out-indent (* i (oc-ic oc)) (oc-out oc))
                      (write s (oc-out oc))
                   )
                )

             'nli-write-out-nl
                (lambda (oc i s)
                   (begin
                      (newline (oc-out oc))
                      (out-indent (* i (oc-ic oc)) (oc-out oc))
                      (write s (oc-out oc))
                      (newline (oc-out oc))
                   )
                )

             'nli-write-string
                (lambda (oc i s)
                   (begin
                      (newline (oc-out oc))
                      (out-indent (* i (oc-ic oc)) (oc-out oc))
                      (write-string s (oc-out oc))
                   )
                )

             'nli-write-string-nl
                (lambda (oc i s)
                   (begin
                      (newline (oc-out oc))
                      (out-indent (* i (oc-ic oc)) (oc-out oc))
                      (write-string s (oc-out oc))
                      (newline (oc-out oc))
                   )
                )

;; Similar, but without any preceding newlines or indentation:

             'write-out (lambda (oc s) (write s (oc-out oc)))

             'write-string (lambda (oc s) (write-string s (oc-out oc)))

             'write-strings
                (lambda (oc strings)
;;                 (for-each (lambda (s) (write-string s (oc-out oc))) strings)
                   (for-each (lambda (s) (format (oc-out oc) "~a" s)) strings)
                )

;; That is, a list of strings or whatever, separated by spaces.
             'write-strings-with-spaces
                (lambda (oc strings)
                   (let loop ((strings strings))
                      (cond ((not (null? strings))
                               (format (oc-out oc) "~a" (car strings))
                               (if (not (null? (cdr strings)))
                                   ((oc-write-string oc) oc " ")
                               )
                               (loop (cdr strings))
                            )
                      )
                   )
                )

    )
)



(define (oc-list-of-level0-forms-in-use oc)
  (list-of-forms-by-dic (oc-calling-convention-in-use oc)
                        (oc-level0-forms oc)
  )
)



(define (dispatch-to-level0-form src oc i)
   (generic-dispatch src

                     src

                     (oc-list-of-level0-forms-in-use oc)

                     first

                     (lambda (x) x)

                     (lambda (src oc i) ((oc-write-out oc) oc src))

                     (lambda (src oc i) (format (oc-out oc) "~a" (L0->V src)))

                     oc i
   )
)


(define-syntax define-level0-form
   (syntax-rules ()
     ((define-level0-form (CACO formname src oc i) body ...)
        (attach!
             (cons (quote formname) (lambda (src oc i) (begin body ...)))
             (list-of-forms-by-dic (quote CACO) *level0-forms*)
        )
     )
   )
)



(define-level0-form ((regular 0.1 draft) :_begin src oc i)
    ((oc-nli-write-string oc) oc i "begin")
    (for-each (lambda (lev0expr)
                      (dispatch-to-level0-form lev0expr oc (1+ i))
              )
              (cdr src)
    )
    ((oc-nli-write-string oc) oc i "end")
)

;; oc-nli-write-out
;; oc-nli-write-out-nl
;; oc-nli-write-string
;; oc-nli-write-string-nl
;;  all indent, after they have printed the first newline.
;; ("nli" = newline + indentation.)
;;
;; oc-write-* forms never indent.

;; This version of if embeds the whole if-statement in begin/end
;; block if there's no else-clause, as otherwise the possible
;; else-clause of a surrounding if would match incorrectly with this if:

(define-level0-form ((regular 0.1 draft) :_if src oc i)
   (let* ((test-expr (second src))
          (then-expr (third src))
          (opt-else-expr (cdddr src))
          (j (if (null? opt-else-expr) (1+ i) i))
         )

      (if (null? opt-else-expr) ((oc-nli-write-string oc) oc i "begin"))

      ((oc-nli-write-string oc) oc j "if (")
      (dispatch-to-level0-form test-expr oc (1+ j))
      ((oc-write-string oc) oc ") ")
      (dispatch-to-level0-form then-expr oc (1+ j))

      (if (null? opt-else-expr)
          ((oc-nli-write-string oc) oc i "end")
          (begin ;; else...
               ((oc-nli-write-string oc) oc j "else")
               (dispatch-to-level0-form (car opt-else-expr) oc (1+ j))
          )          
      )
   )
)


(define-level0-form ((discarded 0.1) :_if src oc i)
   (let ((test-expr (second src))
         (then-expr (third src))
         (opt-else-expr (cdddr src))
        )

      ((oc-nli-write-string oc) oc i "if (")
      (dispatch-to-level0-form test-expr oc (1+ i))
      ((oc-write-string oc) oc ") ")
      (dispatch-to-level0-form then-expr oc (1+ i))
      ((oc-nli-write-string oc) oc i "else")
      (cond ((not (null? opt-else-expr))
                 (dispatch-to-level0-form (car opt-else-expr) oc (1+ i))
            )
            (else (begin ;; Important! Have to add an empty begin end block
;; to else clause, if no else-part, as otherwise the next else-statement
;; will be matched with a wrong if:
                    ((oc-nli-write-string oc) oc (1+ i) "begin")
                    ((oc-nli-write-string oc) oc (1+ i) "end")
                  )
            )
      )
   )
)

;; Verilisp has:
;;
;; (case a
;;   (1 (display 1))
;;   (2 (display 2))
;;   (default (display 0))
;; )
;;
;; We allow also ((1 2) ...), i.e. multiple case-expressions in the
;; "header" of one case-item. (Because Verilog, like C, allows that.)

(define-level0-form ((regular 0.1 draft) :_case src oc i)
   (let ((test-expr (second src))
         (case-items (cddr src))
        )

      ((oc-nli-write-string oc) oc i "case (")
      (dispatch-to-level0-form test-expr oc (1+ i))
      ((oc-write-string oc) oc ")")

      (for-each
          (lambda (case_item)
             (let ((expr-or-exprs (first case_item)))
                ((oc-nli-write-string oc) oc (1+ i) "")
                (if (pair? expr-or-exprs)
                    (let loop ((es expr-or-exprs))
                       (if (pair? es)
                           (begin
                              (dispatch-to-level0-form (first es) oc (1+ i))
                              (if (not (null? (cdr es)))
                                  ((oc-write-string oc) oc ", ")
                              )
                              (loop (cdr es))
                           )
                       )
                    )
                    ((oc-write-out oc) oc expr-or-exprs)
                )
                ((oc-write-string oc) oc ": ")
;; No implicit "begin" here either. Use :_begin, if you need multiple stmts!
                (dispatch-to-level0-form (second case_item) oc (+ 2 i))
             )
          )
          case-items
      )

      ((oc-nli-write-string oc) oc i "endcase")

   )
)



(define-level0-form ((regular 0.1 draft) :_n= src oc i)
   (let ((regname (second src))
         (assignexpr (third src))
        )

      ((oc-nli-write-out oc) oc i regname)
      ((oc-write-string oc) oc " <= ")
      (dispatch-to-level0-form assignexpr oc (1+ i))
      ((oc-write-string oc) oc ";")
   )
)

;; Multiple assignments at once: (or even none!)

(define-level0-form ((regular 0.1 draft) :_n== src oc i)
   ((oc-nli-write-string oc) oc i "begin")
   (let loop ((assignments (cdr src)))
     (cond ((not (null? assignments))
              (dispatch-to-level0-form
                     `(:_n= ,(first assignments) ,(second assignments))
                      oc (1+ i)
              )
              (loop (cddr assignments))
           )
     )
   )
   ((oc-nli-write-string oc) oc i "end")
)


(define-level0-form ((regular 0.1 draft) :_instantiate src oc i)
   (let ((module-name (second src))
         (instantation-name (third src))
        )

     (newline (oc-out oc))
     ((oc-nli-write-out oc) oc i module-name)
     ((oc-write-string oc) oc " ")
     ((oc-write-out oc) oc instantation-name)
     ((oc-write-string oc) oc "(")

     (let loop ((named-args (cdddr src)))
        (cond ((not (null? named-args))
                 (begin
                    ((oc-nli-write-string oc) oc (+ i 2) ".")

                    ((oc-write-strings oc) oc
                            (list (first (first named-args)) "(")
                    )

                    (dispatch-to-level0-form (second (first named-args))
                                             oc (+ i 2)
                    )

                    ((oc-write-string oc) oc ")")
                    (if (not (null? (cdr named-args)))
                        ((oc-write-string oc) oc ",")
                        (begin
                          ((oc-write-string oc) oc ");")
                          (newline (oc-out oc))
                        )
                    )
                    (loop (cdr named-args))
                 )
              )
           )
     )
   )
   (newline (oc-out oc))
)


(define (write-width-specifier oc width)
    (if (not (eq? 1 width))
        ((oc-write-strings oc) oc (list "[" (- width 1) ":0] "))
    )
)


(define-level0-form ((regular 0.1 draft) input src oc i)
 (let ((wirename (if (pair? (second src)) (second (second src)) (second src)))
       (width (if (pair? (second src)) (first (second src)) 1))
      )

      ((oc-nli-write-string oc) oc i "input ")
      (write-width-specifier oc width)
      ((oc-write-out oc) oc wirename)
   )
)


(define-level0-form ((regular 0.1 draft) output src oc i)
 (let ((wirename (if (pair? (second src)) (second (second src)) (second src)))
       (width (if (pair? (second src)) (first (second src)) 1))
      )

      ((oc-nli-write-string oc) oc i "output ")
      (write-width-specifier oc width)
      ((oc-write-out oc) oc wirename)
   )
)


(define-level0-form ((regular 0.1 draft) inout src oc i)
 (let ((wirename (if (pair? (second src)) (second (second src)) (second src)))
       (width (if (pair? (second src)) (first (second src)) 1))
      )

      ((oc-nli-write-string oc) oc i "inout ")
      (write-width-specifier oc width)
      ((oc-write-out oc) oc wirename)
   )
)


(define-level0-form ((regular 0.1 draft) :_module src oc i)
   (let ((modulename (second src))
         (statements (cdddr src))
        )

      ((oc-nli-write-string oc) oc i "module ")
      ((oc-write-out oc) oc modulename)
      ((oc-write-string oc) oc "(")

      (let loop ((inp-out-params (third src)))
         (cond ((not (null? inp-out-params))
                  (begin
                    (dispatch-to-level0-form (first inp-out-params) oc (+ i 2))
                    (if (not (null? (cdr inp-out-params)))
                        ((oc-write-string oc) oc ",")
                        ((oc-write-string oc) oc ");")
                    )
                    (loop (cdr inp-out-params))
                  )
                )
         )
      )

      (newline (oc-out oc))
      (for-each (lambda (lev0expr)
                        (dispatch-to-level0-form lev0expr oc i)
                )
                statements
      )
      ((oc-nli-write-string-nl oc) oc i "endmodule")
   )
)

;;
;; Our syntax:
;;
;; (function [opt width-integer] myfunc (a (2 b c) d)
;;    expression
;; )
;;
;; Try:
;; (level0code->verilog '(:_function life (c n ne s se w sw e nw)
;;                           (== (b 4 0011)
;;                               (bitor (conc (b 3 000) c)
;;                                      (+ (conc (b 3 000) n)
;;                                         (conc (b 3 000) ne)
;;                                         (conc (b 3 000) e)
;;                                         (conc (b 3 000) se)
;;                                         (conc (b 3 000) s)
;;                                         (conc (b 3 000) sw)
;;                                         (conc (b 3 000) w)
;;                                         (conc (b 3 000) nw)
;;                                      )
;;                               )
;;                           )
;;                       )
;;                       (current-output-port)
;;                       *BREAM-CALLING-CONVENTION*
;; )
;; 
;; Or:
;;
;; (level0code->verilog
;;  '(:_function 2 add_mod3 ((2 a) (2 b))
;;       (?: (> (+ a b) 5) (- (+ a b) 6)
;;           (?: (> (+ a b) 2) (- (+ a b) 3) (+ a b))
;;       )
;;   )
;;   (current-output-port)
;;   *BREAM-CALLING-CONVENTION*
;; )
;; 
;; function [1:0] add_mod3;
;;     input [1:0] a;
;;     input [1:0] b;
;; 
;;     add_mod3 = (((a + b) > 5) ? ((a + b) - 6) : (((a + b) > 2) ? ((a + b) - 3) : (a + b)));
;; endfunction
;; 
;;



(define-level0-form ((regular 0.1 draft) :_function src oc i)
  (let* ((reswidth (if (integer? (second src)) (second src) 1))
         (rest-of (if (integer? (second src)) (cddr src) (cdr src)))
         (funcname (first rest-of))
         (inpsignals (second rest-of))
         (expr (third rest-of))
        )

      ((oc-nli-write-string oc) oc i "function ")
      (write-width-specifier oc reswidth)
      ((oc-write-out oc) oc funcname)
      ((oc-write-string oc) oc ";")

      (let loop ((inpsignals inpsignals))
         (if (not (null? inpsignals))
             (let ((signame (if (pair? (car inpsignals))
                                (second (car inpsignals))
                                (car inpsignals)
                            )
                   )
                   (sigwidth (if (pair? (car inpsignals))
                                 (first (car inpsignals))
                                 1
                             )
                   )
                  )

                 (write-declaration oc (1+ i) "input" signame sigwidth #f)

                 (loop (cdr inpsignals))
             )
         )
      )

      (newline (oc-out oc))

;; Then write out funcname = expr;
      (write-declaration oc (1+ i) #f funcname 1 expr)

      ((oc-nli-write-string oc) oc i "endfunction")
      (newline (oc-out oc))
   )
)




;; (:_always ((negedge clk)) ...) --> always @(negedge clk) begin ... end
;; always @(posedge clk or posedge wb_rst_i) 

(define-level0-form ((regular 0.1 draft) :_always src oc i)
   (let ((signals (second src))
         (statements (cddr src))
        )

      ((oc-nli-write-string oc) oc i "always @(")

      (let loop ((signals (second src)))
         (cond ((not (null? signals))
                  (begin
                    ((oc-write-strings-with-spaces oc) oc
                                 (if (pair? (first signals))
                                     (first signals)
                                     (list (first signals))
                                 )
                    )
                    (if (not (null? (cdr signals)))
                        ((oc-write-string oc) oc " or ")
                        ((oc-write-string oc) oc ")")
                    )
                    (loop (cdr signals))
                  )
               )
         )
      )

      ((oc-nli-write-string oc) oc (+ i 0) "begin")
      (for-each (lambda (lev0expr)
                        (dispatch-to-level0-form lev0expr oc (+ i 1))
                )
                statements
      )
      ((oc-nli-write-string-nl oc) oc (+ i 0) "end")
   )
)



(define (write-declaration oc i sigtype signame width opt-assign)
   (begin
      (if sigtype
          (begin
            ((oc-nli-write-string oc) oc i sigtype)
            ((oc-write-string oc) oc " ")
          )
          ((oc-nli-write-string oc) oc i "") ;; For function
      )
      (write-width-specifier oc width)
      ((oc-write-out oc) oc signame)
      (if (and opt-assign (not (null? opt-assign)))
          (begin
             ((oc-write-string oc) oc " = ")
             (dispatch-to-level0-form opt-assign oc (1+ i))
          )
      )
      ((oc-write-string oc) oc ";")
   )
)

(define-level0-form ((regular 0.1 draft) :_wire src oc i)
 (let ((wirename (if (pair? (second src)) (second (second src)) (second src)))
       (width (if (pair? (second src)) (first (second src)) 1))
       (opt-assign (if (pair? (second src)) (cddr (second src)) (cddr src)))
      )
     (write-declaration oc i "wire" wirename width
            (if (pair? opt-assign) (car opt-assign) opt-assign)
     )
 )
)


(define-level0-form ((regular 0.1 draft) :_reg src oc i)
 (let ((regname (if (pair? (second src)) (second (second src)) (second src)))
       (width (if (pair? (second src)) (first (second src)) 1))
       (opt-assign (if (pair? (second src)) (cddr (second src)) (cddr src)))
      )
     (write-declaration oc i "reg" regname width
            (if (pair? opt-assign) (car opt-assign) opt-assign)
     )
 )
)


(define-level0-form ((regular 0.1 draft) :_assign src oc i)
   (let ((wirename (second src))
         (assignexpr (third src))
        )
     (write-declaration oc i "assign" wirename 1 assignexpr)
   )
)


(define-level0-form ((regular 0.1 draft) :_parameter src oc i)
   (let ((paramname (second src))
         (assignexpr (third src))
        )
     (write-declaration oc i "parameter" paramname 1 assignexpr)
   )
)



(define-level0-form ((regular 0.1 draft) :_blockcomment src oc i)
  (if (and (cdr src) (> (length src) 1))
      (begin
        ((oc-nli-write-string oc) oc i "/*")

        (for-each
          (lambda (s)
            ((if (string? s) (oc-nli-write-string oc)  (oc-nli-write-out oc))
                oc i s ;; Don't indent!
            )
          )
          (cdr src)
        )

        ((oc-nli-write-string-nl oc) oc i "*/")
      )
  )
)



(define (interleave f rest)
    (cond ((or (null? rest) (null? (cdr rest))) rest)
          (else (cons (first rest) (cons f (interleave f (cdr rest)))))
    )
)

(define (prefix->infix-deeply src)
   (if (not (pair? src))
       src
       (let ((f (first src))
             (args (cdr src))
            )
         (if (> (length args) 1)
             (interleave f (map prefix->infix-deeply args))
             src
         )
       )
   )
)


(define (prefix->infix src)
   (if (not (pair? src))
       src
       (let ((f (first src))
             (args (cdr src))
            )
         (if (> (length args) 1)
             (interleave f args)
             src
         )
       )
   )
)

(define (discard-d-width-spec-if-present expr)
  (if (and (pair? expr)
           (list? expr)
           (eq? 'd (first expr))
           (> (length expr) 2)
      )
      (third expr) ;; Get int from (d width int)
      expr ;; Otherwise, return it as it was.
  )
)

(define (L0->V expr)
 (if (not (pair? expr)) expr
  (case (car expr)
    ((?:)
     `(,(L0->V (second expr)) ? ,(L0->V (third expr)) : ,(L0->V (fourth expr)))
    )

    ((conc conc2 cat) ;; { } (cat in Verilisp) One or more arguments.
      (cons "{" (append (interleave "," (map L0->V (cdr expr))) (list "}")))
    )

    ((bit ref)
      (list (L0->V (second expr))
            (format #f "[~a]" (discard-d-width-spec-if-present (third expr)))
      )
    )

    ((bits)
      (list (L0->V (second expr))
            (format #f "[~a:~a]"
                    (discard-d-width-spec-if-present (third expr))
                    (discard-d-width-spec-if-present (fourth expr))
            )
      )
    )

    ((d b h o) ;; (b 6 100110) --> 6'b100110
      (format #f "~s'~a~s" (second expr) (first expr) (third expr))
    )

    (else
      (cond ((assoc (car expr) *synonyms->official_Verilog_expressions*)
              =>
               (lambda (lev0.veriname)
                 (prefix->infix
                    (cons (cdr lev0.veriname) (map L0->V (cdr expr)))
                 )
               )
            )
            (else (prefix->infix (cons (car expr) (map L0->V (cdr expr)))))
      )
    )
  )
 )
)




(define *synonyms->official_Verilog_expressions*
        '(
           (lognot . "!")
           (logand . "&&")
           (logor . "||")

           (= . "==")
           (== . "==")
           (!= . "!=")

           (bitnot . "~") ;; Bitwise-negation (complement). Unary

           (bitand  . "&")
           (bitor   . "|")
           (bitxor  . "^")
           (bitxnor . "^~") ;; Or: ~^ ;; Bitwise-xnor, i.e. Bitwise-equal.

           (redand  .  "&") ;; Reduction and
           (rednand . "~&") ;; Reduction nand
           (redor   .  "|") ;; Reduction or
           (rednor  . "~|") ;; Reduction nor
           (redxor  . "^")  ;; Reduction xor
           (redxnor . "^~") ;; Or ~^ ;; Reduction xnor

           (shr     . ">>")  ;; >> Right shift.
           (shl     . "<<")  ;; << Left shift.
           (asr     . ">>>") ;; >>> Arithmetic shift right.
           (asl     . "<<<") ;; <<< Arithmetic shift left.

;; My own additions:
           (+c      . "+")
           (+w      . "+")

           (*c      . "*")
           (*c2     . "*")

           (zero?   . "~|") ;; Alias for rednor, "~|" or for (0==x)
           (0==     . "0 == ") ;;
           (nonzero? . "|") ;; Alias for redor, "|" or for (0!=x)
           (0!=     . "0 != ") ;;
;;         (even?   . "???") ;; Alias for ~(1&x) or (0==(bit x 0))
           (odd?    . "1&")  ;; Alias for (1&x)

           (1+      . "1+")   ;; Alias for 1+
           (-1+     . "(-1)+") ;; Alias for -1
         )
)



