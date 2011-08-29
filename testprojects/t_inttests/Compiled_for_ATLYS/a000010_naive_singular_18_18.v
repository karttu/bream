
module a000010_naive_singular_18_18(
        input clk,
        input start,
        input [17:0] n,
        output [17:0] result,
        output result_ready);

/*

;;
;; intfuns-nonpractical.brm.scm -- A collection of integer functions in Scheme,
;; which work, but can be hardly considered to be optimal, or even reasonable
;; implementation of the mathematical function in question.
;;
;; Copyright (C) 2010-2011 Antti Karttunen, subject to the terms of the GPL v2.
;; See the file COPYING for more information.  Contact the author for
;; further developments at the address: <his-firstname>.<his-surname>@gmail.com
;;
;; Most of these functions I have skimmed from the set of functions I wrote
;; to be executed under MIT/GNU Scheme, which are computing various
;; integer sequences in Sloane's Online-Encyclopedia of Integer Sequences
;; That code is available under the files:
;; http://www.iki.fi/kartturi/matikka/Schemuli/intfuns1.scm.txt
;; http://www.iki.fi/kartturi/matikka/Schemuli/intfun_a.scm.txt
;; http://www.iki.fi/kartturi/matikka/Schemuli/intfun_b.scm.txt
;; (See also  http://oeis.org/ for the definition of each A-number.)
;; Many of those definitions are far from optimal, even if intended
;; to be run under an ordinary CPU-based architecture. The goal has
;; been more to check the various mathematical correspondences,
;; and especially, to get correct results.
;;

*/

/*
(parameter definitions: 4)
*/

parameter st_loop_ready = 0;
parameter st_loop_inits = 1;
parameter st_loop_restarted = 3;
parameter st_loop_waiting = 2;
/*
(wire and register definitions: 34)
*/

reg [1:0] loop_loop_state = st_loop_ready;
wire loop_loop_restarted = (st_loop_restarted == loop_loop_state);
wire loop_loop_waiting = (st_loop_waiting == loop_loop_state);
reg [17:0] i_2;
reg [17:0] s_3;
wire if_test_4 = (~| i_2);
wire [17:0] a_5;
wire a_5_ready;
wire [17:0] sub1_7 = ((-1)+ i_2);
wire bit_8 = (a_5 [0]);
wire [16:0] drop_9 = (a_5 [17:1]);
wire rednor_10 = (~| drop_9);
wire a_11 = (bit_8 & rednor_10);
wire a_11_ready = (a_5_ready & a_5_ready);
wire [1:0] conc2_12 = ({ 1'd0 , a_11 });
wire [2:0] conc2_13 = ({ 1'd0 , conc2_12 });
wire [3:0] conc2_14 = ({ 1'd0 , conc2_13 });
wire [4:0] conc2_15 = ({ 1'd0 , conc2_14 });
wire [5:0] conc2_16 = ({ 1'd0 , conc2_15 });
wire [6:0] conc2_17 = ({ 1'd0 , conc2_16 });
wire [7:0] conc2_18 = ({ 1'd0 , conc2_17 });
wire [8:0] conc2_19 = ({ 1'd0 , conc2_18 });
wire [9:0] conc2_20 = ({ 1'd0 , conc2_19 });
wire [10:0] conc2_21 = ({ 1'd0 , conc2_20 });
wire [11:0] conc2_22 = ({ 1'd0 , conc2_21 });
wire [12:0] conc2_23 = ({ 1'd0 , conc2_22 });
wire [13:0] conc2_24 = ({ 1'd0 , conc2_23 });
wire [14:0] conc2_25 = ({ 1'd0 , conc2_24 });
wire [15:0] conc2_26 = ({ 1'd0 , conc2_25 });
wire [16:0] conc2_27 = ({ 1'd0 , conc2_26 });
wire [17:0] conc2_28 = ({ 1'd0 , conc2_27 });
wire [17:0] plus_29 = (s_3 + conc2_28);
reg [17:0] loop_loop_result_1;
wire loop_loop_result_1_ready = (loop_loop_state == st_loop_ready);

gcd_18_18_18 gcd_6(
        .clk(clk),
        .start(loop_loop_restarted),
        .a(n),
        .b(i_2),
        .result_ready(a_5_ready),
        .result(a_5));


always @(posedge clk)
begin
    if (start) 
        begin
            loop_loop_state <= st_loop_inits;
        end
    else
        case (loop_loop_state)
            st_loop_ready: 
                begin
                end
            st_loop_inits: 
                begin
                    loop_loop_state <= st_loop_restarted;
                    i_2 <= n;
                    s_3 <= 18'd0;
                end
            st_loop_restarted: 
                begin
                    loop_loop_state <= st_loop_waiting;
                end
            st_loop_waiting: 
                begin
                    if (if_test_4) 
                        begin
                            loop_loop_result_1 <= s_3;
                            loop_loop_state <= st_loop_ready;
                        end
                    else
                        begin
                            if (a_11_ready) 
                                begin
                                    loop_loop_state <= st_loop_restarted;
                                    i_2 <= sub1_7;
                                    s_3 <= plus_29;
                                end
                        end
                end
        endcase
end

assign result = loop_loop_result_1;
assign result_ready = (loop_loop_result_1_ready & (~ start));
endmodule
