
module a000010_naive_par_aux_18_18_18_18(
        input clk,
        input start,
        input [17:0] n,
        input [17:0] start_offset,
        input [17:0] increment,
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
(wire and register definitions: 39)
*/

reg [1:0] loop_loop_state = st_loop_ready;
wire loop_loop_restarted = (st_loop_restarted == loop_loop_state);
wire loop_loop_waiting = (st_loop_waiting == loop_loop_state);
wire [17:0] plus_2 = (start_offset + 18'd1);
wire [17:0] plus_3 = (start_offset + 18'd1);
reg [17:0] prev_i_4;
reg [17:0] i_5;
reg [17:0] s_6;
wire gt_7 = (i_5 > n);
wire lt_8 = (i_5 < prev_i_4);
wire if_test_9 = (gt_7 || lt_8);
wire [17:0] a_10;
wire a_10_ready;
wire [17:0] plus_12 = (i_5 + increment);
wire bit_13 = (a_10 [0]);
wire [16:0] drop_14 = (a_10 [17:1]);
wire rednor_15 = (~| drop_14);
wire a_16 = (bit_13 & rednor_15);
wire a_16_ready = (a_10_ready & a_10_ready);
wire [1:0] conc2_17 = ({ 1'd0 , a_16 });
wire [2:0] conc2_18 = ({ 1'd0 , conc2_17 });
wire [3:0] conc2_19 = ({ 1'd0 , conc2_18 });
wire [4:0] conc2_20 = ({ 1'd0 , conc2_19 });
wire [5:0] conc2_21 = ({ 1'd0 , conc2_20 });
wire [6:0] conc2_22 = ({ 1'd0 , conc2_21 });
wire [7:0] conc2_23 = ({ 1'd0 , conc2_22 });
wire [8:0] conc2_24 = ({ 1'd0 , conc2_23 });
wire [9:0] conc2_25 = ({ 1'd0 , conc2_24 });
wire [10:0] conc2_26 = ({ 1'd0 , conc2_25 });
wire [11:0] conc2_27 = ({ 1'd0 , conc2_26 });
wire [12:0] conc2_28 = ({ 1'd0 , conc2_27 });
wire [13:0] conc2_29 = ({ 1'd0 , conc2_28 });
wire [14:0] conc2_30 = ({ 1'd0 , conc2_29 });
wire [15:0] conc2_31 = ({ 1'd0 , conc2_30 });
wire [16:0] conc2_32 = ({ 1'd0 , conc2_31 });
wire [17:0] conc2_33 = ({ 1'd0 , conc2_32 });
wire [17:0] plus_34 = (s_6 + conc2_33);
reg [17:0] loop_loop_result_1;
wire loop_loop_result_1_ready = (loop_loop_state == st_loop_ready);

gcd_once_unrolled_18_18_18 gcd_once_unrolled_11(
        .clk(clk),
        .start(loop_loop_restarted),
        .a(n),
        .b(i_5),
        .result_ready(a_10_ready),
        .result(a_10));


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
                    prev_i_4 <= plus_2;
                    i_5 <= plus_3;
                    s_6 <= 18'd0;
                end
            st_loop_restarted: 
                begin
                    loop_loop_state <= st_loop_waiting;
                end
            st_loop_waiting: 
                begin
                    if (if_test_9) 
                        begin
                            loop_loop_result_1 <= s_6;
                            loop_loop_state <= st_loop_ready;
                        end
                    else
                        begin
                            if (a_16_ready) 
                                begin
                                    loop_loop_state <= st_loop_restarted;
                                    prev_i_4 <= i_5;
                                    i_5 <= plus_12;
                                    s_6 <= plus_34;
                                end
                        end
                end
        endcase
end

assign result = loop_loop_result_1;
assign result_ready = (loop_loop_result_1_ready & (~ start));
endmodule
