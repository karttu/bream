
module gcd_once_unrolled_18_18_18(
        input clk,
        input start,
        input [17:0] a,
        input [17:0] b,
        output [17:0] result,
        output result_ready);

/*

;;
;; intfuns-untested.brm.scm -- A collection of integer functions in Scheme,
;; which are yet to be "breamified" and tested.
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
;; Note: After founding that Bream compiles a function correctly, and
;; that it works OK in real FPGA, one should move its definition then
;; either to
;;   intbasic.brm.scm
;; if it is often needed function implemented reasonably, or, to
;;   intfuns-nonpractical.brm.scm
;; if it is never meant for serious use, but is otherwise useful for testing.
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
(wire and register definitions: 19)
*/

reg [1:0] loop_loop_state = st_loop_ready;
wire loop_loop_restarted = (st_loop_restarted == loop_loop_state);
wire loop_loop_waiting = (st_loop_waiting == loop_loop_state);
reg [17:0] a_2;
reg [17:0] b_3;
wire if_test_4 = (~| b_3);
wire if_test_5 = (a_2 >= b_3);
wire [17:0] b_6 = (a_2 - b_3);
wire if_test_7 = (~| b_6);
wire if_test_8 = (b_3 >= b_6);
wire [17:0] minus_9 = (b_3 - b_6);
wire [17:0] minus_10 = (b_6 - b_3);
wire [17:0] a_11 = (b_3 - a_2);
wire if_test_12 = (~| a_2);
wire if_test_13 = (a_11 >= a_2);
wire [17:0] minus_14 = (a_11 - a_2);
wire [17:0] minus_15 = (a_2 - a_11);
reg [17:0] loop_loop_result_1;
wire loop_loop_result_1_ready = (loop_loop_state == st_loop_ready);
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
                    a_2 <= a;
                    b_3 <= b;
                end
            st_loop_restarted: 
                begin
                    loop_loop_state <= st_loop_waiting;
                end
            st_loop_waiting: 
                begin
                    if (if_test_4) 
                        begin
                            loop_loop_result_1 <= a_2;
                            loop_loop_state <= st_loop_ready;
                        end
                    else
                        if (if_test_5) 
                            if (if_test_7) 
                                begin
                                    loop_loop_result_1 <= b_3;
                                    loop_loop_state <= st_loop_ready;
                                end
                            else
                                if (if_test_8) 
                                    begin
                                        loop_loop_state <= st_loop_restarted;
                                        a_2 <= b_6;
                                        b_3 <= minus_9;
                                    end
                                else
                                    begin
                                        loop_loop_state <= st_loop_restarted;
                                        a_2 <= minus_10;
                                        b_3 <= b_3;
                                    end
                        else
                            if (if_test_12) 
                                begin
                                    loop_loop_result_1 <= a_11;
                                    loop_loop_state <= st_loop_ready;
                                end
                            else
                                if (if_test_13) 
                                    begin
                                        loop_loop_state <= st_loop_restarted;
                                        a_2 <= a_2;
                                        b_3 <= minus_14;
                                    end
                                else
                                    begin
                                        loop_loop_state <= st_loop_restarted;
                                        a_2 <= minus_15;
                                        b_3 <= a_11;
                                    end
                end
        endcase
end

assign result = loop_loop_result_1;
assign result_ready = (loop_loop_result_1_ready & (~ start));
endmodule
