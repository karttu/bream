
module gcd_18_18_18(
        input clk,
        input start,
        input [17:0] a,
        input [17:0] b,
        output [17:0] result,
        output result_ready);

/*

;;
;; intbasic.brm.scm - The most basic integer functions not provided by Verilog.
;;
;; Copyright (C) 2010-2011 Antti Karttunen, subject to the terms of the GPL v2.
;; See the file COPYING for more information.  Contact the author for
;; further developments at the address: <his-firstname>.<his-surname>@gmail.com
;;
;; This module should contain only implementations of integer functions
;; (both define's and define-wirm's are OK) which have been tested
;; (at least rudimentarily) to work as expected on real FPGA platform.
;; Also, the implementation should be "plausible" in sense, that
;; it gives correct results with a modest footprint of space and time.
;; There is a separate module  intfuns-nonpractical.brm.scm
;; for functions that return correct results, but whose implementation
;; is currently very far from optimal.
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
(wire and register definitions: 11)
*/

reg [1:0] loop_gcd_state = st_loop_ready;
wire loop_gcd_restarted = (st_loop_restarted == loop_gcd_state);
wire loop_gcd_waiting = (st_loop_waiting == loop_gcd_state);
reg [17:0] a_2;
reg [17:0] b_3;
wire if_test_4 = (~| b_3);
wire if_test_5 = (a_2 >= b_3);
wire [17:0] minus_6 = (a_2 - b_3);
wire [17:0] minus_7 = (b_3 - a_2);
reg [17:0] loop_gcd_result_1;
wire loop_gcd_result_1_ready = (loop_gcd_state == st_loop_ready);
always @(posedge clk)
begin
    if (start) 
        begin
            loop_gcd_state <= st_loop_inits;
        end
    else
        case (loop_gcd_state)
            st_loop_ready: 
                begin
                end
            st_loop_inits: 
                begin
                    loop_gcd_state <= st_loop_restarted;
                    a_2 <= a;
                    b_3 <= b;
                end
            st_loop_restarted: 
                begin
                    loop_gcd_state <= st_loop_waiting;
                end
            st_loop_waiting: 
                begin
                    if (if_test_4) 
                        begin
                            loop_gcd_result_1 <= a_2;
                            loop_gcd_state <= st_loop_ready;
                        end
                    else
                        if (if_test_5) 
                            begin
                                loop_gcd_state <= st_loop_restarted;
                                a_2 <= b_3;
                                b_3 <= minus_6;
                            end
                        else
                            begin
                                loop_gcd_state <= st_loop_restarted;
                                a_2 <= minus_7;
                                b_3 <= a_2;
                            end
                end
        endcase
end

assign result = loop_gcd_result_1;
assign result_ready = (loop_gcd_result_1_ready & (~ start));
endmodule
