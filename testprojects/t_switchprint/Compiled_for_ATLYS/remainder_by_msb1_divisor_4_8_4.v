
module remainder_by_msb1_divisor_4_8_4(
        input clk,
        input start,
        input [7:0] dividend,
        input [3:0] orgdiv,
        output [3:0] result,
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
(wire and register definitions: 26)
*/

reg [1:0] loop_loop_state = st_loop_ready;
wire loop_loop_restarted = (st_loop_restarted == loop_loop_state);
wire loop_loop_waiting = (st_loop_waiting == loop_loop_state);
wire [3:0] n_2 = 4'd4;
wire [1:0] n_3 = 2'd3;
wire [1:0] n_4 = 2'd2;
wire [1:0] conc2_5 = ({ 1'd0 , 1'd0 });
wire [2:0] conc2_6 = ({ 1'd0 , conc2_5 });
wire [3:0] conc2_7 = ({ 1'd0 , conc2_6 });
wire [7:0] conc2_8 = ({ orgdiv , conc2_7 });
reg [7:0] r_9;
reg [7:0] divider_10;
wire zerop_11 = (~| divider_10);
wire [4:0] conc2_12 = ({ 1'd0 , orgdiv });
wire [5:0] conc2_13 = ({ 1'd0 , conc2_12 });
wire [6:0] conc2_14 = ({ 1'd0 , conc2_13 });
wire [7:0] conc2_15 = ({ 1'd0 , conc2_14 });
wire lt_16 = (r_9 < conc2_15);
wire if_test_17 = (zerop_11 || lt_16);
wire [3:0] branch_t_18 = (r_9 [3:0]);
wire if_test_19 = (divider_10 > r_9);
wire [7:0] shr_20 = (divider_10 >> 1'd1);
wire [7:0] minus_21 = (r_9 - divider_10);
wire [7:0] shr_22 = (divider_10 >> 1'd1);
reg [3:0] loop_loop_result_1;
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
                    r_9 <= dividend;
                    divider_10 <= conc2_8;
                end
            st_loop_restarted: 
                begin
                    loop_loop_state <= st_loop_waiting;
                end
            st_loop_waiting: 
                begin
                    if (if_test_17) 
                        begin
                            loop_loop_result_1 <= branch_t_18;
                            loop_loop_state <= st_loop_ready;
                        end
                    else
                        if (if_test_19) 
                            begin
                                loop_loop_state <= st_loop_restarted;
                                r_9 <= r_9;
                                divider_10 <= shr_20;
                            end
                        else
                            begin
                                loop_loop_state <= st_loop_restarted;
                                r_9 <= minus_21;
                                divider_10 <= shr_22;
                            end
                end
        endcase
end

assign result = loop_loop_result_1;
assign result_ready = (loop_loop_result_1_ready & (~ start));
endmodule
