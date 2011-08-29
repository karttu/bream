
module remainder_4_22_4(
        input clk,
        input start,
        input [21:0] dividend,
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
(wire and register definitions: 38)
*/

reg [1:0] loop_loop_state = st_loop_ready;
wire loop_loop_restarted = (st_loop_restarted == loop_loop_state);
wire loop_loop_waiting = (st_loop_waiting == loop_loop_state);
wire [24:0] conc2_2 = ({ 3'd0 , dividend });
wire [24:0] conc2_3 = ({ orgdiv , 21'd0 });
reg [24:0] r_4;
reg [24:0] divider_5;
wire zerop_6 = (~| divider_5);
wire [4:0] conc2_7 = ({ 1'd0 , orgdiv });
wire [5:0] conc2_8 = ({ 1'd0 , conc2_7 });
wire [6:0] conc2_9 = ({ 1'd0 , conc2_8 });
wire [7:0] conc2_10 = ({ 1'd0 , conc2_9 });
wire [8:0] conc2_11 = ({ 1'd0 , conc2_10 });
wire [9:0] conc2_12 = ({ 1'd0 , conc2_11 });
wire [10:0] conc2_13 = ({ 1'd0 , conc2_12 });
wire [11:0] conc2_14 = ({ 1'd0 , conc2_13 });
wire [12:0] conc2_15 = ({ 1'd0 , conc2_14 });
wire [13:0] conc2_16 = ({ 1'd0 , conc2_15 });
wire [14:0] conc2_17 = ({ 1'd0 , conc2_16 });
wire [15:0] conc2_18 = ({ 1'd0 , conc2_17 });
wire [16:0] conc2_19 = ({ 1'd0 , conc2_18 });
wire [17:0] conc2_20 = ({ 1'd0 , conc2_19 });
wire [18:0] conc2_21 = ({ 1'd0 , conc2_20 });
wire [19:0] conc2_22 = ({ 1'd0 , conc2_21 });
wire [20:0] conc2_23 = ({ 1'd0 , conc2_22 });
wire [21:0] conc2_24 = ({ 1'd0 , conc2_23 });
wire [22:0] conc2_25 = ({ 1'd0 , conc2_24 });
wire [23:0] conc2_26 = ({ 1'd0 , conc2_25 });
wire [24:0] conc2_27 = ({ 1'd0 , conc2_26 });
wire lt_28 = (r_4 < conc2_27);
wire if_test_29 = (zerop_6 || lt_28);
wire [3:0] branch_t_30 = (r_4 [3:0]);
wire if_test_31 = (divider_5 > r_4);
wire [24:0] shr_32 = (divider_5 >> 1'd1);
wire [24:0] minus_33 = (r_4 - divider_5);
wire [24:0] shr_34 = (divider_5 >> 1'd1);
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
                    r_4 <= conc2_2;
                    divider_5 <= conc2_3;
                end
            st_loop_restarted: 
                begin
                    loop_loop_state <= st_loop_waiting;
                end
            st_loop_waiting: 
                begin
                    if (if_test_29) 
                        begin
                            loop_loop_result_1 <= branch_t_30;
                            loop_loop_state <= st_loop_ready;
                        end
                    else
                        if (if_test_31) 
                            begin
                                loop_loop_state <= st_loop_restarted;
                                r_4 <= r_4;
                                divider_5 <= shr_32;
                            end
                        else
                            begin
                                loop_loop_state <= st_loop_restarted;
                                r_4 <= minus_33;
                                divider_5 <= shr_34;
                            end
                end
        endcase
end

assign result = loop_loop_result_1;
assign result_ready = (loop_loop_result_1_ready & (~ start));
endmodule
