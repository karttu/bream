
module quotient_22_22_4(
        input clk,
        input start,
        input [21:0] dividend,
        input [3:0] orgdiv,
        output [21:0] result,
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
(wire and register definitions: 23)
*/

reg [1:0] loop_loop_state = st_loop_ready;
wire loop_loop_restarted = (st_loop_restarted == loop_loop_state);
wire loop_loop_waiting = (st_loop_waiting == loop_loop_state);
wire [24:0] conc2_2 = ({ 3'd0 , dividend });
wire [24:0] conc2_3 = ({ orgdiv , 21'd0 });
reg [4:0] i_4;
reg [21:0] q_5;
reg [24:0] r_6;
reg [24:0] divider_7;
wire zerop_8 = (~| i_4);
wire zerop_9 = (~| orgdiv);
wire if_test_10 = (zerop_8 || zerop_9);
wire if_test_11 = (divider_7 > r_6);
wire [4:0] sub1_12 = ((-1)+ i_4);
wire [21:0] shl_13 = (q_5 << 1'd1);
wire [24:0] shr_14 = (divider_7 >> 1'd1);
wire [4:0] sub1_15 = ((-1)+ i_4);
wire [21:0] shl_16 = (q_5 << 1'd1);
wire [21:0] plus_17 = (shl_16 + 22'd1);
wire [24:0] minus_18 = (r_6 - divider_7);
wire [24:0] shr_19 = (divider_7 >> 1'd1);
reg [21:0] loop_loop_result_1;
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
                    i_4 <= 5'd22;
                    q_5 <= 22'd0;
                    r_6 <= conc2_2;
                    divider_7 <= conc2_3;
                end
            st_loop_restarted: 
                begin
                    loop_loop_state <= st_loop_waiting;
                end
            st_loop_waiting: 
                begin
                    if (if_test_10) 
                        begin
                            loop_loop_result_1 <= q_5;
                            loop_loop_state <= st_loop_ready;
                        end
                    else
                        if (if_test_11) 
                            begin
                                loop_loop_state <= st_loop_restarted;
                                i_4 <= sub1_12;
                                q_5 <= shl_13;
                                r_6 <= r_6;
                                divider_7 <= shr_14;
                            end
                        else
                            begin
                                loop_loop_state <= st_loop_restarted;
                                i_4 <= sub1_15;
                                q_5 <= plus_17;
                                r_6 <= minus_18;
                                divider_7 <= shr_19;
                            end
                end
        endcase
end

assign result = loop_loop_result_1;
assign result_ready = (loop_loop_result_1_ready & (~ start));
endmodule
