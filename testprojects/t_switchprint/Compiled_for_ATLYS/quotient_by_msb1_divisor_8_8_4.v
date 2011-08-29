
module quotient_by_msb1_divisor_8_8_4(
        input clk,
        input start,
        input [7:0] dividend,
        input [3:0] orgdiv,
        output [7:0] result,
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
(wire and register definitions: 28)
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
reg [2:0] i_9;
reg [7:0] q_10;
reg [7:0] r_11;
reg [7:0] divider_12;
wire zerop_13 = (~| i_9);
wire zerop_14 = (~| orgdiv);
wire if_test_15 = (zerop_13 || zerop_14);
wire if_test_16 = (divider_12 > r_11);
wire [2:0] sub1_17 = ((-1)+ i_9);
wire [7:0] shl_18 = (q_10 << 1'd1);
wire [7:0] shr_19 = (divider_12 >> 1'd1);
wire [2:0] sub1_20 = ((-1)+ i_9);
wire [7:0] shl_21 = (q_10 << 1'd1);
wire [7:0] plus_22 = (shl_21 + 8'd1);
wire [7:0] minus_23 = (r_11 - divider_12);
wire [7:0] shr_24 = (divider_12 >> 1'd1);
reg [7:0] loop_loop_result_1;
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
                    i_9 <= 3'd5;
                    q_10 <= 8'd0;
                    r_11 <= dividend;
                    divider_12 <= conc2_8;
                end
            st_loop_restarted: 
                begin
                    loop_loop_state <= st_loop_waiting;
                end
            st_loop_waiting: 
                begin
                    if (if_test_15) 
                        begin
                            loop_loop_result_1 <= q_10;
                            loop_loop_state <= st_loop_ready;
                        end
                    else
                        if (if_test_16) 
                            begin
                                loop_loop_state <= st_loop_restarted;
                                i_9 <= sub1_17;
                                q_10 <= shl_18;
                                r_11 <= r_11;
                                divider_12 <= shr_19;
                            end
                        else
                            begin
                                loop_loop_state <= st_loop_restarted;
                                i_9 <= sub1_20;
                                q_10 <= plus_22;
                                r_11 <= minus_23;
                                divider_12 <= shr_24;
                            end
                end
        endcase
end

assign result = loop_loop_result_1;
assign result_ready = (loop_loop_result_1_ready & (~ start));
endmodule
