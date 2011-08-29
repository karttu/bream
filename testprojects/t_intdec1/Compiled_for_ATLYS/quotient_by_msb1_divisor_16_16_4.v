
module quotient_by_msb1_divisor_16_16_4(
        input clk,
        input start,
        input [15:0] dividend,
        input [3:0] orgdiv,
        output [15:0] result,
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
(wire and register definitions: 44)
*/

reg [1:0] loop_loop_state = st_loop_ready;
wire loop_loop_restarted = (st_loop_restarted == loop_loop_state);
wire loop_loop_waiting = (st_loop_waiting == loop_loop_state);
wire [4:0] n_2 = 5'd12;
wire [3:0] n_3 = 4'd11;
wire [3:0] n_4 = 4'd10;
wire [3:0] n_5 = 4'd9;
wire [3:0] n_6 = 4'd8;
wire [2:0] n_7 = 3'd7;
wire [2:0] n_8 = 3'd6;
wire [2:0] n_9 = 3'd5;
wire [2:0] n_10 = 3'd4;
wire [1:0] n_11 = 2'd3;
wire [1:0] n_12 = 2'd2;
wire [1:0] conc2_13 = ({ 1'd0 , 1'd0 });
wire [2:0] conc2_14 = ({ 1'd0 , conc2_13 });
wire [3:0] conc2_15 = ({ 1'd0 , conc2_14 });
wire [4:0] conc2_16 = ({ 1'd0 , conc2_15 });
wire [5:0] conc2_17 = ({ 1'd0 , conc2_16 });
wire [6:0] conc2_18 = ({ 1'd0 , conc2_17 });
wire [7:0] conc2_19 = ({ 1'd0 , conc2_18 });
wire [8:0] conc2_20 = ({ 1'd0 , conc2_19 });
wire [9:0] conc2_21 = ({ 1'd0 , conc2_20 });
wire [10:0] conc2_22 = ({ 1'd0 , conc2_21 });
wire [11:0] conc2_23 = ({ 1'd0 , conc2_22 });
wire [15:0] conc2_24 = ({ orgdiv , conc2_23 });
reg [3:0] i_25;
reg [15:0] q_26;
reg [15:0] r_27;
reg [15:0] divider_28;
wire zerop_29 = (~| i_25);
wire zerop_30 = (~| orgdiv);
wire if_test_31 = (zerop_29 || zerop_30);
wire if_test_32 = (divider_28 > r_27);
wire [3:0] sub1_33 = ((-1)+ i_25);
wire [15:0] shl_34 = (q_26 << 1'd1);
wire [15:0] shr_35 = (divider_28 >> 1'd1);
wire [3:0] sub1_36 = ((-1)+ i_25);
wire [15:0] shl_37 = (q_26 << 1'd1);
wire [15:0] plus_38 = (shl_37 + 16'd1);
wire [15:0] minus_39 = (r_27 - divider_28);
wire [15:0] shr_40 = (divider_28 >> 1'd1);
reg [15:0] loop_loop_result_1;
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
                    i_25 <= 4'd13;
                    q_26 <= 16'd0;
                    r_27 <= dividend;
                    divider_28 <= conc2_24;
                end
            st_loop_restarted: 
                begin
                    loop_loop_state <= st_loop_waiting;
                end
            st_loop_waiting: 
                begin
                    if (if_test_31) 
                        begin
                            loop_loop_result_1 <= q_26;
                            loop_loop_state <= st_loop_ready;
                        end
                    else
                        if (if_test_32) 
                            begin
                                loop_loop_state <= st_loop_restarted;
                                i_25 <= sub1_33;
                                q_26 <= shl_34;
                                r_27 <= r_27;
                                divider_28 <= shr_35;
                            end
                        else
                            begin
                                loop_loop_state <= st_loop_restarted;
                                i_25 <= sub1_36;
                                q_26 <= plus_38;
                                r_27 <= minus_39;
                                divider_28 <= shr_40;
                            end
                end
        endcase
end

assign result = loop_loop_result_1;
assign result_ready = (loop_loop_result_1_ready & (~ start));
endmodule
