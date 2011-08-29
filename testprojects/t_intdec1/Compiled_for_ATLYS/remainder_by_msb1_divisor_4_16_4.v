
module remainder_by_msb1_divisor_4_16_4(
        input clk,
        input start,
        input [15:0] dividend,
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
(wire and register definitions: 50)
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
reg [15:0] r_25;
reg [15:0] divider_26;
wire zerop_27 = (~| divider_26);
wire [4:0] conc2_28 = ({ 1'd0 , orgdiv });
wire [5:0] conc2_29 = ({ 1'd0 , conc2_28 });
wire [6:0] conc2_30 = ({ 1'd0 , conc2_29 });
wire [7:0] conc2_31 = ({ 1'd0 , conc2_30 });
wire [8:0] conc2_32 = ({ 1'd0 , conc2_31 });
wire [9:0] conc2_33 = ({ 1'd0 , conc2_32 });
wire [10:0] conc2_34 = ({ 1'd0 , conc2_33 });
wire [11:0] conc2_35 = ({ 1'd0 , conc2_34 });
wire [12:0] conc2_36 = ({ 1'd0 , conc2_35 });
wire [13:0] conc2_37 = ({ 1'd0 , conc2_36 });
wire [14:0] conc2_38 = ({ 1'd0 , conc2_37 });
wire [15:0] conc2_39 = ({ 1'd0 , conc2_38 });
wire lt_40 = (r_25 < conc2_39);
wire if_test_41 = (zerop_27 || lt_40);
wire [3:0] branch_t_42 = (r_25 [3:0]);
wire if_test_43 = (divider_26 > r_25);
wire [15:0] shr_44 = (divider_26 >> 1'd1);
wire [15:0] minus_45 = (r_25 - divider_26);
wire [15:0] shr_46 = (divider_26 >> 1'd1);
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
                    r_25 <= dividend;
                    divider_26 <= conc2_24;
                end
            st_loop_restarted: 
                begin
                    loop_loop_state <= st_loop_waiting;
                end
            st_loop_waiting: 
                begin
                    if (if_test_41) 
                        begin
                            loop_loop_result_1 <= branch_t_42;
                            loop_loop_state <= st_loop_ready;
                        end
                    else
                        if (if_test_43) 
                            begin
                                loop_loop_state <= st_loop_restarted;
                                r_25 <= r_25;
                                divider_26 <= shr_44;
                            end
                        else
                            begin
                                loop_loop_state <= st_loop_restarted;
                                r_25 <= minus_45;
                                divider_26 <= shr_46;
                            end
                end
        endcase
end

assign result = loop_loop_result_1;
assign result_ready = (loop_loop_result_1_ready & (~ start));
endmodule
