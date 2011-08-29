
module quotient_by_msb1_divisor_18_18_4(
        input clk,
        input start,
        input [17:0] dividend,
        input [3:0] orgdiv,
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
(wire and register definitions: 48)
*/

reg [1:0] loop_loop_state = st_loop_ready;
wire loop_loop_restarted = (st_loop_restarted == loop_loop_state);
wire loop_loop_waiting = (st_loop_waiting == loop_loop_state);
wire [4:0] n_2 = 5'd14;
wire [3:0] n_3 = 4'd13;
wire [3:0] n_4 = 4'd12;
wire [3:0] n_5 = 4'd11;
wire [3:0] n_6 = 4'd10;
wire [3:0] n_7 = 4'd9;
wire [3:0] n_8 = 4'd8;
wire [2:0] n_9 = 3'd7;
wire [2:0] n_10 = 3'd6;
wire [2:0] n_11 = 3'd5;
wire [2:0] n_12 = 3'd4;
wire [1:0] n_13 = 2'd3;
wire [1:0] n_14 = 2'd2;
wire [1:0] conc2_15 = ({ 1'd0 , 1'd0 });
wire [2:0] conc2_16 = ({ 1'd0 , conc2_15 });
wire [3:0] conc2_17 = ({ 1'd0 , conc2_16 });
wire [4:0] conc2_18 = ({ 1'd0 , conc2_17 });
wire [5:0] conc2_19 = ({ 1'd0 , conc2_18 });
wire [6:0] conc2_20 = ({ 1'd0 , conc2_19 });
wire [7:0] conc2_21 = ({ 1'd0 , conc2_20 });
wire [8:0] conc2_22 = ({ 1'd0 , conc2_21 });
wire [9:0] conc2_23 = ({ 1'd0 , conc2_22 });
wire [10:0] conc2_24 = ({ 1'd0 , conc2_23 });
wire [11:0] conc2_25 = ({ 1'd0 , conc2_24 });
wire [12:0] conc2_26 = ({ 1'd0 , conc2_25 });
wire [13:0] conc2_27 = ({ 1'd0 , conc2_26 });
wire [17:0] conc2_28 = ({ orgdiv , conc2_27 });
reg [3:0] i_29;
reg [17:0] q_30;
reg [17:0] r_31;
reg [17:0] divider_32;
wire zerop_33 = (~| i_29);
wire zerop_34 = (~| orgdiv);
wire if_test_35 = (zerop_33 || zerop_34);
wire if_test_36 = (divider_32 > r_31);
wire [3:0] sub1_37 = ((-1)+ i_29);
wire [17:0] shl_38 = (q_30 << 1'd1);
wire [17:0] shr_39 = (divider_32 >> 1'd1);
wire [3:0] sub1_40 = ((-1)+ i_29);
wire [17:0] shl_41 = (q_30 << 1'd1);
wire [17:0] plus_42 = (shl_41 + 18'd1);
wire [17:0] minus_43 = (r_31 - divider_32);
wire [17:0] shr_44 = (divider_32 >> 1'd1);
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
                    i_29 <= 4'd15;
                    q_30 <= 18'd0;
                    r_31 <= dividend;
                    divider_32 <= conc2_28;
                end
            st_loop_restarted: 
                begin
                    loop_loop_state <= st_loop_waiting;
                end
            st_loop_waiting: 
                begin
                    if (if_test_35) 
                        begin
                            loop_loop_result_1 <= q_30;
                            loop_loop_state <= st_loop_ready;
                        end
                    else
                        if (if_test_36) 
                            begin
                                loop_loop_state <= st_loop_restarted;
                                i_29 <= sub1_37;
                                q_30 <= shl_38;
                                r_31 <= r_31;
                                divider_32 <= shr_39;
                            end
                        else
                            begin
                                loop_loop_state <= st_loop_restarted;
                                i_29 <= sub1_40;
                                q_30 <= plus_42;
                                r_31 <= minus_43;
                                divider_32 <= shr_44;
                            end
                end
        endcase
end

assign result = loop_loop_result_1;
assign result_ready = (loop_loop_result_1_ready & (~ start));
endmodule
