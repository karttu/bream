
module quotient_by_msb1_divisor_20_20_4(
        input clk,
        input start,
        input [19:0] dividend,
        input [3:0] orgdiv,
        output [19:0] result,
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
(wire and register definitions: 52)
*/

reg [1:0] loop_loop_state = st_loop_ready;
wire loop_loop_restarted = (st_loop_restarted == loop_loop_state);
wire loop_loop_waiting = (st_loop_waiting == loop_loop_state);
wire [4:0] n_2 = 5'd16;
wire [3:0] n_3 = 4'd15;
wire [3:0] n_4 = 4'd14;
wire [3:0] n_5 = 4'd13;
wire [3:0] n_6 = 4'd12;
wire [3:0] n_7 = 4'd11;
wire [3:0] n_8 = 4'd10;
wire [3:0] n_9 = 4'd9;
wire [3:0] n_10 = 4'd8;
wire [2:0] n_11 = 3'd7;
wire [2:0] n_12 = 3'd6;
wire [2:0] n_13 = 3'd5;
wire [2:0] n_14 = 3'd4;
wire [1:0] n_15 = 2'd3;
wire [1:0] n_16 = 2'd2;
wire [1:0] conc2_17 = ({ 1'd0 , 1'd0 });
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
wire [19:0] conc2_32 = ({ orgdiv , conc2_31 });
reg [4:0] i_33;
reg [19:0] q_34;
reg [19:0] r_35;
reg [19:0] divider_36;
wire zerop_37 = (~| i_33);
wire zerop_38 = (~| orgdiv);
wire if_test_39 = (zerop_37 || zerop_38);
wire if_test_40 = (divider_36 > r_35);
wire [4:0] sub1_41 = ((-1)+ i_33);
wire [19:0] shl_42 = (q_34 << 1'd1);
wire [19:0] shr_43 = (divider_36 >> 1'd1);
wire [4:0] sub1_44 = ((-1)+ i_33);
wire [19:0] shl_45 = (q_34 << 1'd1);
wire [19:0] plus_46 = (shl_45 + 20'd1);
wire [19:0] minus_47 = (r_35 - divider_36);
wire [19:0] shr_48 = (divider_36 >> 1'd1);
reg [19:0] loop_loop_result_1;
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
                    i_33 <= 5'd17;
                    q_34 <= 20'd0;
                    r_35 <= dividend;
                    divider_36 <= conc2_32;
                end
            st_loop_restarted: 
                begin
                    loop_loop_state <= st_loop_waiting;
                end
            st_loop_waiting: 
                begin
                    if (if_test_39) 
                        begin
                            loop_loop_result_1 <= q_34;
                            loop_loop_state <= st_loop_ready;
                        end
                    else
                        if (if_test_40) 
                            begin
                                loop_loop_state <= st_loop_restarted;
                                i_33 <= sub1_41;
                                q_34 <= shl_42;
                                r_35 <= r_35;
                                divider_36 <= shr_43;
                            end
                        else
                            begin
                                loop_loop_state <= st_loop_restarted;
                                i_33 <= sub1_44;
                                q_34 <= plus_46;
                                r_35 <= minus_47;
                                divider_36 <= shr_48;
                            end
                end
        endcase
end

assign result = loop_loop_result_1;
assign result_ready = (loop_loop_result_1_ready & (~ start));
endmodule
