
module t_gcd1test(
        input clk,
        input start,
        input [7:0] in_switches,
        input [2:0] in_buttons,
        input in_uart_rxd,
        output out_uart_txd,
        input [6:0] out_leds,
        output result_ready);

/*

;;
;; t_gcd1test.brm.scm - Output n: (gcd (high-half n) (low-half n))
;; Copyright (C) 2010-2011 Antti Karttunen, and subject to GPL v2.
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
(wire and register definitions: 39)
*/

wire [19:0] uplimit_1 = 20'd1048575;
reg [1:0] loop_loop_state = st_loop_ready;
wire loop_loop_restarted = (st_loop_restarted == loop_loop_state);
wire loop_loop_waiting = (st_loop_waiting == loop_loop_state);
wire [19:0] bitxor_3 = (uplimit_1 ^ uplimit_1);
reg [19:0] i_4;
reg p_5;
wire tif_test_9 = (~| p_5);
wire [9:0] drop_11 = (i_4 [19:10]);
wire [9:0] bits_12 = (i_4 [9:0]);
wire [9:0] a_10;
wire a_10_ready;
wire [10:0] conc2_14 = ({ 1'd0 , a_10 });
wire [11:0] conc2_15 = ({ 1'd0 , conc2_14 });
wire [12:0] conc2_16 = ({ 1'd0 , conc2_15 });
wire [13:0] conc2_17 = ({ 1'd0 , conc2_16 });
wire [14:0] conc2_18 = ({ 1'd0 , conc2_17 });
wire [15:0] conc2_19 = ({ 1'd0 , conc2_18 });
wire [16:0] conc2_20 = ({ 1'd0 , conc2_19 });
wire [17:0] conc2_21 = ({ 1'd0 , conc2_20 });
wire [18:0] conc2_22 = ({ 1'd0 , conc2_21 });
wire [19:0] conc2_23 = ({ 1'd0 , conc2_22 });
wire [19:0] if_8 = (tif_test_9 ? i_4 : conc2_23);
wire if_8_ready = ((0 !=  tif_test_9) ? 1 : a_10_ready);
wire tif_test_25 = (~| p_5);
wire [7:0] if_24 = (tif_test_25 ? 8'd58 : 8'd13);
wire tif_test_27 = (~| p_5);
wire [7:0] if_26 = (tif_test_27 ? 8'd32 : 8'd10);
reg seq2_7_started;
wire seq2_7_startable = if_8_ready;
wire seq2_7;
wire seq2_7_ready;
wire if_test_29 = (~| p_5);
wire bitnot_30 = (~ p_5);
wire if_test_31 = (i_4 < uplimit_1);
wire [19:0] plus_32 = (20'd1 + i_4);
wire bitnot_33 = (~ p_5);
reg [19:0] loop_loop_result_2;
wire loop_loop_result_2_ready = (loop_loop_state == st_loop_ready);

gcd_10_10_10 gcd_13(
        .clk(clk),
        .start(loop_loop_restarted),
        .a(drop_11),
        .b(bits_12),
        .result_ready(a_10_ready),
        .result(a_10));



outdec_with_2_postdelims_1_1_20_8_8 outdec_with_2_postdelims_28(
        .clk(clk),
        .start((loop_loop_waiting & seq2_7_startable & (~ seq2_7_started))),
        .outchan(out_uart_txd),
        .n(if_8),
        .delim1byte(if_24),
        .delim2byte(if_26),
        .result_ready(seq2_7_ready),
        .result(seq2_7));


always @(posedge clk)
begin
    if (start) 
        begin
            loop_loop_state <= st_loop_inits;
            seq2_7_started <= 0;
        end
    else
        case (loop_loop_state)
            st_loop_ready: 
                begin
                end
            st_loop_inits: 
                begin
                    loop_loop_state <= st_loop_restarted;
                    i_4 <= bitxor_3;
                    p_5 <= 1'd0;
                end
            st_loop_restarted: 
                begin
                    loop_loop_state <= st_loop_waiting;
                    seq2_7_started <= 0;
                end
            st_loop_waiting: 
                begin
                    begin
                        if (seq2_7_startable) 
                            seq2_7_started <= 1;
                    end
                    begin
                        if ((seq2_7_ready & seq2_7_started)) 
                            if (if_test_29) 
                                begin
                                    loop_loop_state <= st_loop_restarted;
                                    i_4 <= i_4;
                                    p_5 <= bitnot_30;
                                end
                            else
                                if (if_test_31) 
                                    begin
                                        loop_loop_state <= st_loop_restarted;
                                        i_4 <= plus_32;
                                        p_5 <= bitnot_33;
                                    end
                                else
                                    begin
                                        loop_loop_result_2 <= i_4;
                                        loop_loop_state <= st_loop_ready;
                                    end
                    end
                end
        endcase
end

assign result_ready = (loop_loop_result_2_ready & (~ start));
endmodule
