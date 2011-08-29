
module test1out(
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
;; test1out.brm.scm - A very first simple test program for Bream compiler.
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
(wire and register definitions: 13)
*/

reg [1:0] loop_loop_state = st_loop_ready;
wire loop_loop_restarted = (st_loop_restarted == loop_loop_state);
wire loop_loop_waiting = (st_loop_waiting == loop_loop_state);
reg [3:0] i_2;
wire seqif_test_4 = (i_2 == 4'd11);
wire [3:0] plusw_7 = (i_2 + 4'd2);
reg seq2_6_started;
wire seq2_6_startable = (~| seqif_test_4);
wire seq2_6;
wire seq2_6_ready;
wire [3:0] plusw_9 = (i_2 + 4'd1);
reg [3:0] loop_loop_result_1;
wire loop_loop_result_1_ready = (loop_loop_state == st_loop_ready);

outchars12_1_1_4_8_8_8_8_8_8_8_8_8_8_8_8 outchars12_8(
        .clk(clk),
        .start((loop_loop_waiting & seq2_6_startable & (~ seq2_6_started))),
        .outchan(out_uart_txd),
        .n(plusw_7),
        .c0(8'd13),
        .c1(8'd10),
        .c2(8'd72),
        .c3(8'd101),
        .c4(8'd108),
        .c5(8'd108),
        .c6(8'd111),
        .c7(8'd87),
        .c8(8'd111),
        .c9(8'd114),
        .c10(8'd108),
        .c11(8'd100),
        .result_ready(seq2_6_ready),
        .result(seq2_6));


always @(posedge clk)
begin
    if (start) 
        begin
            loop_loop_state <= st_loop_inits;
            seq2_6_started <= 0;
        end
    else
        case (loop_loop_state)
            st_loop_ready: 
                begin
                end
            st_loop_inits: 
                begin
                    loop_loop_state <= st_loop_restarted;
                    i_2 <= 4'd0;
                end
            st_loop_restarted: 
                begin
                    loop_loop_state <= st_loop_waiting;
                    seq2_6_started <= 0;
                end
            st_loop_waiting: 
                begin
                    begin
                        if (seq2_6_startable) 
                            seq2_6_started <= 1;
                    end
                    if (seqif_test_4) 
                        begin
                            loop_loop_result_1 <= i_2;
                            loop_loop_state <= st_loop_ready;
                        end
                    else
                        begin
                            if ((seq2_6_ready & seq2_6_started)) 
                                begin
                                    loop_loop_state <= st_loop_restarted;
                                    i_2 <= plusw_9;
                                end
                        end
                end
        endcase
end

assign result_ready = (loop_loop_result_1_ready & (~ start));
endmodule
