
module outchars12_1_1_4_8_8_8_8_8_8_8_8_8_8_8_8(
        input clk,
        input start,
        output outchan,
        input [3:0] n,
        input [7:0] c0,
        input [7:0] c1,
        input [7:0] c2,
        input [7:0] c3,
        input [7:0] c4,
        input [7:0] c5,
        input [7:0] c6,
        input [7:0] c7,
        input [7:0] c8,
        input [7:0] c9,
        input [7:0] c10,
        input [7:0] c11,
        output result,
        output result_ready);

/*

;;
;; simpleio.brm.scm - Very simple I/O routines for the initial version
;;                    of Bream. Currently just a couple of output routines
;;                    that use a separate outbyte115200_1_1_8.v Verilog-module
;;                    which sends a byte to UART with the speed 115200 bps.
;;
;; Copyright (C) 2010-2011 Antti Karttunen, and subject to LGPL x.xx?
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
(wire and register definitions: 24)
*/

reg [1:0] loop_loop_state = st_loop_ready;
wire loop_loop_restarted = (st_loop_restarted == loop_loop_state);
wire loop_loop_waiting = (st_loop_waiting == loop_loop_state);
reg [3:0] n_2;
reg [7:0] c0_3;
reg [7:0] c1_4;
reg [7:0] c2_5;
reg [7:0] c3_6;
reg [7:0] c4_7;
reg [7:0] c5_8;
reg [7:0] c6_9;
reg [7:0] c7_10;
reg [7:0] c8_11;
reg [7:0] c9_12;
reg [7:0] c10_13;
reg [7:0] c11_14;
wire seqif_test_16 = (~| n_2);
reg seq2_18_started;
wire seq2_18_startable = (~| seqif_test_16);
wire seq2_18;
wire seq2_18_ready;
wire [3:0] minus_20 = (n_2 - 4'd1);
reg loop_loop_result_1;
wire loop_loop_result_1_ready = (loop_loop_state == st_loop_ready);

outbyte115200_1_1_8 outbyte115200_19(
        .clk(clk),
        .start((loop_loop_waiting & seq2_18_startable & (~ seq2_18_started))),
        .outchan(outchan),
        .byte_to_output(c0_3),
        .result_ready(seq2_18_ready),
        .result(seq2_18));


always @(posedge clk)
begin
    if (start) 
        begin
            loop_loop_state <= st_loop_inits;
            seq2_18_started <= 0;
        end
    else
        case (loop_loop_state)
            st_loop_ready: 
                begin
                end
            st_loop_inits: 
                begin
                    loop_loop_state <= st_loop_restarted;
                    n_2 <= n;
                    c0_3 <= c0;
                    c1_4 <= c1;
                    c2_5 <= c2;
                    c3_6 <= c3;
                    c4_7 <= c4;
                    c5_8 <= c5;
                    c6_9 <= c6;
                    c7_10 <= c7;
                    c8_11 <= c8;
                    c9_12 <= c9;
                    c10_13 <= c10;
                    c11_14 <= c11;
                end
            st_loop_restarted: 
                begin
                    loop_loop_state <= st_loop_waiting;
                    seq2_18_started <= 0;
                end
            st_loop_waiting: 
                begin
                    begin
                        if (seq2_18_startable) 
                            seq2_18_started <= 1;
                    end
                    if (seqif_test_16) 
                        begin
                            loop_loop_result_1 <= 1'd1;
                            loop_loop_state <= st_loop_ready;
                        end
                    else
                        begin
                            if ((seq2_18_ready & seq2_18_started)) 
                                begin
                                    loop_loop_state <= st_loop_restarted;
                                    n_2 <= minus_20;
                                    c0_3 <= c1_4;
                                    c1_4 <= c2_5;
                                    c2_5 <= c3_6;
                                    c3_6 <= c4_7;
                                    c4_7 <= c5_8;
                                    c5_8 <= c6_9;
                                    c6_9 <= c7_10;
                                    c7_10 <= c8_11;
                                    c8_11 <= c9_12;
                                    c9_12 <= c10_13;
                                    c10_13 <= c11_14;
                                    c11_14 <= 8'd126;
                                end
                        end
                end
        endcase
end

assign result = loop_loop_result_1;
assign result_ready = (loop_loop_result_1_ready & (~ start));
endmodule
