
module t_intfac1(
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
;; t_intfac1.brm.scm - A very first simple test program for Bream compiler.
;; Copyright (C) 2010-2011 Antti Karttunen, and subject to GPL v2.
;; This outputs numbers 0 - 10! in factorial number system notation.
;; That is, from 0 to 1000000000
;;
;; CHANGES
;;
;; Edited    Aug 28 2011 by karttu.
;;   Renamed the I/O argument txd to out_uart_txd (by the new naming scheme).
;;   Use macro print_dec_with_crlf for printing out the decimal numbers.
;;   Corrected the upper limit to conform with our claims above...
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
(wire and register definitions: 10)
*/

reg [1:0] loop_loop_state = st_loop_ready;
wire loop_loop_restarted = (st_loop_restarted == loop_loop_state);
wire loop_loop_waiting = (st_loop_waiting == loop_loop_state);
reg [21:0] i_2;
wire outfac_with_2_postdelims_4;
wire outfac_with_2_postdelims_4_ready;
wire seqif_test_7 = (22'd3628800 == i_2);
wire [21:0] plus_8 = (i_2 + 22'd1);
reg [21:0] loop_loop_result_1;
wire loop_loop_result_1_ready = (loop_loop_state == st_loop_ready);

outfac_with_2_postdelims_1_1_22_8_8 outfac_with_2_postdelims_5(
        .clk(clk),
        .start(loop_loop_restarted),
        .outchan(out_uart_txd),
        .n(i_2),
        .delim1byte(8'd13),
        .delim2byte(8'd10),
        .result_ready(outfac_with_2_postdelims_4_ready),
        .result(outfac_with_2_postdelims_4));


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
                    i_2 <= 22'd0;
                end
            st_loop_restarted: 
                begin
                    loop_loop_state <= st_loop_waiting;
                end
            st_loop_waiting: 
                begin
                    begin
                        if (outfac_with_2_postdelims_4_ready) 
                            if (seqif_test_7) 
                                begin
                                    loop_loop_result_1 <= i_2;
                                    loop_loop_state <= st_loop_ready;
                                end
                            else
                                begin
                                    loop_loop_state <= st_loop_restarted;
                                    i_2 <= plus_8;
                                end
                    end
                end
        endcase
end

assign result_ready = (loop_loop_result_1_ready & (~ start));
endmodule
