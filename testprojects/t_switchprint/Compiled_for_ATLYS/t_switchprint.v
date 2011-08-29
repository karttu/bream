
module t_switchprint(
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
;; t_switchprint.brm.scm - Output the binary number specified by the eight
;;  slide switches (i.e. in_switches[7:0]) to UART as a decimal number.
;;  This will output a single integer after a start, and after that,
;;  a new integer on a new line, always after the user has changed
;;  some of the switches.
;;
;; Copyright (C) 2010-2011 Antti Karttunen, and subject to GPL v2.
;;
;; CHANGES
;;
;; Edited    Aug 28 2011 by karttu.
;;   Renamed the I/O arguments according to the new naming scheme.
;;   Use macro print_dec_with_crlf for printing out the decimal numbers.
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
reg [7:0] copy_of_switches_2;
wire outdec_with_2_postdelims_4;
wire outdec_with_2_postdelims_4_ready;
wire neq_8 = (in_switches != copy_of_switches_2);
reg seq2_7_started;
wire seq2_7_startable = outdec_with_2_postdelims_4_ready;
wire seq2_7;
wire seq2_7_ready;
reg [-1:0] loop_loop_result_1;
wire loop_loop_result_1_ready = (loop_loop_state == st_loop_ready);

outdec_with_2_postdelims_1_1_8_8_8 outdec_with_2_postdelims_5(
        .clk(clk),
        .start(loop_loop_restarted),
        .outchan(out_uart_txd),
        .n(copy_of_switches_2),
        .delim1byte(8'd13),
        .delim2byte(8'd10),
        .result_ready(outdec_with_2_postdelims_4_ready),
        .result(outdec_with_2_postdelims_4));



wait_until_1_1 wait_until_9(
        .clk(clk),
        .start((loop_loop_waiting & seq2_7_startable & (~ seq2_7_started))),
        .condition(neq_8),
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
                    copy_of_switches_2 <= in_switches;
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
                        if (outdec_with_2_postdelims_4_ready) 
                            begin
                                if ((seq2_7_ready & seq2_7_started)) 
                                    begin
                                        loop_loop_state <= st_loop_restarted;
                                        copy_of_switches_2 <= in_switches;
                                    end
                            end
                    end
                end
        endcase
end

assign result_ready = (loop_loop_result_1_ready & (~ start));
endmodule
