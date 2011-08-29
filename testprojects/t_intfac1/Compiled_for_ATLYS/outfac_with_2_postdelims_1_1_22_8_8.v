
module outfac_with_2_postdelims_1_1_22_8_8(
        input clk,
        input start,
        output outchan,
        input [21:0] n,
        input [7:0] delim1byte,
        input [7:0] delim2byte,
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
(wire and register definitions: 40)
*/

reg [1:0] loop_loop_state = st_loop_ready;
wire loop_loop_restarted = (st_loop_restarted == loop_loop_state);
wire loop_loop_waiting = (st_loop_waiting == loop_loop_state);
reg [21:0] n_2;
reg [3:0] i_3;
reg [7:0] c0_4;
reg [7:0] c1_5;
reg [7:0] c2_6;
reg [7:0] c3_7;
reg [7:0] c4_8;
reg [7:0] c5_9;
reg [7:0] c6_10;
reg [7:0] c7_11;
reg [7:0] c8_12;
reg [7:0] c9_13;
reg [7:0] c10_14;
reg [7:0] c11_15;
wire zerop_17 = (~| n_2);
wire gt_18 = (i_3 > 4'd2);
wire seqif_test_19 = (zerop_17 && gt_18);
reg branch_t_20_started;
wire branch_t_20_startable = (| seqif_test_19);
wire branch_t_20;
wire branch_t_20_ready;
reg quotient_22_started;
wire quotient_22_startable = (~| seqif_test_19);
wire [21:0] quotient_22;
wire quotient_22_ready;
wire [3:0] plus_24 = (i_3 + 4'd1);
reg a_25_started;
wire a_25_startable = (~| seqif_test_19);
wire [3:0] a_25;
wire a_25_ready;
wire [4:0] conc2_27 = ({ 1'd0 , a_25 });
wire [5:0] conc2_28 = ({ 1'd0 , conc2_27 });
wire [6:0] conc2_29 = ({ 1'd0 , conc2_28 });
wire [7:0] conc2_30 = ({ 1'd0 , conc2_29 });
wire [7:0] plus_31 = (8'd48 + conc2_30);
reg loop_loop_result_1;
wire loop_loop_result_1_ready = (loop_loop_state == st_loop_ready);

outchars12_1_1_4_8_8_8_8_8_8_8_8_8_8_8_8 outchars12_21(
        .clk(clk),
        .start((loop_loop_waiting & branch_t_20_startable & (~ branch_t_20_started))),
        .outchan(outchan),
        .n(i_3),
        .c0(c0_4),
        .c1(c1_5),
        .c2(c2_6),
        .c3(c3_7),
        .c4(c4_8),
        .c5(c5_9),
        .c6(c6_10),
        .c7(c7_11),
        .c8(c8_12),
        .c9(c9_13),
        .c10(c10_14),
        .c11(c11_15),
        .result_ready(branch_t_20_ready),
        .result(branch_t_20));



quotient_22_22_4 quotient_23(
        .clk(clk),
        .start((loop_loop_waiting & quotient_22_startable & (~ quotient_22_started))),
        .dividend(n_2),
        .orgdiv(i_3),
        .result_ready(quotient_22_ready),
        .result(quotient_22));



remainder_4_22_4 remainder_26(
        .clk(clk),
        .start((loop_loop_waiting & a_25_startable & (~ a_25_started))),
        .dividend(n_2),
        .orgdiv(i_3),
        .result_ready(a_25_ready),
        .result(a_25));


always @(posedge clk)
begin
    if (start) 
        begin
            loop_loop_state <= st_loop_inits;
            branch_t_20_started <= 0;
            quotient_22_started <= 0;
            a_25_started <= 0;
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
                    i_3 <= 4'd2;
                    c0_4 <= delim1byte;
                    c1_5 <= delim2byte;
                    c2_6 <= 8'd0;
                    c3_7 <= 8'd0;
                    c4_8 <= 8'd0;
                    c5_9 <= 8'd0;
                    c6_10 <= 8'd0;
                    c7_11 <= 8'd0;
                    c8_12 <= 8'd0;
                    c9_13 <= 8'd0;
                    c10_14 <= 8'd0;
                    c11_15 <= 8'd0;
                end
            st_loop_restarted: 
                begin
                    loop_loop_state <= st_loop_waiting;
                    branch_t_20_started <= 0;
                    quotient_22_started <= 0;
                    a_25_started <= 0;
                end
            st_loop_waiting: 
                begin
                    begin
                        if (branch_t_20_startable) 
                            branch_t_20_started <= 1;
                    end
                    begin
                        if (quotient_22_startable) 
                            quotient_22_started <= 1;
                    end
                    begin
                        if (a_25_startable) 
                            a_25_started <= 1;
                    end
                    if (seqif_test_19) 
                        begin
                            if ((branch_t_20_ready & branch_t_20_started)) 
                                begin
                                    loop_loop_result_1 <= branch_t_20;
                                    loop_loop_state <= st_loop_ready;
                                end
                        end
                    else
                        begin
                            if (((quotient_22_ready & quotient_22_started) & (a_25_ready & a_25_started))) 
                                begin
                                    loop_loop_state <= st_loop_restarted;
                                    n_2 <= quotient_22;
                                    i_3 <= plus_24;
                                    c0_4 <= plus_31;
                                    c1_5 <= c0_4;
                                    c2_6 <= c1_5;
                                    c3_7 <= c2_6;
                                    c4_8 <= c3_7;
                                    c5_9 <= c4_8;
                                    c6_10 <= c5_9;
                                    c7_11 <= c6_10;
                                    c8_12 <= c7_11;
                                    c9_13 <= c8_12;
                                    c10_14 <= c9_13;
                                    c11_15 <= c10_14;
                                end
                        end
                end
        endcase
end

assign result = loop_loop_result_1;
assign result_ready = (loop_loop_result_1_ready & (~ start));
endmodule
