
module t_inttests(
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
;; t_inttests.brm.scm - A suite of tests for various integer functions.
;; Now testing the computation of http://oeis.org/A000010 (i.e. "phi"),
;; with a heavily parallelized naive algorithm.
;;
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
(wire and register definitions: 148)
*/

wire [17:0] uplimit_1 = 18'd262143;
reg [1:0] loop_loop_state = st_loop_ready;
wire loop_loop_restarted = (st_loop_restarted == loop_loop_state);
wire loop_loop_waiting = (st_loop_waiting == loop_loop_state);
reg [17:0] n_3;
reg p_4;
wire tif_test_8 = (~| p_4);
wire [17:0] i_9 = 18'd31;
wire [17:0] a000010_naive_par_aux_10;
wire a000010_naive_par_aux_10_ready;
wire [17:0] i_12 = 18'd30;
wire [17:0] a000010_naive_par_aux_13;
wire a000010_naive_par_aux_13_ready;
wire [17:0] i_15 = 18'd29;
wire [17:0] a000010_naive_par_aux_16;
wire a000010_naive_par_aux_16_ready;
wire [17:0] i_18 = 18'd28;
wire [17:0] a000010_naive_par_aux_19;
wire a000010_naive_par_aux_19_ready;
wire [17:0] i_21 = 18'd27;
wire [17:0] a000010_naive_par_aux_22;
wire a000010_naive_par_aux_22_ready;
wire [17:0] i_24 = 18'd26;
wire [17:0] a000010_naive_par_aux_25;
wire a000010_naive_par_aux_25_ready;
wire [17:0] i_27 = 18'd25;
wire [17:0] a000010_naive_par_aux_28;
wire a000010_naive_par_aux_28_ready;
wire [17:0] i_30 = 18'd24;
wire [17:0] a000010_naive_par_aux_31;
wire a000010_naive_par_aux_31_ready;
wire [17:0] i_33 = 18'd23;
wire [17:0] a000010_naive_par_aux_34;
wire a000010_naive_par_aux_34_ready;
wire [17:0] i_36 = 18'd22;
wire [17:0] a000010_naive_par_aux_37;
wire a000010_naive_par_aux_37_ready;
wire [17:0] i_39 = 18'd21;
wire [17:0] a000010_naive_par_aux_40;
wire a000010_naive_par_aux_40_ready;
wire [17:0] i_42 = 18'd20;
wire [17:0] a000010_naive_par_aux_43;
wire a000010_naive_par_aux_43_ready;
wire [17:0] i_45 = 18'd19;
wire [17:0] a000010_naive_par_aux_46;
wire a000010_naive_par_aux_46_ready;
wire [17:0] i_48 = 18'd18;
wire [17:0] a000010_naive_par_aux_49;
wire a000010_naive_par_aux_49_ready;
wire [17:0] i_51 = 18'd17;
wire [17:0] a000010_naive_par_aux_52;
wire a000010_naive_par_aux_52_ready;
wire [17:0] i_54 = 18'd16;
wire [17:0] a000010_naive_par_aux_55;
wire a000010_naive_par_aux_55_ready;
wire [17:0] i_57 = 18'd15;
wire [17:0] a000010_naive_par_aux_58;
wire a000010_naive_par_aux_58_ready;
wire [17:0] i_60 = 18'd14;
wire [17:0] a000010_naive_par_aux_61;
wire a000010_naive_par_aux_61_ready;
wire [17:0] i_63 = 18'd13;
wire [17:0] a000010_naive_par_aux_64;
wire a000010_naive_par_aux_64_ready;
wire [17:0] i_66 = 18'd12;
wire [17:0] a000010_naive_par_aux_67;
wire a000010_naive_par_aux_67_ready;
wire [17:0] i_69 = 18'd11;
wire [17:0] a000010_naive_par_aux_70;
wire a000010_naive_par_aux_70_ready;
wire [17:0] i_72 = 18'd10;
wire [17:0] a000010_naive_par_aux_73;
wire a000010_naive_par_aux_73_ready;
wire [17:0] i_75 = 18'd9;
wire [17:0] a000010_naive_par_aux_76;
wire a000010_naive_par_aux_76_ready;
wire [17:0] i_78 = 18'd8;
wire [17:0] a000010_naive_par_aux_79;
wire a000010_naive_par_aux_79_ready;
wire [17:0] i_81 = 18'd7;
wire [17:0] a000010_naive_par_aux_82;
wire a000010_naive_par_aux_82_ready;
wire [17:0] i_84 = 18'd6;
wire [17:0] a000010_naive_par_aux_85;
wire a000010_naive_par_aux_85_ready;
wire [17:0] i_87 = 18'd5;
wire [17:0] a000010_naive_par_aux_88;
wire a000010_naive_par_aux_88_ready;
wire [17:0] i_90 = 18'd4;
wire [17:0] a000010_naive_par_aux_91;
wire a000010_naive_par_aux_91_ready;
wire [17:0] i_93 = 18'd3;
wire [17:0] a000010_naive_par_aux_94;
wire a000010_naive_par_aux_94_ready;
wire [17:0] i_96 = 18'd2;
wire [17:0] a000010_naive_par_aux_97;
wire a000010_naive_par_aux_97_ready;
wire [17:0] i_99 = 18'd1;
wire [17:0] a000010_naive_par_aux_100;
wire a000010_naive_par_aux_100_ready;
wire [17:0] plus_102 = (a000010_naive_par_aux_100 + 18'd0);
wire [17:0] plus_103 = (a000010_naive_par_aux_97 + plus_102);
wire [17:0] plus_104 = (a000010_naive_par_aux_94 + plus_103);
wire [17:0] plus_105 = (a000010_naive_par_aux_91 + plus_104);
wire [17:0] plus_106 = (a000010_naive_par_aux_88 + plus_105);
wire [17:0] plus_107 = (a000010_naive_par_aux_85 + plus_106);
wire [17:0] plus_108 = (a000010_naive_par_aux_82 + plus_107);
wire [17:0] plus_109 = (a000010_naive_par_aux_79 + plus_108);
wire [17:0] plus_110 = (a000010_naive_par_aux_76 + plus_109);
wire [17:0] plus_111 = (a000010_naive_par_aux_73 + plus_110);
wire [17:0] plus_112 = (a000010_naive_par_aux_70 + plus_111);
wire [17:0] plus_113 = (a000010_naive_par_aux_67 + plus_112);
wire [17:0] plus_114 = (a000010_naive_par_aux_64 + plus_113);
wire [17:0] plus_115 = (a000010_naive_par_aux_61 + plus_114);
wire [17:0] plus_116 = (a000010_naive_par_aux_58 + plus_115);
wire [17:0] plus_117 = (a000010_naive_par_aux_55 + plus_116);
wire [17:0] plus_118 = (a000010_naive_par_aux_52 + plus_117);
wire [17:0] plus_119 = (a000010_naive_par_aux_49 + plus_118);
wire [17:0] plus_120 = (a000010_naive_par_aux_46 + plus_119);
wire [17:0] plus_121 = (a000010_naive_par_aux_43 + plus_120);
wire [17:0] plus_122 = (a000010_naive_par_aux_40 + plus_121);
wire [17:0] plus_123 = (a000010_naive_par_aux_37 + plus_122);
wire [17:0] plus_124 = (a000010_naive_par_aux_34 + plus_123);
wire [17:0] plus_125 = (a000010_naive_par_aux_31 + plus_124);
wire [17:0] plus_126 = (a000010_naive_par_aux_28 + plus_125);
wire [17:0] plus_127 = (a000010_naive_par_aux_25 + plus_126);
wire [17:0] plus_128 = (a000010_naive_par_aux_22 + plus_127);
wire [17:0] plus_129 = (a000010_naive_par_aux_19 + plus_128);
wire [17:0] plus_130 = (a000010_naive_par_aux_16 + plus_129);
wire [17:0] plus_131 = (a000010_naive_par_aux_13 + plus_130);
wire [17:0] plus_132 = (a000010_naive_par_aux_10 + plus_131);
wire [17:0] if_7 = (tif_test_8 ? n_3 : plus_132);
wire if_7_ready = ((0 !=  tif_test_8) ? 1 : (a000010_naive_par_aux_10_ready & (a000010_naive_par_aux_13_ready & (a000010_naive_par_aux_16_ready & (a000010_naive_par_aux_19_ready & (a000010_naive_par_aux_22_ready & (a000010_naive_par_aux_25_ready & (a000010_naive_par_aux_28_ready & (a000010_naive_par_aux_31_ready & (a000010_naive_par_aux_34_ready & (a000010_naive_par_aux_37_ready & (a000010_naive_par_aux_40_ready & (a000010_naive_par_aux_43_ready & (a000010_naive_par_aux_46_ready & (a000010_naive_par_aux_49_ready & (a000010_naive_par_aux_52_ready & (a000010_naive_par_aux_55_ready & (a000010_naive_par_aux_58_ready & (a000010_naive_par_aux_61_ready & (a000010_naive_par_aux_64_ready & (a000010_naive_par_aux_67_ready & (a000010_naive_par_aux_70_ready & (a000010_naive_par_aux_73_ready & (a000010_naive_par_aux_76_ready & (a000010_naive_par_aux_79_ready & (a000010_naive_par_aux_82_ready & (a000010_naive_par_aux_85_ready & (a000010_naive_par_aux_88_ready & (a000010_naive_par_aux_91_ready & (a000010_naive_par_aux_94_ready & (a000010_naive_par_aux_97_ready & a000010_naive_par_aux_100_ready)))))))))))))))))))))))))))))));
wire tif_test_134 = (~| p_4);
wire [7:0] if_133 = (tif_test_134 ? 8'd58 : 8'd13);
wire tif_test_136 = (~| p_4);
wire [7:0] if_135 = (tif_test_136 ? 8'd32 : 8'd10);
reg seq2_6_started;
wire seq2_6_startable = if_7_ready;
wire seq2_6;
wire seq2_6_ready;
wire if_test_138 = (~| p_4);
wire bitnot_139 = (~ p_4);
wire if_test_140 = (n_3 < uplimit_1);
wire [17:0] plus_141 = (18'd1 + n_3);
wire bitnot_142 = (~ p_4);
reg [17:0] loop_loop_result_2;
wire loop_loop_result_2_ready = (loop_loop_state == st_loop_ready);

a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_11(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_9),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_10_ready),
        .result(a000010_naive_par_aux_10));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_14(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_12),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_13_ready),
        .result(a000010_naive_par_aux_13));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_17(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_15),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_16_ready),
        .result(a000010_naive_par_aux_16));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_20(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_18),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_19_ready),
        .result(a000010_naive_par_aux_19));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_23(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_21),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_22_ready),
        .result(a000010_naive_par_aux_22));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_26(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_24),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_25_ready),
        .result(a000010_naive_par_aux_25));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_29(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_27),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_28_ready),
        .result(a000010_naive_par_aux_28));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_32(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_30),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_31_ready),
        .result(a000010_naive_par_aux_31));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_35(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_33),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_34_ready),
        .result(a000010_naive_par_aux_34));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_38(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_36),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_37_ready),
        .result(a000010_naive_par_aux_37));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_41(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_39),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_40_ready),
        .result(a000010_naive_par_aux_40));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_44(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_42),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_43_ready),
        .result(a000010_naive_par_aux_43));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_47(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_45),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_46_ready),
        .result(a000010_naive_par_aux_46));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_50(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_48),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_49_ready),
        .result(a000010_naive_par_aux_49));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_53(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_51),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_52_ready),
        .result(a000010_naive_par_aux_52));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_56(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_54),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_55_ready),
        .result(a000010_naive_par_aux_55));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_59(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_57),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_58_ready),
        .result(a000010_naive_par_aux_58));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_62(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_60),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_61_ready),
        .result(a000010_naive_par_aux_61));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_65(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_63),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_64_ready),
        .result(a000010_naive_par_aux_64));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_68(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_66),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_67_ready),
        .result(a000010_naive_par_aux_67));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_71(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_69),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_70_ready),
        .result(a000010_naive_par_aux_70));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_74(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_72),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_73_ready),
        .result(a000010_naive_par_aux_73));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_77(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_75),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_76_ready),
        .result(a000010_naive_par_aux_76));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_80(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_78),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_79_ready),
        .result(a000010_naive_par_aux_79));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_83(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_81),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_82_ready),
        .result(a000010_naive_par_aux_82));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_86(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_84),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_85_ready),
        .result(a000010_naive_par_aux_85));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_89(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_87),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_88_ready),
        .result(a000010_naive_par_aux_88));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_92(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_90),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_91_ready),
        .result(a000010_naive_par_aux_91));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_95(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_93),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_94_ready),
        .result(a000010_naive_par_aux_94));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_98(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_96),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_97_ready),
        .result(a000010_naive_par_aux_97));



a000010_naive_par_aux_18_18_18_18 a000010_naive_par_aux_101(
        .clk(clk),
        .start(loop_loop_restarted),
        .n(n_3),
        .start_offset(i_99),
        .increment(18'd31),
        .result_ready(a000010_naive_par_aux_100_ready),
        .result(a000010_naive_par_aux_100));



outdec_with_2_postdelims_1_1_18_8_8 outdec_with_2_postdelims_137(
        .clk(clk),
        .start((loop_loop_waiting & seq2_6_startable & (~ seq2_6_started))),
        .outchan(out_uart_txd),
        .n(if_7),
        .delim1byte(if_133),
        .delim2byte(if_135),
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
                    n_3 <= 18'd0;
                    p_4 <= 1'd0;
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
                    begin
                        if ((seq2_6_ready & seq2_6_started)) 
                            if (if_test_138) 
                                begin
                                    loop_loop_state <= st_loop_restarted;
                                    n_3 <= n_3;
                                    p_4 <= bitnot_139;
                                end
                            else
                                if (if_test_140) 
                                    begin
                                        loop_loop_state <= st_loop_restarted;
                                        n_3 <= plus_141;
                                        p_4 <= bitnot_142;
                                    end
                                else
                                    begin
                                        loop_loop_result_2 <= n_3;
                                        loop_loop_state <= st_loop_ready;
                                    end
                    end
                end
        endcase
end

assign result_ready = (loop_loop_result_2_ready & (~ start));
endmodule
