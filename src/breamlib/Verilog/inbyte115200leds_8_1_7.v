//
// Verilog-source for Bream-function 8'(inbyte115200leds inchan 7'statusleds)
// Copyright (C) 2010-2011 Antti Karttunen.
//
// Uses uart_receiver module of the "UART 16550 compatible" project
// of Jacob Gorban & Igor Mohor. See: http://www.opencores.org/cores/uart16550/
//
// Edited    Sep 04 2011 by karttu (Antti Karttunen).
//   Started modifying from outbyte115200_1_1_8.v module.

//
// Unless otherwise mentioned, all the files in this code tree are
// Copyright (C) 2010-2011 Antti Karttunen, subject to the terms of the GPL v2.
// See the file COPYING for more information.  Contact the author for
// further developments at the address: <his-firstname>.<his-surname>@gmail.com
//
//

// Note that (at least with Xilinx's FPGAs) we don't need a separate
// global reset-line, as we can have a register has_been_called_at_least_once
// auto-initialized to zero, and then use that and the value of start-signal
// to generate the reset signal later for Uart and its FIFO-components.

// Some later version could take also bps_rate_divider as an input-variable.

// This version shows some status-info on 7 leds.

module inbyte115200leds_8_1_7(input clk,
                              input start,
                              input inchan, 
                              output [6:0] statusleds,
                              output [7:0] result,
                              output result_ready
                             );


reg has_been_called_at_least_once = 0; // XXX - Works with Xilinx, but elsewhere?

// ST_READY changes to ST_JUST_RESETED if this has been called never before,
// otherwise directly to ST_WAITING.
// From ST_JUST_RESETED we switch to ST_WAITING after one cycle
// (enough to reset the stuff in uart_receiver).
// From ST_WAITING we switch either to ST_READ_BYTE if there's something
// to read in receiver-fifo, or stay in the same state.
// From ST_READ_BYTE we switch back to ST_READY when the byte read
// has been copied to result.

parameter ST_READY        = 2'b00;
parameter ST_WAITING      = 2'b10;
parameter ST_READ_BYTE    = 2'b01;
parameter ST_JUST_RESETED = 2'b11;


reg [1:0] top_state = ST_READY;


//  Include uart_defines.v file of "UART 16550 compatible" project
//  ( http://www.opencores.org/cores/uart16550/ )
//  Because we need UART_FIFO_DEPTH definition, to check _proactively_
//  that the transmitter fifo will not become full.

`include "others/uart_defines.v"

parameter fifo_width = `UART_FIFO_WIDTH;
parameter fifo_depth = `UART_FIFO_DEPTH;
parameter fifo_pointer_w = `UART_FIFO_POINTER_W;
parameter fifo_counter_w = `UART_FIFO_COUNTER_W;
parameter fifo_rec_item_w = `UART_FIFO_REC_WIDTH;

wire [fifo_counter_w-1:0] rec_queue_count; // Comes from receiver fifo.

// These are valid only one clock cycle after the pop signal:
wire [fifo_rec_item_w-1:0] item_at_the_bottom_of_fifo;
wire [7:0] byte_at_the_bottom_of_fifo
  = item_at_the_bottom_of_fifo[fifo_rec_item_w-1:3];

reg [7:0] byte_to_return = 8'd65; // Avoid "prestart ghost returns"!
assign result = byte_to_return;
assign result_ready = ((ST_READY == top_state)&(~start));

wire ft_reset = (start & ~has_been_called_at_least_once);

reg enable_for_uart = 0;


// Set UART_LC_DL, UART_LC_BC, UART_LC_SP, UART_LC_EP, UART_LC_PE & UART_LC_SB as zeros.
// We use 8 data bits, no parity, one stop bit. (8N1).
parameter UART_LCR_8N1 = 8'b00000011;

`include "uart2defines.v"
parameter bps_rate_divider = `UART_BPS_RATE_DIVIDER;

reg [11:0] dlc = 12'b000000000000;


wire something_to_read = ((rec_queue_count>0));
wire pop_signal = (something_to_read & has_been_called_at_least_once
                   & (start | (ST_WAITING == top_state)));


uart_receiver UART_REC(.clk(clk),
                       .wb_rst_i(ft_reset),
                       .lcr(UART_LCR_8N1),
                       .rf_pop(pop_signal),
                       .srx_pad_i(inchan),
                       .enable(enable_for_uart),
                       .counter_t(), // We don't use timeouts.
                       .rf_count(rec_queue_count), // From fifo.
                       .rf_data_out(item_at_the_bottom_of_fifo),
                       .rf_error_bit(statusleds[6]), // An output signal
                       .rf_overrun(statusleds[5]),     // Ditto.
                       .rx_reset(ft_reset),
                       .lsr_mask(ft_reset),
                       .rstate(), // Not connected now.
                       .rf_push_pulse() // Ditto.
//                     .rstate(statusleds[3:0]), // This is an output signal.
//                     .rf_push_pulse(statusleds[4]) // Ditto.
                      );

// This is more useful information for debugging:
assign statusleds[fifo_counter_w-1:0] = rec_queue_count;


always @(posedge clk)
begin
   if(start) // Blunt start, takes effect on any state.
     begin
       if(~has_been_called_at_least_once)
         begin
           top_state <= ST_JUST_RESETED;
         end
       else // Not the first time here.
         begin
           if(something_to_read) byte_to_return <= byte_at_the_bottom_of_fifo;
           top_state <= (something_to_read ? ST_READY : ST_WAITING);
         end
     end
   else
     case(top_state)
       ST_READY: // Do nothing, as everything is ready.
         begin
         end
       ST_JUST_RESETED:
         begin
           top_state <= ST_WAITING;
           has_been_called_at_least_once <= 1;
         end
       ST_WAITING:
         begin
           if(something_to_read) byte_to_return <= byte_at_the_bottom_of_fifo;
           top_state <= (something_to_read ? ST_READY : ST_WAITING);
         end
     endcase
end


// If uart_rfifo.v REALLY were as sluggish as it claims in the comment:
// "please note though that data_out is only valid one clock after pop signal"
// next to statement: assign data_out = {data8_out,fifo[bottom]};
//
// THEN this would (probably) work allright, instead of a delay of 16 chars:
//
// always @(posedge clk)
// begin
//    if(start) // Blunt start, takes effect on any state.
//      begin
//        if(~has_been_called_at_least_once)
//          begin
//            top_state <= ST_JUST_RESETED;
//          end
//        else // Not the first time here.
//          begin
//            top_state <= (something_to_read ? ST_READ_BYTE : ST_WAITING);
//          end
//      end
//    else
//      case(top_state)
//        ST_JUST_RESETED:
//          begin
//            top_state <= ST_WAITING;
//            has_been_called_at_least_once <= 1;
//          end
//        ST_WAITING:
//          begin
//            top_state <= (something_to_read ? ST_READ_BYTE : ST_WAITING);
//          end
//        ST_READ_BYTE:
//          begin
//            byte_to_return <= byte_at_the_bottom_of_fifo;
//            top_state <= ST_READY;
//          end
//      endcase
// end
// 


// Frequency divider
always @(posedge clk) 
begin
   if (ft_reset || (0==dlc)) dlc <= bps_rate_divider - 12'b1;
   else dlc <= dlc - 12'b1; // Decrement counter
end

// Enable signal generation logic
always @(posedge clk)
begin
   if(ft_reset) enable_for_uart <= 1'b0;
   else if((0!=bps_rate_divider) && (0==dlc)) enable_for_uart <= 1'b1;
   else	enable_for_uart <= 1'b0;
end

endmodule
