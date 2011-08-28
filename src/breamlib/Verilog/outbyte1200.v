//
// Verilog-source for Bream-function (V:outbyte1200 >outchan byte).
// Copyright (C) 2010 Antti Karttunen.
//
// Edited    Sep 21 2010 by karttu (Antti Karttunen).
//   The first working version. Now the autoreset works as well.
//
// Unless otherwise mentioned, all the files in this code tree are
// Copyright (C) 2010 Antti Karttunen, and subject to the terms of the GPL v2.
// See the file COPYING for more information.  Contact the author for
// further developments at the address: <his-firstname>.<his-surname>@gmail.com
//
//

// Note that (at least with Xilinx's FPGAs) we don't need a separate
// global reset-line, as we can have a register has_been_called_at_least_once
// auto-initialized to zero, and then use that and the value of start-signal
// to generate the reset signal later for Uart and its FIFO-components.

// Some later version will take also bps_rate_divider as an input-variable.

module outbyte1200(input clk,
                   input start,
                   output outchan, 
                   input [7:0] byte_to_output,
                   output result,
                   output result_ready
                  );


reg has_been_called_at_least_once = 0; // XXX - Works with Xilinx, but elsewhere?

// ST_READY changes to ST_JUST_RESETED if this has been called never before,
// otherwise directly to ST_WAITING.
// From ST_JUST_RESETED we switch to ST_WAITING after one cycle
// (enough to reset the stuff in uart_transmiteer).
// From ST_WAITING we switch back to ST_READY when uart_ready is high.
// From ST_NEVERNEVER we always switch back to ST_READY.

parameter ST_READY        = 2'b00;
parameter ST_JUST_RESETED = 2'b01;
parameter ST_WAITING      = 2'b10;
parameter ST_NEVERNEVER   = 2'b11;


reg [1:0] top_state = ST_READY;

assign result = has_been_called_at_least_once; // Just something.
assign result_ready = ((ST_READY == top_state)&(~start));

reg [7:0] copy_of_byte_to_output;

wire ft_reset = (start & ~has_been_called_at_least_once);

// Push byte to fifo, either
// (A) immediately when the start signal rises, (if ever called before), or
// (B) at next cycle, so we have time to do the reseting.
wire push_byte_to_fifo = (has_been_called_at_least_once ? start : (ST_JUST_RESETED == top_state));
wire [7:0] byte_pushed = (has_been_called_at_least_once ? byte_to_output : copy_of_byte_to_output);

reg enable_for_uart = 0;
wire [2:0] uart_state;


// Set UART_LC_DL, UART_LC_BC, UART_LC_SP, UART_LC_EP, UART_LC_PE & UART_LC_SB as zeros.
// We use 8 data bits, no parity, one stop bit. (8N1).
parameter UART_LCR_8N1 = 8'b00000011;

// FPGA is running at 50 MHz, 16 cycles per bit,
// so we divide it with 2604, to get 1200 bps:
parameter bps_rate_divider = 12'd2604;

reg [11:0] dlc = 12'b000000000000;

uart_transmitter UART1200(.clk(clk),
                          .wb_rst_i(ft_reset),
                          .lcr(UART_LCR_8N1),
                          .tf_push(push_byte_to_fifo),
                          .wb_dat_i(byte_pushed),
                          .enable(enable_for_uart),
                          .stx_pad_o(outchan),
                          .tstate(uart_state), // This is an output signal.
                          .tf_count(), // As well. This one not connected.
                          .tx_reset(ft_reset),
                          .lsr_mask(ft_reset)
                         );


wire uart_ready = (~|uart_state);


always @(posedge clk)
begin
   if(start) // Blunt start, takes effect on any state.
     begin
       copy_of_byte_to_output <= byte_to_output;
       if(~has_been_called_at_least_once)
         begin
           top_state <= ST_JUST_RESETED;
         end
       else // Not the first time here.
         begin
           top_state <= ST_WAITING;
         end
     end
   else
     case(top_state)
       ST_JUST_RESETED:
         begin
           top_state <= ST_WAITING;
           has_been_called_at_least_once <= 1;
         end
       ST_WAITING: top_state <= (uart_ready ? ST_READY : ST_WAITING);
       ST_NEVERNEVER: // Shouldn't happen. Let's do the reset the next time.
         begin
           top_state <= ST_READY;
           has_been_called_at_least_once <= 0; // Better to reset everything.
         end
     endcase

end


// Frequency divider
always @(posedge clk) 
begin
   if (ft_reset || (0==dlc)) dlc <= bps_rate_divider - 1;
   else dlc <= dlc - 1; // Decrement counter
end

// Enable signal generation logic
always @(posedge clk)
begin
   if(ft_reset) enable_for_uart <= 1'b0;
   else if((0!=bps_rate_divider) && (0==dlc)) enable_for_uart <= 1'b1;
   else	enable_for_uart <= 1'b0;
end

endmodule
