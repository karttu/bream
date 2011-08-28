//
// Verilog-source for Bream-function (wait_until_start_debounced)
// Copyright (C) 2010-2011 Antti Karttunen, subject to the terms of the GPL v2.
//

/* 
   Modified from module debounce.v - also by karttu, 12.12.2004,

// that was in turn based on the description given at:
// http://groups-beta.google.com/group/comp.lang.vhdl/messages/06dceec75f8ca08e,93d2ccf324a87543,ba3d805dcf093614,afeaec4c5fdc6004,bb912762f254318b?thread_id=1a429c2092c1b3e3&view=thread&noheader=1
// Especially the message from Rick Collins:
//
// The second way is to use a single pole single throw switch and any fast
// (>100 Hz) clock and require that the switch be in a given state for some
// minimum amount of time before changing the output. The idea is to wait
// out the bouncing time before changing the output. For example if you
// were trying to detect when the user pushes a momentary push button, you
// might use a 1 KHz clock and a seven bit counter. The counter is reset
// any time the button is not pushed. When the button is pushed, the
// counter is allowed to run, getting reset when ever the switch bounces.
// After the switch has stopped bouncing for 128 counts, (you can assume
// that it won't bounce any more) and the counter reaches max count. The
// counter then outputs a terminal count signal and stops counting. When
// the button switch is released, the counter is reset and the TC output
// goes away. If this is not long enough to debounce the switch, use a
// slower clock or a longer counter.

 */

module wait_until_start_debounced_1(input clk,
                                    input start, // The bouncing start button.
                                    output result,
                                    output result_ready
                                   );


reg DEB_OUT = 0;

assign result_ready = DEB_OUT;
assign result = DEB_OUT;

// 2^23 x 10 ns = 83886080 ns =~ 1/12 s. For 100MHz clock.
// 2^23 x 20 ns = 167772160 ns =~ 1/6 s. For 50MHz clock.

parameter msb = 22;
reg [msb:0] delay_counter = 1;

always @(posedge clk)
  begin
   if(~start)
    begin
     DEB_OUT <= 0;
     delay_counter <= 1;
    end
   else if(delay_counter)
    begin
     DEB_OUT <= 0;
     delay_counter <= delay_counter + 1;
    end
   else // delay_counter has wrapped over,
    begin // so the button has settled to ON state.
     DEB_OUT <= 1;
    end
  end

endmodule
