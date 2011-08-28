
//
// Verilog-source for Bream-function (wait_until boolearn'condition)
// Copyright (C) 2010-2011 Antti Karttunen, subject to the terms of the GPL v2.
//

// This should be used mainly for "timing side effects", as a non-last
// call in seq, for instance: (result is discarded).

module wait_until_1_1(input clk,
                      input start,
                      input condition,
                      output result,
                      output result_ready
                     );

assign result_ready = (condition&(~start));
assign result = condition; // Shouldn't be used, might vacillate.

endmodule
