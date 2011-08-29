
// Spartan-6 in our ATLYS is running by default at 100 MHz, 16 cycles per bit,
// so we divide it with 54, to get 1200 bps:

// 100000000.0/(16*54) = 115740.74074074074
// which is 1.0046939300411524 * 115200, i.e. only half percent too fast.

`define UART_BPS_RATE_DIVIDER 12'd54 // For ATLYS running (be default) on 100MHz.

