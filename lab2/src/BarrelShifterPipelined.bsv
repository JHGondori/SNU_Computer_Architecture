import Multiplexer::*;
import FIFO::*;
import FIFOF::*;
import Vector::*;
import SpecialFIFOs::*;

/* Interface of the basic right shifter module */
interface BarrelShifterRightPipelined;
	method Action shift_request(Bit#(64) operand, Bit#(6) shamt, Bit#(1) val);
	method ActionValue#(Bit#(64)) shift_response();
endinterface

module mkBarrelShifterRightPipelined(BarrelShifterRightPipelined);
	/* use mkFIFOF for request-response interface.	*/
	let inFifo <- mkFIFOF;
	let outFifo <- mkFIFOF;
	let fifo1 <- mkFIFOF;
	let fifo2 <- mkFIFOF;
	let fifo3 <- mkFIFOF;
	let fifo4 <- mkFIFOF;
	let fifo5 <- mkFIFOF;
	function f0(x);
		return tuple3(multiplexer_n(tpl_2(x)[0], tpl_1(x), {(tpl_3(x) == 0) ? '0: '1, tpl_1(x)[63:1]}), tpl_2(x), tpl_3(x));
	endfunction

	function f1(x);
		return tuple3(multiplexer_n(tpl_2(x)[1], tpl_1(x), {(tpl_3(x) == 0) ? '0: '1, tpl_1(x)[63:2]}), tpl_2(x), tpl_3(x));
	endfunction

	function f2(x);
		return tuple3(multiplexer_n(tpl_2(x)[2], tpl_1(x), {(tpl_3(x) == 0) ? '0: '1, tpl_1(x)[63:4]}), tpl_2(x), tpl_3(x));
	endfunction

	function f3(x);
		return tuple3(multiplexer_n(tpl_2(x)[3], tpl_1(x), {(tpl_3(x) == 0) ? '0: '1, tpl_1(x)[63:8]}), tpl_2(x), tpl_3(x));
	endfunction

	function f4(x);
		return tuple3(multiplexer_n(tpl_2(x)[4], tpl_1(x), {(tpl_3(x) == 0) ? '0: '1, tpl_1(x)[63:16]}), tpl_2(x), tpl_3(x));
	endfunction

	function f5(x);
		return tuple3(multiplexer_n(tpl_2(x)[5], tpl_1(x), {(tpl_3(x) == 0) ? '0: '1, tpl_1(x)[63:32]}), tpl_2(x), tpl_3(x));
	endfunction

	rule stage1;
		fifo1.enq(f0(inFifo.first));
		inFifo.deq();
	endrule

	rule stage2;
		fifo2.enq(f1(fifo1.first));
		fifo1.deq();
	endrule

	rule stage3;
		fifo3.enq(f2(fifo2.first));
		fifo2.deq();
	endrule

	rule stage4;
		fifo4.enq(f3(fifo3.first));
		fifo3.deq();
	endrule

	rule stage5;
		fifo5.enq(f4(fifo4.first));
		fifo4.deq();
	endrule

	rule stage6;
		outFifo.enq(tpl_1(f5(fifo5.first)));
		fifo5.deq();
	endrule

	method Action shift_request(Bit#(64) operand, Bit#(6) shamt, Bit#(1) val);
		inFifo.enq(tuple3(operand, shamt, val));
	endmethod

	method ActionValue#(Bit#(64)) shift_response();
		outFifo.deq;
		return outFifo.first;
	endmethod
endmodule


/* Interface of the three shifter modules
 *
 * They have the same interface.
 * So, we just copy it using typedef declarations.
 */
interface BarrelShifterRightLogicalPipelined;
	method Action shift_request(Bit#(64) operand, Bit#(6) shamt);
	method ActionValue#(Bit#(64)) shift_response();
endinterface

typedef BarrelShifterRightLogicalPipelined BarrelShifterRightArithmeticPipelined;
typedef BarrelShifterRightLogicalPipelined BarrelShifterLeftPipelined;

module mkBarrelShifterLeftPipelined(BarrelShifterLeftPipelined);
	/* TODO: Implement left shifter using the pipelined right shifter. */
	let bsrp <- mkBarrelShifterRightPipelined;

	method Action shift_request(Bit#(64) operand, Bit#(6) shamt);
		bsrp.shift_request(reverseBits(operand), shamt, 0);
	endmethod

	method ActionValue#(Bit#(64)) shift_response();
		let result <- bsrp.shift_response();
		result = reverseBits(result);
		return result;
	endmethod
endmodule

module mkBarrelShifterRightLogicalPipelined(BarrelShifterRightLogicalPipelined);
	/* TODO: Implement right logical shifter using the pipelined right shifter. */
	let bsrp <- mkBarrelShifterRightPipelined;

	method Action shift_request(Bit#(64) operand, Bit#(6) shamt);
		bsrp.shift_request(operand, shamt, 0);
	endmethod

	method ActionValue#(Bit#(64)) shift_response();
		let result <- bsrp.shift_response();
		return result;
	endmethod
endmodule

module mkBarrelShifterRightArithmeticPipelined(BarrelShifterRightArithmeticPipelined);
	/* TODO: Implement right arithmetic shifter using the pipelined right shifter. */
	let bsrp <- mkBarrelShifterRightPipelined;

	method Action shift_request(Bit#(64) operand, Bit#(6) shamt);
		bsrp.shift_request(operand, shamt, operand[63]);
	endmethod

	method ActionValue#(Bit#(64)) shift_response();
		let result <- bsrp.shift_response();
		return result;
	endmethod
endmodule
