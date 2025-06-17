/*

Copyright (C) 2012 Muralidaran Vijayaraghavan <vmurali@csail.mit.edu>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/


import Types::*;
import ProcTypes::*;
import Ehr::*;
import ConfigReg::*;
import Fifo::*;

/*Exercise 4*/
typedef enum {Ctr, Mem} InstCntType deriving(Bits, Eq);
typedef enum {HitJ, HitJr, HitBr, MissJ, MissJr, MissBr} InstMissCntType deriving(Bits, Eq);


interface CsrFile;
    method Action start(Data id);
    method Bool started;
    method Data rd(CsrIndx idx);
    method Action wr(Maybe#(CsrIndx) idx, Data val);
    method ActionValue#(CpuToHostData) cpuToHost;
    /*Exercise 4*/
    method Action incInstTypeCnt(InstCntType inst);
    method Action incBPMissCnt();
    method Action incMissInstTypeCnt(InstMissCntType inst);


endinterface

(* synthesize *)
module mkCsrFile(CsrFile);
    Reg#(Bool) startReg <- mkConfigReg(False);

	// CSR 
    Reg#(Data) numInsts <- mkConfigReg(0); // csrInstret -- read only
    Reg#(Data) cycles <- mkReg(0); // csrCycle -- read only
	Reg#(Data) coreId <- mkConfigReg(0); // csrMhartid -- read only
    
    /*Exercise 4*/
    Reg#(Data) numMem  <- mkConfigReg(0);
    Reg#(Data) numCtr  <- mkConfigReg(0);
    Reg#(Data) numBPMiss <- mkConfigReg(0);
    // Count BP Hit for Instruction Type
    Reg#(Data) numHitJ  <- mkConfigReg(0);
    Reg#(Data) numHitJr  <- mkConfigReg(0);
    Reg#(Data) numHitBr <- mkConfigReg(0);    
    // Count BP Miss for Instruction Type
    Reg#(Data) numMissJ  <- mkConfigReg(0);
    Reg#(Data) numMissJr  <- mkConfigReg(0);
    Reg#(Data) numMissBr <- mkConfigReg(0);


    Fifo#(2, CpuToHostData) toHostFifo <- mkCFFifo; // csrMtohost -- write only
    Fifo#(2, Tuple3#(CsrIndx, Data, Data)) csrFifo <- mkCFFifo;

    rule count (startReg);
        cycles <= cycles + 1;
        $display("\nCycle %d ----------------------------------------------------", cycles);
    endrule

    method Action start(Data id) if(!startReg);
        startReg <= True;
        cycles <= 0;
		coreId <= id;
    endmethod

    method Bool started;
        return startReg;
    endmethod

    method Data rd(CsrIndx idx);
        return (case(idx)
                    csrCycle: cycles;
                    csrInstret: numInsts;
                    csrMhartid: coreId;
					default: ?;
                endcase);
    endmethod
    
    method Action wr(Maybe#(CsrIndx) csrIdx, Data val);
        if(csrIdx matches tagged Valid .idx) begin
            case (idx)
                csrMtohost: begin

                    /*Exercise 4*/
                    $fwrite(stderr, "===========================\n");
                    $fwrite(stderr, "Specific type of executed instructions\n");
                    $fwrite(stderr, "Ctr              : %d\n", numCtr);
                    $fwrite(stderr, "Mem              : %d\n", numMem);
                    $fwrite(stderr, "\nMispredicted       : %d\n", numBPMiss);
                    $fwrite(stderr, "Misprediction detail\n");
                    $fwrite(stderr, "J               : %d / %d\n", numMissJ, numHitJ + numMissJ);
                    $fwrite(stderr, "JR              : %d / %d\n", numMissJr, numMissJr + numMissJr);
                    $fwrite(stderr, "BR              : %d / %d\n", numMissBr, numMissBr + numMissBr);
                    
                        
                    // high 16 bits encodes type, low 16 bits are data
                    Bit#(16) hi = truncateLSB(val);
                    Bit#(16) lo = truncate(val);
                    toHostFifo.enq(CpuToHostData {
                        c2hType: unpack(truncate(hi)),
                        data: lo,
                        data2: numInsts
                    });
                end
            endcase
        end
        numInsts <= numInsts + 1;
    endmethod


        /*Exercise 4*/
    method Action incInstTypeCnt(InstCntType inst);
      case(inst)
        Ctr : numCtr <= numCtr + 1;
        Mem : numMem <= numMem + 1;
        endcase
    endmethod

    method Action incMissInstTypeCnt(InstMissCntType inst);
        /* TODO: Implement incMissInstTypeCnt */
        //noAction;
        case(inst)
          HitJ : numHitJ <= numHitJ + 1;
          HitJr : numHitJr <= numHitJr + 1;
          HitBr : numHitBr <= numHitBr + 1;

          MissJ : numMissJ <= numMissJ + 1;
          MissJr : numMissJr <= numMissJr + 1;
          MissBr : numMissBr <= numMissBr + 1;
       endcase
    endmethod

    method Action incBPMissCnt();
      numBPMiss <= numBPMiss + 1;
    endmethod

    method ActionValue#(CpuToHostData) cpuToHost;
        toHostFifo.deq;
        return toHostFifo.first;
    endmethod

endmodule