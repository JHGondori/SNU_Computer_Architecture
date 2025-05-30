import Types::*;
import ProcTypes::*;
import CMemTypes::*;
import RFile::*;
import IMemory::*;
import DMemory::*;
import Decode::*;
import Exec::*;
import CsrFile::*;
import Vector::*;
import Fifo::*;
import Ehr::*;
import GetPut::*;

typedef struct {
  Instruction inst;
  Addr pc;
  Addr ppc;
  Bool epoch;
} Fetch2Decode deriving(Bits, Eq);

typedef struct {
  DecodedInst dInst;
  Addr pc;
  Addr ppc;
  Bool epoch;
} Decode2Rest deriving(Bits, Eq);

(*synthesize*)
module mkProc(Proc);
  Reg#(Addr)    pc  <- mkRegU;
  RFile         rf  <- mkRFile;
  IMemory     iMem  <- mkIMemory;
  DMemory     dMem  <- mkDMemory;
  CsrFile     csrf <- mkCsrFile;

  Reg#(ProcStatus)   stat		<- mkRegU;
  Fifo#(1,ProcStatus) statRedirect <- mkBypassFifo;

  // Control hazard handling Elements
  Reg#(Bool) fEpoch <- mkRegU;
  Reg#(Bool) eEpoch <- mkRegU;

  Fifo#(1, Addr)         execRedirect <- mkBypassFifo;

  Fifo#(2, Fetch2Decode)  f2d <- mkPipelineFifo;

  Fifo#(2, Decode2Rest) d2r <- mkPipelineFifo;

  Bool memReady = iMem.init.done() && dMem.init.done();
  rule test (!memReady);
    let e = tagged InitDone;
    iMem.init.request.put(e);
    dMem.init.request.put(e);
  endrule
/* Exercise 1 주석
  rule doFetch(csrf.started && stat == AOK);
   let inst = iMem.req(pc);
   	let ppc = pc + 4;
    if(execRedirect.notEmpty) begin
      execRedirect.deq;
      pc <= execRedirect.first;
      fEpoch <= !fEpoch;
    end
    else begin
      pc <= ppc;
    end
    f2d.enq(Fetch2Decode{inst:inst, pc:pc, ppc:ppc, epoch:fEpoch}); 
    $display("Fetch : from Pc %d , \n", pc);
  endrule
*/

  rule doFetch(csrf.started && stat == AOK);
	/* Exercise_2 */
	/*TODO: 
	Remove 1-cycle inefficiency when execRedirect is used. */
    let nextPc;
    let nextEpoch = fEpoch;

    if (execRedirect.notEmpty) begin
      nextPc = execRedirect.first;
      nextEpoch = !fEpoch;
      execRedirect.deq;
    end
    else begin
      nextPc = pc;
    end

    let inst = iMem.req(nextPc);
    let ppc = nextPc + 4;

    f2d.enq(Fetch2Decode{inst: inst, pc: nextPc, ppc: ppc, epoch: nextEpoch});

    pc <= ppc;
    fEpoch <= nextEpoch;

    $display("Fetch : from Pc %d , \n", nextPc);
  endrule

  rule doDecode(csrf.started && stat == AOK);
    let f = f2d.first;
    f2d.deq;
    let dInst = decode(f.inst);
    $display(fshow(dInst));
    d2r.enq(Decode2Rest{dInst:dInst, pc:f.pc, ppc:f.ppc, epoch:f.epoch});
  
  endrule

  rule doRest(csrf.started && stat == AOK);
	/* Exercise_1 */
	/* TODO: 
	Divide the doRest rule into doDecode, doRest rules 
	to implement 3-stage pipelined processor */

    let dInst   = d2r.first.dInst;
    let pc   = d2r.first.pc;
    let ppc    = d2r.first.ppc;
    let iEpoch = d2r.first.epoch;
    d2r.deq;

    if(iEpoch == eEpoch) begin
        // Register Read 
        let rVal1 = isValid(dInst.src1) ? rf.rd1(validValue(dInst.src1)) : ?;
        let rVal2 = isValid(dInst.src2) ? rf.rd2(validValue(dInst.src2)) : ?;
        let csrVal = isValid(dInst.csr) ? csrf.rd(validValue(dInst.csr)) : ?;

    		// Execute         
        let eInst = exec(dInst, rVal1, rVal2, pc, ppc, csrVal);       
        $display(fshow(eInst));        
        
        if(eInst.mispredict) begin
          eEpoch <= !eEpoch;
          execRedirect.enq(eInst.addr);
       $display("jump! :mispredicted, address %d ", eInst.addr);
        end

      //Memory 
      let iType = eInst.iType;
      case(iType)
        Ld :
        begin
          let d <- dMem.req(MemReq{op: Ld, addr: eInst.addr, data: ?});
          eInst.data = d;
        end

        St:
        begin
          let d <- dMem.req(MemReq{op: St, addr: eInst.addr, data: eInst.data});
        end
        Unsupported :
        begin
          $fwrite(stderr, "ERROR: Executing unsupported instruction\n");
          $finish;
        end
      endcase

      //WriteBack 
      if (isValid(eInst.dst)) begin
          rf.wr(fromMaybe(?, eInst.dst), eInst.data);
      end
      csrf.wr(eInst.iType == Csrw ? eInst.csr : Invalid, eInst.data);

      
	  /* Exercise_3 */
	  /* TODO:
	  1. count the number of each instruciton type
	    Ctr(Control)   : J, Jr, Br
	    Mem(Memory)    : Ld, St 
	        
	  2. count the number of mispredictions */   
      case(iType)
        J: csrf.incInstTypeCnt(Ctr);
        Jr: csrf.incInstTypeCnt(Ctr);
        Br: csrf.incInstTypeCnt(Ctr);
        Ld: csrf.incInstTypeCnt(Mem);
        St: csrf.incInstTypeCnt(Mem);
      endcase

      if(execRedirect.notEmpty) begin
          csrf.incBPMissCnt();
        end




	/* Exercise_4 */
	  /* TODO:
	  1. Implement incInstTypeCnt method in CsrFile.bsv
	        
	  2. count number of mispredictions for each instruction type */   
      case(iType)
        J: begin
          if(execRedirect.notEmpty) csrf.incMissInstTypeCnt(MissJ);
          else csrf.incMissInstTypeCnt(J);
        end
        Jr: begin
          if(execRedirect.notEmpty) csrf.incMissInstTypeCnt(MissJR);
          else csrf.incMissInstTypeCnt(JR);
        end
        Br: begin 
          if(execRedirect.notEmpty) csrf.incMissInstTypeCnt(MissBR);
          else csrf.incMissInstTypeCnt(BR);
        end
      endcase

  end
  endrule

  rule upd_Stat(csrf.started);
	$display("Stat update");
  	statRedirect.deq;
    stat <= statRedirect.first;
  endrule

  method ActionValue#(CpuToHostData) cpuToHost;
    let retV <- csrf.cpuToHost;
    return retV;
  endmethod

  method Action hostToCpu(Bit#(32) startpc) if (!csrf.started && memReady);
    csrf.start(0);
    eEpoch <= False;
    fEpoch <= False;
    pc <= startpc;
    stat <= AOK;
  endmethod

  interface iMemInit = iMem.init;
  interface dMemInit = dMem.init;

endmodule
