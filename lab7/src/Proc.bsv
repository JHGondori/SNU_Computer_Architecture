import Types::*;
import ProcTypes::*;
import CMemTypes::*;
import RFile::*;
import IMemory::*;
import Decode::*;
import Exec::*;
import CsrFile::*;
import Vector::*;
import Fifo::*;
import Ehr::*;
import GetPut::*;
import CacheTypes::*; 
import LocalCache::*; 
import Memory::*; 
import Connectable::*; 



typedef struct {
  Instruction inst;
  Addr pc;
  Addr ppc;
  Bool epoch;
} Fetch2Decode deriving(Bits, Eq);

typedef struct {
  Addr pc;
  Addr ppc;
  DecodedInst dInst;
  Bool epoch;
} Decode2Rest deriving (Bits, Eq);

typedef ExecInst Rest2Memory;


(*synthesize*)
module mkProc(Proc);
  Reg#(Addr)    pc  <- mkRegU;
  RFile         rf  <- mkRFile;
  IMemory     iMem  <- mkIMemory;
  Cache		dCache <- mkCache; 
  Memory     mem  <- mkMemory; 
  CsrFile     csrf <- mkCsrFile;

  Reg#(ProcStatus)   stat		<- mkRegU;
  Fifo#(1,ProcStatus) statRedirect <- mkBypassFifo;


  Reg#(Bool) fEpoch <- mkRegU;
  Reg#(Bool) eEpoch <- mkRegU;

  Fifo#(1, Addr)         execRedirect <- mkBypassFifo;

  Fifo#(2, Fetch2Decode)  f2d <- mkPipelineFifo;
  Fifo#(2,Decode2Rest) d2r <- mkPipelineFifo;
  Fifo#(1, Rest2Memory) m12m2 <- mkBypassFifo; 

  //Bool memReady = iMem.init.done() && dMem.init.done(); 
  Bool memReady = iMem.init.done();
  rule test (!memReady);
    let e = tagged InitDone;
    iMem.init.request.put(e);
  endrule


  rule doFetch(csrf.started && stat == AOK);
    let updatedPC = pc;
    let updatedfEpoch = fEpoch;

    if(execRedirect.notEmpty) begin
      execRedirect.deq;
      updatedPC = execRedirect.first;
      fEpoch <= !fEpoch;
      updatedfEpoch = !fEpoch;
      
      $display("jump! : datahazard -> fetch side\n");
      $display("address : %d",updatedPC);
    end
    
    let inst = iMem.req(updatedPC);
    let ppc = updatedPC + 4;
    
    pc <= ppc;

    f2d.enq(Fetch2Decode{inst: inst, pc: updatedPC, ppc: ppc, epoch: updatedfEpoch});
    $display("Fetch : from Pc %d , updatedPC: %d, inst : %x, \n", pc, updatedPC, showInst(inst)); 
  endrule

  rule doDecode(csrf.started && stat == AOK);
    let inst = f2d.first.inst;
    let pc = f2d.first.pc;
    let ppc = f2d.first.ppc;
    let iEpoch = f2d.first.epoch;
    f2d.deq;

    if(iEpoch == eEpoch)
    begin
      let dInst = decode(inst);
      d2r.enq(Decode2Rest{dInst:dInst, pc:pc, ppc:ppc, epoch:iEpoch});
    end

    $display("do decoded");
  endrule

  rule doRest(csrf.started && stat == AOK);
    let dInst   = d2r.first.dInst;
    let pc   = d2r.first.pc;
    let ppc    = d2r.first.ppc;
    let iEpoch = d2r.first.epoch;
    d2r.deq;

    if(iEpoch == eEpoch) begin
      // Register Read and Exec
      let rVal1 = isValid(dInst.src1) ? rf.rd1(validValue(dInst.src1)) : ?;
      let rVal2 = isValid(dInst.src2) ? rf.rd2(validValue(dInst.src2)) : ?;
      let csrVal = isValid(dInst.csr) ? csrf.rd(validValue(dInst.csr)) : ?;
      let eInst = exec(dInst, rVal1, rVal2, pc, ppc, csrVal);           
      $display("Execute done"); 
      
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
			    /* TODO: use dCache for request */
          dCache.req(MemReq{
            op: Ld,
            addr: eInst.addr,
            data: ?
          });
        end

        St:
        begin
			    /* TODO: use dCache for request */
          dCache.req(MemReq{
            op: St,
            addr: eInst.addr,
            data: eInst.data
          });
        end

        Unsupported :
        begin
          $fwrite(stderr, "ERROR: Executing unsupported instruction\n");
          $finish;
        end
      endcase   
	    m12m2.enq(Rest2Memory{iType: eInst.iType,dst:eInst.dst,csr:eInst.csr,data:eInst.data,addr:eInst.addr, mispredict:eInst.mispredict, brTaken:eInst.brTaken });
	  end
	endrule

	rule doRest2(csrf.started && stat == AOK);
	  m12m2.deq;
    begin
      let eInst = m12m2.first;
      Data ldData = ?;

      case(eInst.iType)
        Ld:
          begin
            let ldData <- dCache.resp; //
            eInst.data = ldData;
          end
      endcase

      if (isValid(eInst.dst)) begin
          rf.wr(fromMaybe(?, eInst.dst), eInst.data);
      end
      csrf.wr(eInst.iType == Csrw ? eInst.csr : Invalid, eInst.data);
      $display("Write Back done");

      case(eInst.iType)
        J: csrf.incInstTypeCnt(Ctr);
        Jr: csrf.incInstTypeCnt(Ctr);
        Br: csrf.incInstTypeCnt(Ctr);
        Ld: csrf.incInstTypeCnt(Mem);
        St: csrf.incInstTypeCnt(Mem);
      endcase
        
      if(eInst.mispredict) begin
        csrf.incBPMissCnt();
        case(eInst.iType)
          J: csrf.incMissInstTypeCnt(MissJ);
          Jr: csrf.incMissInstTypeCnt(MissJr);
          Br: csrf.incMissInstTypeCnt(MissBr);
          //default: csrf.incMissInstTypeCnt(Missunknown);
        endcase
      end
      else begin
        case(eInst.iType)
          J: csrf.incMissInstTypeCnt(HitJ);
          Jr: csrf.incMissInstTypeCnt(HitJr);
          Br: csrf.incMissInstTypeCnt(HitBr);
          //default: csrf.incMissInstTypeCnt(Hitunknown);
        endcase
      end    
    end
  endrule

  rule upd_Stat(csrf.started);
	  $display("Stat update");
  	statRedirect.deq;
    stat <= statRedirect.first;
  endrule

  mkConnection(mem.dReq, dCache.memReq);
  mkConnection(mem.dResp, dCache.memResp);

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

endmodule
