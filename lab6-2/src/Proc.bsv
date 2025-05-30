import Types::*;
import ProcTypes::*;
import CMemTypes::*;
import MemInit::*;
import RFile::*;
import IMemory::*;
import DMemory::*;
import Decode::*;
import Exec::*;
import CsrFile::*;
import Fifo::*;
import GetPut::*;

typedef struct {
  Instruction inst;
  Addr pc;
  Addr ppc;
  Bool epoch;
} Fetch2Decode deriving(Bits, Eq);

typedef struct{
  DecodedInst dInst;
  Addr pc;
  Addr ppc;
  Maybe#(Data) rVal1;
  Maybe#(Data) rVal2;
  Maybe#(Data) csrVal;
  Bool epoch;
} Decode2Exec deriving(Bits, Eq);

typedef struct{
  ExecInst eInst;
  Bool pass;
} Exec2Memory deriving(Bits, Eq);

typedef struct{
  ExecInst eInst;
  Bool pass;
} Memory2WriteBack deriving(Bits, Eq);

typedef struct{
  Maybe#(ExecInst) eInst;
} Forward deriving(Bits, Eq);

(*synthesize*)
module mkProc(Proc);
  Reg#(Addr)    pc  <- mkRegU;
  RFile         rf  <- mkBypassRFile; // Use the BypassRFile to handle the hazards. (wr < rd, Refer to M10.)
  //RFile         rf  <- mkRFile;
  IMemory     iMem  <- mkIMemory;
  DMemory     dMem  <- mkDMemory;
  CsrFile     csrf <- mkCsrFile;

  // The control hazard is handled using two Epoch registers and one BypassFifo.
  Reg#(Bool) fEpoch <- mkRegU;
  Reg#(Bool) eEpoch <- mkRegU;
  Fifo#(1, Addr) execRedirect <- mkBypassFifo;
   
  // PipelineFifo to construct the two-stage pipeline (Fetch stage and Rest stage).
  Fifo#(1, Fetch2Decode)  f2d <- mkPipelineFifo;
  Fifo#(1, Decode2Exec)  d2e <- mkPipelineFifo;
  Fifo#(1, Exec2Memory) e2m <- mkPipelineFifo;
  Fifo#(1, Memory2WriteBack) m2w <- mkPipelineFifo;

 /* Lab 6-2: TODO) - Implement a 5-stage pipelined processor using a data forwarding (bypassing) logic. 
                   - To begin with, it is recommended that you reuse the code that you implemented in Lab 6-1.
                   - Define the correct bypassing units using BypassFiFo. */
  Fifo#(1, Forward) e2d <- mkBypassFifo;
  Fifo#(1, Forward) m2d <- mkBypassFifo;
               
  rule doFetch(csrf.started);
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

  rule doDecode(csrf.started);
    let inst = f2d.first.inst;
    let ipc = f2d.first.pc;
    let ppc = f2d.first.ppc;
    let iEpoch = f2d.first.epoch;

    Forward f[2];
    let dInst = decode(inst);
    let rVal1 = ?;
    let rVal1Forward = ?;
    let rVal2 = ?;
    let rVal2Forward = ?;
    let csrVal = ?;

    if(e2d.notEmpty) begin
      e2d.deq;
      f[1] = e2d.first;
    end else begin
      f[1] = Forward{
              eInst : tagged Invalid
            };
    end
    if(m2d.notEmpty) begin
      m2d.deq;
      f[0] = m2d.first;
    end else begin
      f[0] = Forward{
              eInst : tagged Invalid
            };
    end

    function Bool isLoadUseHazard(DecodedInst dInst, Forward fwd);
      Bool ret = False;
      if (isValid(fwd.eInst)) begin
        let ei = validValue(fwd.eInst);
        if (ei.iType == Ld && (
          (isValid(dInst.src1) && fromMaybe(?, dInst.src1) == fromMaybe(?, ei.dst)) ||
          (isValid(dInst.src2) && fromMaybe(?, dInst.src2) == fromMaybe(?, ei.dst))
          )) ret = True;
      end
      return ret;
    endfunction

    let stall = isLoadUseHazard(dInst, f[1]);


    if(isValid(dInst.src1)) begin
      let src1 = fromMaybe(?, dInst.src1);
      if(isValid(f[1].eInst) &&
        isValid(validValue(f[1].eInst).dst) &&
        fromMaybe(?, validValue(f[1].eInst).dst) == src1) begin
          rVal1 = tagged Valid validValue(f[1].eInst).data;
          rVal1Forward = True;
      end
      else if(isValid(f[0].eInst) &&
        isValid(validValue(f[0].eInst).dst) &&
        fromMaybe(?, validValue(f[0].eInst).dst) == src1) begin
          rVal1 = tagged Valid validValue(f[0].eInst).data;
          rVal1Forward = True;
      end
      else begin
        rVal1 = tagged Valid rf.rd1(src1);
        rVal1Forward = False;
      end
    end else begin
      rVal1 = tagged Invalid;
      rVal1Forward = False;
    end

    if(isValid(dInst.src2)) begin
      let src2 = fromMaybe(?, dInst.src2);
      if(isValid(f[1].eInst) &&
        isValid(validValue(f[1].eInst).dst) &&
        fromMaybe(?, validValue(f[1].eInst).dst) == src2) begin
          rVal2 = tagged Valid validValue(f[1].eInst).data;
          rVal2Forward = True;
      end
      else if(isValid(f[0].eInst) &&
        isValid(validValue(f[0].eInst).dst) &&
        fromMaybe(?, validValue(f[0].eInst).dst) == src2) begin
          rVal2 = tagged Valid validValue(f[0].eInst).data;
          rVal2Forward = True;
      end
      else begin
        rVal2 = tagged Valid rf.rd2(src2);
        rVal2Forward = False;
      end
    end else begin
      rVal2 = tagged Invalid;
      rVal2Forward = False;
    end

    csrVal = isValid(dInst.csr) ? tagged Valid csrf.rd(validValue(dInst.csr)) : tagged Invalid;


    if(!stall) begin
      $display("Decode : from Pc %d , \n", ipc);
      f2d.deq;
      d2e.enq(Decode2Exec{
              dInst : dInst, 
              pc : ipc, 
              ppc : ppc, 
              rVal1 : rVal1, 
              rVal2 : rVal2, 
              csrVal : csrVal,
              epoch : iEpoch
          });
    end
    else begin
      if(iEpoch!=fEpoch || execRedirect.notEmpty) f2d.deq;
      $display("stall");
    end
  endrule

  rule doExecute(csrf.started);
    let dInst = d2e.first.dInst;
    let ipc = d2e.first.pc;
    let ppc = d2e.first.ppc;
    let rVal1 = isValid(d2e.first.rVal1) ? validValue(d2e.first.rVal1) : ?;
    let rVal2 = isValid(d2e.first.rVal2) ? validValue(d2e.first.rVal2) : ?;
    let csrVal = isValid(d2e.first.csrVal) ? validValue(d2e.first.csrVal) : ?;
    let iEpoch = d2e.first.epoch;
    d2e.deq;

    //Execute
    let eInst = exec(dInst, rVal1, rVal2, ipc, ppc, csrVal);
    if(iEpoch == eEpoch) begin
      $display("Execute : from Pc %d , \n", ipc);
      e2d.enq(Forward{
              eInst : tagged Valid eInst
          });
      e2m.enq(Exec2Memory{
              eInst : eInst, 
              pass : False
          });
      if(eInst.mispredict) begin
        eEpoch <= !eEpoch;
        execRedirect.enq(eInst.addr);
      end
    end
    else begin
      e2m.enq(Exec2Memory{
              eInst : eInst,
              pass : True
          });
    end 
  endrule

  rule doMemory(csrf.started);
    let eInst = e2m.first.eInst;
    let pass = e2m.first.pass;
    e2m.deq;

    if(!pass) begin
    //Memory
      $display("Memory");
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
      m2d.enq(Forward{
              eInst : tagged Valid eInst
          });
      m2w.enq(Memory2WriteBack{
              eInst : eInst,
              pass : False
          });
    end
    else begin
      m2w.enq(Memory2WriteBack{
              eInst : eInst,
              pass : True
          });
    end
  endrule

  rule doWriteBack(csrf.started);
    let eInst = m2w.first.eInst;
    let pass = m2w.first.pass;
    m2w.deq;

    if(!pass) begin
      //WriteBack 
      $display("writeback");
      if (isValid(eInst.dst)) begin
          rf.wr(fromMaybe(?, eInst.dst), eInst.data);
      end
      csrf.wr(eInst.iType == Csrw ? eInst.csr : Invalid, eInst.data);
    end
  endrule

  method ActionValue#(CpuToHostData) cpuToHost;
    let retV <- csrf.cpuToHost;
    return retV;
  endmethod

  method Action hostToCpu(Bit#(32) startpc) if (!csrf.started);
    csrf.start(0);
    eEpoch <= False;
    fEpoch <= False;
    pc <= startpc;
  endmethod

  interface iMemInit = iMem.init;
  interface dMemInit = dMem.init;

endmodule
