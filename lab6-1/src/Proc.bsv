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
import Scoreboard::*;
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

(*synthesize*)
module mkProc(Proc);
  Reg#(Addr)    pc  <- mkRegU;
  RFile         rf  <- mkBypassRFile;  // Use the BypassRFile to handle the hazards. (wr < rd, Refer to M10.)
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

  // Scoreboard instantiation. Use this module to address the data hazard. 
  // Refer to scoreboard.bsv in the 'common-lib' directory.
  Scoreboard#(4) sb <- mkPipelineScoreboard;


/* Lab 6-1: TODO) - Implement a 5-stage pipelined processor using the provided scoreboard.
                  - Refer to common-lib/scoreboard.bsv and the PowerPoint slides.
                  - Use the scoreboard interface properly. */
  
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
    
    //Decode
    let dInst = decode(inst);

    let stall = sb.search1(dInst.src1)
               || sb.search2(dInst.src2);
    //Register Read
    if(!stall) begin
      $display("Decode : from Pc %d , \n", ipc);
      f2d.deq;
      let rVal1 = isValid(dInst.src1) ? tagged Valid rf.rd1(validValue(dInst.src1)) : Invalid;
      let rVal2 = isValid(dInst.src2) ? tagged Valid rf.rd2(validValue(dInst.src2)) : Invalid;
      let csrVal = isValid(dInst.csr) ? tagged Valid csrf.rd(validValue(dInst.csr)) : Invalid;

      sb.insert(dInst.dst);
      
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

    if(!pass) begin
      //WriteBack 
      $display("writeback");
      if (isValid(eInst.dst)) begin
          rf.wr(fromMaybe(?, eInst.dst), eInst.data);
      end
      csrf.wr(eInst.iType == Csrw ? eInst.csr : Invalid, eInst.data);
    end
    m2w.deq;
    sb.remove;
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