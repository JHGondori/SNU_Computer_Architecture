
import GetPut::*;

import Types::*;
import CacheTypes::*;
import Vector::*;

//typedef Data MemResp; 

typedef WordsPerBlock MaxBurstLength;
typedef enum {Idle, Busy,IBusy, DBusy, BothBusy} MemStatus deriving(Eq, Bits, FShow);

typedef enum{Ld, St} MemOp deriving(Eq, Bits, FShow);
typedef struct{
    MemOp op;
    Addr  addr;
    Data  data;
} MemReq deriving(Eq, Bits, FShow);

typedef struct{
    MemOp op;
    Addr  addr;
    Line  data;
	Bit#(TAdd#(1,TLog#(MaxBurstLength))) burstLength;
} CacheMemReq deriving(Eq, Bits, FShow);

typedef 16 NumTokens;
typedef Bit#(TLog#(NumTokens)) Token;

typedef 16 LoadBufferSz;
typedef Bit#(TLog#(LoadBufferSz)) LoadBufferIndex;

typedef struct {
    Addr addr;
    Data data;
} MemInitLoad deriving(Eq, Bits, FShow);

typedef union tagged {
    MemInitLoad InitLoad;
    void InitDone;
} MemInit deriving(Eq, Bits, FShow);

interface MemInitIfc;
    interface Put#(MemInit) request;
    method Bool done();
endinterface

