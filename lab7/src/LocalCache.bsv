import Types::*;
import CMemTypes::*;
import CacheTypes::*;
import Fifo::*;
import RegFile::*;
import Vector::*;
import MemInit::*; 

interface Cache;
	method Action req(MemReq r);
	method ActionValue#(Data) resp;

 	method ActionValue#(CacheMemReq) memReq;
 	method Action memResp(Line r);

	method Data getMissCnt;
	method Data getTotalReq;
endinterface

typedef enum {Ready, StartMiss, SendFillReq, WaitFillResp} CacheStatus deriving (Bits, Eq);


(*synthesize*)
module mkCacheSetAssociative (Cache);
	Vector#(LinesPerSet, RegFile#(CacheIndex, Line))				  dataArray <- replicateM(mkRegFileFull);
	Vector#(LinesPerSet, RegFile#(CacheIndex, Maybe#(CacheTag)))       tagArray <- replicateM(mkRegFileFull);
	Vector#(LinesPerSet, RegFile#(CacheIndex, Bool))				 dirtyArray <- replicateM(mkRegFileFull);
	Vector#(LinesPerSet, RegFile#(CacheIndex, SetOffset)) lruArray <- replicateM(mkRegFileFull);

	Reg#(Bit#(TAdd#(SizeOf#(CacheIndex), 1))) init <- mkReg(0);
	Reg#(CacheStatus)					    status <- mkReg(Ready);
	Reg#(CacheStatus)					    testflag <- mkReg(Ready);
	Reg#(Maybe#(SetOffset)) 		targetLine <- mkReg(Invalid);

	Fifo#(1, Data)  hitQ <- mkBypassFifo;
	Reg#(MemReq) missReq <- mkRegU;

	Fifo#(2, CacheMemReq) memReqQ <- mkCFFifo;
	Fifo#(2, Line) 		 memRespQ <- mkCFFifo;

	Reg#(Data) missCnt <- mkReg(0);
	Reg#(Data)  reqCnt <- mkReg(0);

	function CacheIndex getIdx(Addr addr) = truncate(addr >> (2 + fromInteger(valueOf(SizeOf#(BlockOffset))))); 
	function CacheTag getTag(Addr addr) = truncateLSB(addr);
	function BlockOffset getOffset(Addr addr) = truncate(addr >> 2); 

	function Addr getBlockAddr(CacheTag tag, CacheIndex idx);
		BlockOffset def_offset = 0;
		Addr addr = {tag, idx, def_offset, 2'b0}; 
		return addr;
	endfunction

	function Maybe#(SetOffset) checkHit(CacheTag tag, CacheIndex idx);
		// Returns the SetOffset when cache hit occurs at given idx with the given tag.
		// It happens by checking the validity and tag value.
		Maybe#(SetOffset) ret = Invalid;

		for(Integer i = 0; i < valueOf(LinesPerSet); i = i + 1)
		begin
			let tagArrayVal = tagArray[i].sub(idx);

			if(isValid(tagArrayVal) && (fromMaybe(?, tagArrayVal) == tag) )
			begin
				ret = tagged Valid fromInteger(i);
			end
		end
		return ret;
	endfunction

	function Maybe#(SetOffset) findInvalid(CacheIndex idx);
		// Returns the SetOffset of a invalid cache slot at given idx.
		// If no one exists, returns Invalid.
		Maybe#(SetOffset) ret = Invalid;

		for(Integer i = 0; i < valueOf(LinesPerSet); i = i+1)
		begin
			if(!isValid(tagArray[i].sub(idx)))
			begin
				ret = tagged Valid fromInteger(i);
			end
		end

		return ret;
	endfunction

    function SetOffset findLRU(CacheIndex idx);
		// Returns the exact LRU.
		return lruArray[valueOf(LinesPerSet) - 1].sub(idx);
	endfunction

	function Action updateLRUArray(CacheIndex idx, SetOffset lineNum);
		// update lruArray to help finding LRU.
	    return action
			// find the index of lineNum element in the LRUArray.
	       	Integer idxInLRUArray = 0;
        	for (Integer i = 1; i < valueOf(LinesPerSet); i = i+1)
        	begin
            	if (lineNum == lruArray[i].sub(idx)) begin
            		idxInLRUArray = i;
          		end
			end

			// right shift elements before lineNum.
          	for (Integer i = 1;  i<= idxInLRUArray; i = i+1)
          	begin
            	lruArray[i].upd(idx, lruArray[i-1].sub(idx));
          	end

          	// put lineNum at the front.
          	lruArray[0].upd(idx, lineNum);
	    endaction;
    endfunction

	

	/* You can use this function in rules(startMiss,waitFillResp) when implement set associative cache */
	function SetOffset findLineToUse(CacheIndex idx);
		// if empty line exists, use that line.
		// if empty line doesn't exist, use LRU.
		let emptyLine = findInvalid(idx);
		if (isValid(emptyLine)) begin
			return fromMaybe(?, emptyLine);
		end else begin
			return findLRU(idx);
		end
	endfunction



 	let inited = truncateLSB(init) == 1'b1;

	rule initialize(!inited);
		init <= init + 1;

		for(Integer i = 0; i< valueOf(LinesPerSet);i = i+1)
		begin
			tagArray[i].upd(truncate(init), Invalid);
			dirtyArray[i].upd(truncate(init), False);
			lruArray[i].upd(truncate(init), fromInteger(i));
		end
	endrule

	rule startMiss(status == StartMiss);
		/* TODO: Implement here */
		let idx = getIdx(missReq.addr);
		let tag = getTag(missReq.addr);
		let lineNum = findLineToUse(idx);
		if (isValid(tagArray[lineNum].sub(idx)) && dirtyArray[lineNum].sub(idx)) begin
			let oldTag = fromMaybe(?, tagArray[lineNum].sub(idx));
			let writebackAddr = getBlockAddr(oldTag, idx);
			memReqQ.enq(CacheMemReq{
				op: St,
				addr: writebackAddr,
				data: dataArray[lineNum].sub(idx),
				burstLength: fromInteger(valueOf(WordsPerBlock))
			});
		end
		status <= SendFillReq;
		targetLine <= tagged Valid lineNum;
	endrule

	rule sendFillReq(status == SendFillReq);
		/* TODO: Implement here */
		let idx = getIdx(missReq.addr);
		let tag = getTag(missReq.addr);
		memReqQ.enq(CacheMemReq{
			op: Ld,
			addr: getBlockAddr(tag, idx),
			data: ?,
			burstLength: fromInteger(valueOf(WordsPerBlock))
		});
		status <= WaitFillResp;
	endrule

	rule waitFillResp(status == WaitFillResp);
		/* TODO: Implement here */
		let idx = getIdx(missReq.addr);
		let tag = getTag(missReq.addr);
		let lineNum = fromMaybe(?, targetLine);
		let offset = getOffset(missReq.addr);
		let data = memRespQ.first;
		memRespQ.deq;
		dataArray[lineNum].upd(idx, data);
		tagArray[lineNum].upd(idx, tagged Valid tag);
		updateLRUArray(idx, lineNum);
		dirtyArray[lineNum].upd(idx, False);
		hitQ.enq(data[offset]);
		status <= Ready;
	endrule

	method Action req(MemReq r) if (status == Ready && inited);
		// If hit, then
		//     - If r.op == Ld, return the cache value
		//     - If r.op == St, change the cache value and set as dirty
		// If not-hit, then
		//     - If r.op == Ld, send memory load request and wait.
		//     - If r.op == St, send store request.

		/* TODO: Implement here */
	    let idx = getIdx(r.addr);
		let tag = getTag(r.addr);
		let offset = getOffset(r.addr);
		let hit = checkHit(tag, idx);
		let lineNum = fromMaybe(?, hit);

		if(r.op == Ld)
		begin
			if(isValid(hit))
			begin
				updateLRUArray(idx, lineNum);
				hitQ.enq(dataArray[lineNum].sub(idx)[offset]);
			end
			else
			begin
				status <= StartMiss;
				missReq <= r;
			end
		end
		else
		begin
			if(isValid(hit))
			begin
				updateLRUArray(idx, lineNum);
				Line line = dataArray[lineNum].sub(idx);
				line[offset] = r.data;
				dataArray[lineNum].upd(idx, line);
				dirtyArray[lineNum].upd(idx, True);
			end
			else
			begin
				Line line = newVector;
				line[0] = r.data;
				memReqQ.enq(CacheMemReq{
					op : St,
					addr : r.addr,
					data : line,
					burstLength : 1
				});
			end
		end

		/* DO NOT MODIFY BELOW HERE! */
		if(!isValid(hit))
		begin
			missCnt <= missCnt + 1;
		end
		reqCnt <= reqCnt + 1; 
	endmethod


	method ActionValue#(Data) resp;
		hitQ.deq;
		return hitQ.first;
	endmethod

	method ActionValue#(CacheMemReq) memReq;
		memReqQ.deq;
		return memReqQ.first;
	endmethod

	method Action memResp(Line r);
		memRespQ.enq(r);
	endmethod

	method Data getMissCnt;
		return missCnt;
	endmethod

	method Data getTotalReq;
		return reqCnt;
	endmethod
endmodule

(*synthesize*)
module mkCache (Cache);
	Cache cacheSetAssociative <- mkCacheSetAssociative;
	return cacheSetAssociative;
endmodule
