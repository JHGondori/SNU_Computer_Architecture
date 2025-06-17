import Types::*;
import Vector::*;

/* =============== MODIFY HERE FOR SIMULATION =============== */
// When you are using direct mapped cache, LinesPerSet must be 1.
typedef 4 WordsPerBlock;  // You can change this to 1, 2, 4,..
typedef 8 LinesPerSet;   // You can change this to 1, 2, 4,..
/* ========================================================== */


/* YOU CAN MODIFY TotalCacheSize IF U WANT TO SEE SET ASSOCIATIVITY EFFECT MORE! */
typedef 256 TotalCacheSize;  
typedef Vector#(WordsPerBlock, Data) Line;
typedef Line MemResp;

typedef Bit#(TLog#(WordsPerBlock)) BlockOffset;
typedef Bit#(TLog#(LinesPerSet)) SetOffset; 

typedef TDiv#(TDiv#(TotalCacheSize, LinesPerSet), WordsPerBlock) NumOfSets;
typedef Bit#(TLog#(NumOfSets)) CacheIndex;  
typedef Bit#(TSub#(TSub#(TSub#(AddrSz, TLog#(NumOfSets)),SizeOf#(BlockOffset)),2)) CacheTag;


