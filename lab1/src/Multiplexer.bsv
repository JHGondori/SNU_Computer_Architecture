function Bit#(1) and1(Bit#(1) a, Bit#(1) b);
  return a & b;
endfunction

function Bit#(1) or1(Bit#(1) a, Bit#(1) b);
  return a | b;
endfunction

function Bit#(1) not1(Bit#(1) a);
  return ~ a;
endfunction

function Bit#(1) multiplexer1(Bit#(1) sel, Bit#(1) a, Bit#(1) b);
  return or1(and1(not1(sel), a), and1(sel, b));
endfunction

function Bit#(64) multiplexer64(Bit#(1) sel, Bit#(64) a, Bit#(64) b);
  return multiplexer_n(sel, a, b);
endfunction

typedef 64 N;
function Bit#(N) multiplexerN(Bit#(1) sel, Bit#(N) a, Bit#(N) b);
  Bit#(N) aggregate;
  for(Integer i = 0; i < valueOf(N); i = i + 1)
    aggregate[i] = multiplexer1(sel, a[i], b[i]);
  return aggregate;
endfunction

//typedef 64 N; // Not needed
function Bit#(n) multiplexer_n(Bit#(1) sel, Bit#(n) a, Bit#(n) b);
  Bit#(n) aggregate;
  for(Integer i = 0; i < valueOf(n); i = i + 1)
    aggregate[i] = multiplexer1(sel, a[i], b[i]);
  return aggregate;
endfunction
