/*
  Pointers in emscripten compiled code are represented as offsets
  into the global HEAP ArrayBuffer.

  GHCJS pointers (Addr#) and unlifted arrays (ByteArray# etc.) are represented
  as a pair of a buffer and an offset.
 */

function h$logWrapper(x) {
  /* console.log(x); */
}

function h$copyToHeap(buf_d, buf_o, tgt, len) {
  if(len === 0) return;
  var u8 = buf_d.u8;
  var hexes = "";
  for(var i=0;i<len;i++) {
    h$direct_sqlite.HEAPU8[tgt+i] = u8[buf_o+i];
    hexes += h$toHex(u8[buf_o+i]);
  }
  // h$logWrapper("=> " + len + " " + hexes + " " + buf_o + " " + buf_d.len);
}

function h$copyFromHeap(src, buf_d, buf_o, len) {
  var u8 = buf_d.u8;
  var hexes = "";
  for(var i=0;i<len;i++) {
    u8[buf_o+i] = h$direct_sqlite.HEAPU8[src+i];
    hexes += h$toHex(h$direct_sqlite.HEAPU8[src+i]);
  }
  // h$logWrapper("<= " + len + " " + hexes + " " + buf_o + " " + buf_d.len);
}

function h$toHex(n) {
  var s = n.toString(16);
  if(s.length === 1) s = '0' + s;
  return s;
}

var h$buffers     = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0];
var h$bufferSizes = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0];

function h$getTmpBuffer(n, minSize) {
  var sn = h$bufferSizes[n];
  if(sn < minSize) {
    if(sn > 0) {
      h$direct_sqlite._free(h$buffers[n]);
    }
    h$buffers[n] = h$direct_sqlite._malloc(2*minSize); // fixme 2* shouldn't be needed
    h$bufferSizes[n] = minSize;
  }
  return h$buffers[n];
}

function h$getTmpBufferWith(n, buf_d, buf_o, len) {
  // fixme: we can avoid the copying if the buffer is already the actual
  //        heap buffer
  var buf_ptr = h$getTmpBuffer(n, len);
  h$copyToHeap(buf_d, buf_o, buf_ptr, len);
  return buf_ptr;
}


function h$copyStringToHeap(str_d, str_o) {
  var len = h$strlen(str_d, str_o)+1;
  var heap = h$direct_sqlite._malloc(len);
  writeArrayToMemory(str_d.u8.slice(str_o,len),heap);
  return heap;
}

function h$sqlite3_open(str_d, str_o, db_d, db_o) {
  var s = h$copyStringToHeap(str_d, str_o);
      prt2 = h$getTmpBufferWith;
      ptr  = h$getTmpBuffer(4, 2);
  h$direct_sqlite._sqlite3_open(s,ptr);
  h$copyFromHeap(ptr, db_d, db_o, 2);
  h$direct_sqlite._free(len);
}