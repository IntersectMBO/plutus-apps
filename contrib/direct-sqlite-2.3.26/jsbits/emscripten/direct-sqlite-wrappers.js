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


function h$copyStringToHeap(n, str_d, str_o) {
  var len = h$strlen(str_d, str_o)+1;
  return h$getTmpBufferWith(n, str_d, str_o, len);
}

h$direct_sqlite.h$cstring = function(offset) {
  var len = 0;
  while(this.HEAPU8[offset+len] !== 0){ len++; };
  var str = h$newByteArray(len+1);
  str.u8.set(this.HEAPU8.subarray(offset,offset+len+1));
  return str;
}

h$direct_sqlite.h$ptr = function(offset) {
  var ptr = h$newByteArray(4);
  ptr.u8.set(this.HEAPU8.subarray(offset, offset+4));
  return ptr;
}

// sqlite3_open ::  CString -> Ptr (Ptr CDatabase) -> IO CError
function h$sqlite3_open(str_d, str_o, db_d, db_o) {
  var str = h$copyStringToHeap(0, str_d, str_o);
      ptr = h$getTmpBufferWith(1, db_d, db_o, 4);
      ret = h$direct_sqlite._sqlite3_open(str, ptr);

  // make sure to return a pointer! Needs to be a pair! ([d,0])
  db_d.arr = [[h$direct_sqlite.h$ptr(ptr),0]];
  return ret;
}

// sqlite3_close :: Ptr CDatabase -> IO CError
function h$sqlite3_close(db_d, db_o) {
  var ptr = db_d.i3[0];
  return h$direct_sqlite._sqlite3_close(ptr);
}

// sqlite3_errcode :: Ptr CDatabase -> IO CError
function h$sqlite3_errcode(db_d, db_o) {
  var ptr = db_d.i3[0];
  return h$direct_sqlite._sqlite3_close(ptr);
}

// sqlite3_interrupt :: Ptr CDatabase -> IO ()
function h$sqlite3_interrupt(db_d, db_o) {
  var ptr = db_d.i3[0];
  // we don't really need to return here. it's unit.
  return h$direct_sqlite._sqlite3_interrupt(ptr);
}

// sqlite3_errmsg :: Ptr CDatabase -> IO CString
function h$sqlite3_errmsg(db_d, db_o) {
  var ptr = db_d.i3[0];
      charptr = h$direct_sqlite._sqlite3_errmsg(ptr);
  return h$direct_sqlite.h$cstring(charptr);
}
// sqlite3_trace :: Ptr CDatabase -> FunPtr (CTraceCallback a)  -> Ptr a -> IO (Ptr ())
// sqlite3_get_autocommit :: Ptr CDatabase -> IO CInt
// sqlite3_enable_shared_cache :: Bool -> IO CError
// sqlite3_exec :: Ptr CDatabase -> CString -> FunPtr (CExecCallback a) -> Ptr a -> Ptr CString -> IO CError
function h$sqlite3_exec(db_d, db_o,
                        sql_d, sql_o,
                        cb_d, cb_o,
                        ptr_d, ptr_o,
                        res_d, res_o) {

  if(cb_d !== null) {
    console.log("FunPtr don't appear to work in GHCJS.");
    console.log(cb_d, cb_o);
    h$exitProcess(1);
  }
  var sql = h$copyStringToHeap(0, sql_d, sql_o);
      cb_ptr = cb_d === null ? null : h$getTmpBufferWith(1, cb_d, cb_o, 8); // FIXME: why 8? FunPtr are 8, and Ptrs only 4?
      ptr_ptr = ptr_d === null ? null : h$getTmpBufferWith(2, ptr_d, ptr_o, 4);
      res_ptr = h$getTmpBufferWith(3, res_d, res_o, 4);
      ret = h$direct_sqlite._sqlite3_exec(db_d.i3[0], sql, null /* cb_ptr */, null, res_ptr);
  // I don't think we need to read back anything from a FunPtr value. Just pass it.
  res_d.arr = [[h$direct_sqlite.h$cstring(res_ptr),0]];
  if(ptr_d !== null) {
    ptr_d.arr = [[h$direct_sqlite.h$cstring(ptr_ptr),0]]
  }
  return ret;
}
// sqlite3_prepare_v2 :: Ptr CDatabase -> CString -> CNumBytes -> Ptr (Ptr CStatement) -> Ptr CString -> IO CError
function h$sqlite3_prepare_v2(db_d, db_o, sql_d, sql_o, bytes, hdl_d, hdl_o, res_d, res_o) {
  var sql = h$copyStringToHeap(0, sql_d, sql_o);
      hdl_ptr = h$getTmpBufferWith(1, hdl_d, hdl_o, 4);
      res_ptr = res_d === null ? null : h$getTmpBufferWith(2, res_d, res_o, 4);

  var ret = h$direct_sqlite._sqlite3_prepare_v2(db_d.i3[0], sql, bytes, hdl_ptr, res_ptr);
  hdl_d.arr = [[h$direct_sqlite.h$ptr(hdl_ptr),0]];
  if(res_d !== null) {
    res_d.arr = [[h$direct_sqlite.h$cstring(res_ptr),0]];
  }
  return ret;
}
// sqlite3_db_handle :: Ptr CStatement -> IO (Ptr CDatabase)
// sqlite3_step :: Ptr CStatement -> IO CError
function h$sqlite3_step(stmt_d, stmt_o) {
  return h$direct_sqlite._sqlite3_step(stmt_d.i3[0]);
}
// sqlite3_step_unsafe :: Ptr CStatement -> IO CError
function h$sqlite3_step_unsafe(stmt_d, stmt_o) {
  return h$direct_sqlite._sqlite3_step_unsafe(stmt_d.i3[0]);
}
// sqlite3_reset :: Ptr CStatement -> IO CError
function h$sqlite3_reset(stmt_d, stmt_o) {
  return h$direct_sqlite._sqlite3_reset(stmt_d.i3[0]);
}
// sqlite3_finalize :: Ptr CStatement -> IO CError
function h$sqlite3_finalize(stmt_d, stmt_o) {
  return h$direct_sqlite._sqlite3_finalize(stmt_d.i3[0]);
}
// sqlite3_clear_bindings :: Ptr CStatement -> IO CError
function h$sqlite3_clear_bindings(stmt_d, stmt_o) {
  return h$direct_sqlite._sqlite3_clear_bindings(stmt_d.i3[0]);
}
// sqlite3_sql :: Ptr CStatement -> IO CString
// sqlite3_bind_parameter_count :: Ptr CStatement -> IO CParamIndex
// sqlite3_bind_parameter_name :: Ptr CStatement -> CParamIndex -> IO CString
// sqlite3_bind_parameter_index :: Ptr CStatement -> CString -> IO CParamIndex
// sqlite3_column_count :: Ptr CStatement -> IO CColumnCount
function h$sqlite3_column_count(stmt_d, stmt_o) {
  return h$direct_sqlite._sqlite3_column_count(stmt_d.i3[0]);
}
// sqlite3_column_name :: Ptr CStatement -> CColumnIndex -> IO CString
// sqlite3_bind_blob :: Ptr CStatement -> CParamIndex -> Ptr a -> CNumBytes -> Ptr CDestructor -> IO CError
// sqlite3_bind_zeroblob :: Ptr CStatement -> CParamIndex -> CInt -> IO CError
// sqlite3_bind_text :: Ptr CStatement -> CParamIndex -> CString -> CNumBytes -> Ptr CDestructor -> IO CError
// sqlite3_bind_double   :: Ptr CStatement -> CParamIndex -> Double -> IO CError
// sqlite3_bind_int64    :: Ptr CStatement -> CParamIndex -> Int64 -> IO CError
// sqlite3_bind_null     :: Ptr CStatement -> CParamIndex -> IO CError
// sqlite3_column_type   :: Ptr CStatement -> CColumnIndex -> IO CColumnType
function h$sqlite3_column_type(stmt_d, stmt_o, idx) {
  return h$direct_sqlite._sqlite3_column_type(stmt_d.i3[0], idx);
}
// sqlite3_column_bytes  :: Ptr CStatement -> CColumnIndex -> IO CNumBytes
function h$sqlite3_column_bytes(stmt_d, stmt_o, idx) {
  return h$direct_sqlite._sqlite3_column_bytes(stmt_d.i3[0], idx);
}
// sqlite3_column_blob   :: Ptr CStatement -> CColumnIndex -> IO (Ptr a)
function h$sqlite3_column_blob(stmt_d, stmt_o, idx) {
  var ret = h$direct_sqlite._sqlite3_column_blob(stmt_d.i3[0], idx);
  return h$direct_sqlite.h$ptr(ret);
}
// sqlite3_column_text   :: Ptr CStatement -> CColumnIndex -> IO CString
function h$sqlite3_column_text(stmt_d, stmt_o, idx) {
  var ret = h$direct_sqlite._sqlite3_column_text(stmt_d.i3[0], idx);
  return h$direct_sqlite.h$cstring(ret);
}
// sqlite3_column_int64  :: Ptr CStatement -> CColumnIndex -> IO Int64
function h$sqlite3_column_int64(stmt_d, stmt_o, idx) {
  return h$direct_sqlite._sqlite3_column_int64(stmt_d.i3[0], idx);
}
// sqlite3_column_double :: Ptr CStatement -> CColumnIndex -> IO Double
function h$sqlite3_column_double(stmt_d, stmt_o, idx) {
  return h$direct_sqlite._sqlite3_column_double(stmt_d.i3[0], idx);
}
// sqlite3_last_insert_rowid :: Ptr CDatabase -> IO Int64
// sqlite3_changes :: Ptr CDatabase -> IO CInt
// sqlite3_total_changes :: Ptr CDatabase -> IO CInt
// sqlite3_create_function_v2 :: Ptr CDatabase -> CString -> CArgCount -> CInt -> Ptr a -> FunPtr CFunc -> FunPtr CFunc -> FunPtr CFuncFinal -> FunPtr (CFuncDestroy a) -> IO CError
// sqlite3_user_data :: Ptr CContext -> IO (Ptr a)
// sqlite3_context_db_handle :: Ptr CContext -> IO (Ptr CDatabase)
// sqlite3_aggregate_context :: Ptr CContext -> CNumBytes -> IO (Ptr a)
// sqlite3_value_type   :: Ptr CValue -> IO CColumnType
// sqlite3_value_bytes  :: Ptr CValue -> IO CNumBytes
// sqlite3_value_blob   :: Ptr CValue -> IO (Ptr a)
// sqlite3_value_text   :: Ptr CValue -> IO CString
// sqlite3_value_int64  :: Ptr CValue -> IO Int64
// sqlite3_value_double :: Ptr CValue -> IO Double
// sqlite3_result_null     :: Ptr CContext -> IO ()
// sqlite3_result_blob     :: Ptr CContext -> Ptr a -> CNumBytes -> Ptr CDestructor -> IO ()
// sqlite3_result_zeroblob :: Ptr CContext -> CNumBytes -> IO ()
// sqlite3_result_text     :: Ptr CContext -> CString -> CNumBytes -> Ptr CDestructor -> IO ()
// sqlite3_result_int64    :: Ptr CContext -> Int64 -> IO ()
// sqlite3_result_double   :: Ptr CContext -> Double -> IO ()
// sqlite3_result_value    :: Ptr CContext -> Ptr CValue -> IO ()
// sqlite3_result_error    :: Ptr CContext -> CString -> CNumBytes -> IO ()
// sqlite3_create_collation_v2 :: Ptr CDatabase -> CString -> CInt -> Ptr a -> FunPtr (CCompare a) -> FunPtr (CFuncDestroy a) -> IO CError
// sqlite3_free :: Ptr a -> IO ()
function h$sqlite3_free(ptr_d, ptr_o) {
  console.log(ptr_d, ptr_o);
  return h$direct_sqlite._sqlite3_free(ptr_d.i3[0]);
}
// sqlite3_free_p :: FunPtr (Ptr a -> IO ())
// sqlite3_enable_load_extension :: Ptr CDatabase -> Bool -> IO CError
// sqlite3_wal_hook :: Ptr CDatabase -> FunPtr CWalHook -> Ptr a -> IO (Ptr ())
// sqlite3_blob_open :: Ptr CDatabase -> CString -> CString -> CString -> Int64 -> CInt -> Ptr (Ptr CBlob) -> IO CError
// sqlite3_blob_close :: Ptr CBlob -> IO CError
// sqlite3_blob_reopen :: Ptr CBlob -> Int64 -> IO CError
// sqlite3_blob_bytes :: Ptr CBlob -> IO CInt
// sqlite3_blob_read :: Ptr CBlob -> Ptr a -> CInt -> CInt -> IO CError
// sqlite3_blob_write :: Ptr CBlob -> Ptr a -> CInt -> CInt -> IO CError
// sqlite3_backup_init :: Ptr CDatabase -> CString -> Ptr CDatabase -> CString -> IO (Ptr CBackup)
// sqlite3_backup_finish :: Ptr CBackup -> IO CError
// sqlite3_backup_step :: Ptr CBackup -> CInt -> IO CError
// sqlite3_backup_remaining :: Ptr CBackup -> IO CInt
// sqlite3_backup_pagecount :: Ptr CBackup -> IO CInt
