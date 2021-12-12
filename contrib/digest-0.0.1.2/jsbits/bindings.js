// EMCC:EXPORTED_FUNCTIONS _malloc _free
// EMCC:EXPORTED_FUNCTIONS _adler32 _crc32
function h$copyToHeap(buf_d, buf_o, tgt, len) {
    if(len === 0) return;
    var u8 = buf_d.u8;
    for(var i=0;i<len;i++) {
        HEAPU8[tgt+i] = u8[buf_o+i];
    }
}

function h$copyFromHeap(src, buf_d, buf_o, len) {
    var u8 = buf_d.u8;
    for(var i=0;i<len;i++) {
        u8[buf_o+i] = HEAPU8[src+i];
    }
}

function h$withCBuffer(str_d, str_o, len, cont) {
    var str = _malloc(len);
    if(str_d !== null) h$copyToHeap(str_d, str_o, str, len);
    var ret = cont(str);
    _free(str);
    return ret;
}

function h$adler32(adler, buf_d, buf_o, buf_len) {
    return h$withCBuffer(buf_d, buf_o, buf_len, function(buf) {
        return _adler32(adler, buf, buf_len);
    });
}

function h$crc32(crc, buf_d, buf_o, buf_len) {
    return h$withCBuffer(buf_d, buf_o, buf_len, function(buf) {
        return _crc32(crc, buf, buf_len);
    });
}