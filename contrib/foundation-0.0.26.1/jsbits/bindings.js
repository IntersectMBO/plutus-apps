
// EMCC:EXPORTED_FUNCTIONS _malloc _free
// EMCC:EXPORTED_FUNCTIONS _foundation_rngV1_generate

function h$foundation_sysrandom_linux(buf_d, buf_o, size) {
    return -19; // ENODEV; foundation returns the same for non-linux hosts.
}

// c_rngv1_generate :: Ptr Word8 -- new key
// -> Ptr Word8 -- destination
// -> Ptr Word8 -- current key
// -> CountOf Word8 -- number of bytes to generate
// -> IO Word32
//
// int foundation_rngV1_generate(uint8_t newkey[CHACHA_KEY_SIZE], uint8_t *dst, uint8_t key[CHACHA_KEY_SIZE], FsCountOf bytes)
//
// #define CHACHA_KEY_SIZE 3

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

function h$withOutBuffer(ptr_d, ptr_o, len, cont) {
    var ptr = _malloc(len);
    h$copyToHeap(ptr_d, ptr_o, ptr, len);
    var ret = cont(ptr);
    h$copyFromHeap(ptr, ptr_d, ptr_o, len);
    _free(ptr);
    return ret;
  }


var CHACHA_KEY_SIZE = 32;
function h$foundation_rngV1_generate(newkey_d, newkey_o, dst_d, dst_o, key_d, key_o, dst_len) {
    return h$withCBuffer(newkey_d, newkey_o, CHACHA_KEY_SIZE, function(newkey) {
        return h$withOutBuffer(dst_d, dst_o, dst_len, function(dst) {
            return h$withCBuffer(key_d, key_o, CHACHA_KEY_SIZE, function(key) {
                return _foundation_rngV1_generate(newkey, dst, key, dst_len);
            });
        });
    });
}