#include <ghcjs/rts.h>

// EMCC:EXPORTED_FUNCTIONS _malloc _free _crypto_generichash_blake2b
// EMCC:EXPORTED_FUNCTIONS _crypto_generichash_blake2b_final
// EMCC:EXPORTED_FUNCTIONS _crypto_generichash_blake2b_init
// EMCC:EXPORTED_FUNCTIONS _crypto_generichash_blake2b_update
// EMCC:EXPORTED_FUNCTIONS _crypto_hash_sha256 _crypto_hash_sha256_final
// EMCC:EXPORTED_FUNCTIONS _crypto_hash_sha256_init
// EMCC:EXPORTED_FUNCTIONS _crypto_hash_sha256_update
// EMCC:EXPORTED_FUNCTIONS _crypto_sign_ed25519_detached
// EMCC:EXPORTED_FUNCTIONS _crypto_sign_ed25519_seed_keypair
// EMCC:EXPORTED_FUNCTIONS _crypto_sign_ed25519_sk_to_pk
// EMCC:EXPORTED_FUNCTIONS _crypto_sign_ed25519_sk_to_seed
// EMCC:EXPORTED_FUNCTIONS _crypto_sign_ed25519_verify_detached
// EMCC:EXPORTED_FUNCTIONS _sodium_compare _sodium_free _sodium_init
// EMCC:EXPORTED_FUNCTIONS _sodium_malloc _sodium_memzero

function maybeCopyToEmscripten(d, o, len) {
  if(d.u8 == HEAPU8) {
    return { o: o, copyBack: function() {}, scrub: function() {}, free: function() {} };
  }
  else {
    oEmscripten = _malloc(len);
    for(var n = 0; n < len; n++) {
      HEAPU8[oEmscripten + n] = d.u8[o + n];
    }
    return {
      o: oEmscripten,
      copyBack: function() {
        for(var n = 0; n < len; n++) {
          d.u8[o + n] = HEAPU8[oEmscripten + n];
        }
      },
      scrub: function() {
        for(var n = 0; n < len; n++) {
          HEAPU8[oEmscripten + n] = 0;
        }
      },
      free: function() {
        _free(oEmscripten);
      }
    };
  }
}

h$sodium_withOutBuffer = function(d, o, len, cont) {
  var ptr = _malloc(len);
  for(var n = 0; n < len; n++) {
    HEAPU8[ptr + n] = d.u8[o + n];
  };
  var ret = cont(ptr);
  for(var n = 0; n < len; n++) {
    d.u8[o + n] = HEAPU8[ptr + n];
  }
  for(var n = 0; n < len; n++) {
    HEAPU8[ptr + n] = 0;
  }
  _free(ptr);
  return ret;
}

function h$sodium_init() {
  _sodium_init();
}

function h$sodium_malloc(size) {
  h$ret1 = _sodium_malloc(size);
  return { dv: new DataView(HEAPU8.buffer), u8: HEAPU8 };
}
function h$sodium_free(ptr) {
  _sodium_free(ptr);
}

// -- newtype SizedPtr (n :: Nat) = SizedPtr (Ptr Void)
// -- type CRYPTO_SHA256_BYTES = 32
var CRYPTO_SHA256_BYTES = 32;
// -- type CRYPTO_SHA512_BYTES = 64
var CRYPTO_BLAKE2B_256_BYTES = 64;
// -- type CRYPTO_BLAKE2B_256_BYTES = 32
var CRYPTO_BLAKE2B_256_BYTES = 32;
// -- type CRYPTO_SHA256_STATE_SIZE = (104)
var CRYPTO_SHA256_STATE_SIZE = 104;
// -- type CRYPTO_SHA512_STATE_SIZE = (208)
var CRYPTO_SHA512_STATE_SIZE = 208;
// -- type CRYPTO_BLAKE2B_256_STATE_SIZE = (384)
var CRYPTO_BLAKE2B_256_STATE_SIZE = 384;
// -- type CRYPTO_SIGN_ED25519_BYTES = 64
var CRYPTO_SIGN_ED25519_BYTES = 64;
// -- type CRYPTO_SIGN_ED25519_SEEDBYTES = 32
var CRYPTO_SIGN_ED25519_SEEDBYTES = 32;
// -- type CRYPTO_SIGN_ED25519_PUBLICKEYBYTES = 32
var CRYPTO_SIGN_ED25519_PUBLICKEYBYTES = 32;
// -- type CRYPTO_SIGN_ED25519_SECRETKEYBYTES = 64
var CRYPTO_SIGN_ED25519_SECRETKEYBYTES = 64;
//
// The following is the output of
// $ rg 'foreign import.*sodium' -A10 --no-line-number  |grep -v "\--" |sed -e 's/.*\.hs.//g'|xargs|sed 's/foreign import capi/\n/g'|sed 's/unsafe //g' |sed 's/ sodium.h/\/\//g' |sort|uniq
//
// crypto_generichash_blake2b c_crypto_generichash_blake2b :: Ptr out -> CSize -> Ptr CUChar -> CULLong -> Ptr key -> CSize -> IO Int
function h$crypto_generichash_blake2b(out_d, out_o, outlen,
                                      in_d, in_o, inlen_msw, inlen_lsw,
                                      key_d, key_o, keylen) {
  return h$sodium_withOutBuffer(out_d, out_o, outlen, function(out) {
    return h$sodium_withOutBuffer(in_d, in_o, inlen_lsw, function(in_) {
      return h$sodium_withOutBuffer(key_d, key_o, keylen, function(key) {
        return _crypto_generichash_blake2b(out, outlen, in_, inlen_lsw, inlen_msw, key, keylen);
      });
    });
  });
}

// crypto_generichash_blake2b_final c_crypto_generichash_blake2b_final :: SizedPtr CRYPTO_BLAKE2B_256_STATE_SIZE -> Ptr out -> CSize -> IO Int
function h$crypto_generichash_blake2b_final(state_d, state_o, out_d, out_o, outlen) {
  return h$sodium_withOutBuffer(state_d, state_o, CRYPTO_BLAKE2B_256_STATE_SIZE, function(state) {
    return h$sodium_withOutBuffer(out_d, out_o, outlen, function(out) {
      return _crypto_generichash_blake2b_final(state, out, outlen);
    });
  });
}
// crypto_generichash_blake2b_init c_crypto_generichash_blake2b_init :: SizedPtr CRYPTO_BLAKE2B_256_STATE_SIZE -> Ptr key -> CSize -> CSize -> IO Int
function h$crypto_generichash_blake2b_init(state_d, state_o, key_d, key_o, keylen, outlen) {
  return h$sodium_withOutBuffer(state_d, state_o, CRYPTO_BLAKE2B_256_STATE_SIZE, function(state) {
    return h$sodium_withOutBuffer(key_d, key_o, keylen, function(key) {
      return _crypto_generichash_blake2b_init(state, key, keylen, outlen);
    });
  });
}
// crypto_generichash_blake2b_update c_crypto_generichash_blake2b_update :: SizedPtr CRYPTO_BLAKE2B_256_STATE_SIZE -> Ptr CUChar -> CULLong -> IO Int
function h$crypto_generichash_blake2b_update(state_d, state_o, in_d, in_o, inlen_msw, inlen_lsw) {
  return h$sodium_withOutBuffer(state_d, state_o, CRYPTO_BLAKE2B_256_STATE_SIZE, function(state) {
    return h$sodium_withOutBuffer(in_d, in_o, inlen_lsw, function(in_) {
      return _crypto_generichash_blake2b_update(state, in_, inlen_lsw, inlen_msw);
    });
  });
}
// crypto_hash_sha256 c_crypto_hash_sha256 :: SizedPtr CRYPTO_SHA256_BYTES -> Ptr CUChar -> CULLong -> IO Int
function h$crypto_hash_sha256(out_d, out_o, in_d, in_o, inlen_msw, inlen_lsw) {
  return h$sodium_withOutBuffer(out_d, out_o, CRYPTO_SHA256_BYTES, function(out) {
    return h$sodium_withOutBuffer(in_d, in_o, inlen_lsw, function(in_) {
      return _crypto_hash_sha256(out, in_, inlen_lsw, inlen_msw);
    });
  });
}
// crypto_hash_sha256_final c_crypto_hash_sha256_final :: SizedPtr CRYPTO_SHA256_STATE_SIZE -> SizedPtr CRYPTO_SHA256_BYTES -> IO Int
function h$crypto_hash_sha256_final(state_d, state_o, out_d, out_o) {
  return h$sodium_withOutBuffer(state_d, state_o, CRYPTO_SHA256_STATE_SIZE, function(state) {
    return h$sodium_withOutBuffer(out_d, out_o, CRYPTO_SHA512_BYTES, function(out) {
      return _crypto_hash_sha256_final(state, out);
    });
  });
}
// crypto_hash_sha256_init c_crypto_hash_sha256_init :: SizedPtr CRYPTO_SHA256_STATE_SIZE -> IO Int
function h$crypto_hash_sha256_init(state_d, state_o) {
  return h$sodium_withOutBuffer(state_d, state_o, CRYPTO_SHA512_STATE_SIZE, function(state) {
    return _crypto_hash_sha256_init(state);
  });
}
// crypto_hash_sha256_update c_crypto_hash_sha256_update :: SizedPtr CRYPTO_SHA256_STATE_SIZE -> Ptr CUChar -> CULLong -> IO Int
function h$crypto_hash_sha256_update(state_d, state_o, msg_d, msg_o, msglen_msw, msglen_lsw) {
  return h$sodium_withOutBuffer(state_d, state_o, CRYPTO_SHA512_STATE_SIZE, function(state) {
    return h$sodium_withOutBuffer(msg_d, msg_o, msglen_lsw, function(msg) {
      return _crypto_hash_sha256_update(state, msg, msglen_lsw, msglen_msw);
    });
  });
}
// crypto_sign_ed25519_detached c_crypto_sign_ed25519_detached :: SizedPtr CRYPTO_SIGN_ED25519_BYTES -> Ptr CULLong -> Ptr CUChar -> CULLong -> SizedPtr CRYPTO_SIGN_ED25519_SECRETKEYBYTES -> IO Int
function h$crypto_sign_ed25519_detached(sig_d, sig_o, siglen_msw, siglen_lsw, msg_d, msg_o, msglen_msw, msglen_lsw, sk_d, sk_o) {
  return h$sodium_withOutBuffer(sig_d, sig_o, siglen_lsw, function(sig) {
    return h$sodium_withOutBuffer(msg_d, msg_o, msglen_lsw, function(msg) {
      return h$sodium_withOutBuffer(sk_d, sk_o, CRYPTO_SIGN_ED25519_SECRETKEYBYTES, function(sk) {
        return _crypto_sign_ed25519_detached(sig, siglen_lsw, siglen_msw, msg, msglen_lsw, msglen_msw, sk);
      });
    });
  });
}
// crypto_sign_ed25519_seed_keypair c_crypto_sign_ed25519_seed_keypair :: SizedPtr CRYPTO_SIGN_ED25519_PUBLICKEYBYTES -> SizedPtr CRYPTO_SIGN_ED25519_SECRETKEYBYTES -> SizedPtr CRYPTO_SIGN_ED25519_SEEDBYTES -> IO Int
function h$crypto_sign_ed25519_seed_keypair(pk_d, pk_o, sk_d, sk_o, seed_d, seed_o) {
  return h$sodium_withOutBuffer(pk_d, pk_o, CRYPTO_SIGN_ED25519_PUBLICKEYBYTES, function(pk) {
    return h$sodium_withOutBuffer(sk_d, sk_o, CRYPTO_SIGN_ED25519_SECRETKEYBYTES, function(sk) {
      return h$sodium_withOutBuffer(seed_d, seed_o, CRYPTO_SIGN_ED25519_SEEDBYTES, function(seed) {
        return _crypto_sign_ed25519_seed_keypair(pk, sk, seed);
      });
    });
  });
}
// crypto_sign_ed25519_sk_to_pk c_crypto_sign_ed25519_sk_to_pk :: SizedPtr CRYPTO_SIGN_ED25519_PUBLICKEYBYTES -> SizedPtr CRYPTO_SIGN_ED25519_SECRETKEYBYTES -> IO Int
function h$crypto_sign_ed25519_sk_to_pk(pk_d, pk_o, sk_d, sk_o) {
  return h$sodium_withOutBuffer(pk_d, pk_o, CRYPTO_SIGN_ED25519_PUBLICKEYBYTES, function(pk) {
    return h$sodium_withOutBuffer(sk_d, sk_o, CRYPTO_SIGN_ED25519_SECRETKEYBYTES, function(sk) {
      return _crypto_sign_ed25519_sk_to_pk(pk, sk);
    });
  });
}
// crypto_sign_ed25519_sk_to_seed c_crypto_sign_ed25519_sk_to_seed :: SizedPtr CRYPTO_SIGN_ED25519_SEEDBYTES -> SizedPtr CRYPTO_SIGN_ED25519_SECRETKEYBYTES -> IO Int
function h$crypto_sign_ed25519_sk_to_seed(seed_d, seed_o, sk_d, sk_o) {
  return h$sodium_withOutBuffer(seed_d, seed_o, CRYPTO_SIGN_ED25519_SEEDBYTES, function(seed) {
    return h$sodium_withOutBuffer(sk_d, sk_o, CRYPTO_SIGN_ED25519_SECRETKEYBYTES, function(sk) {
      return _crypto_sign_ed25519_sk_to_seed(seed, sk);
    });
  });
}
// crypto_sign_ed25519_verify_detached c_crypto_sign_ed25519_verify_detached :: SizedPtr CRYPTO_SIGN_ED25519_BYTES -> Ptr CUChar -> CULLong -> SizedPtr CRYPTO_SIGN_ED25519_PUBLICKEYBYTES -> IO Int
function h$crypto_sign_ed25519_verify_detached(sig_d, sig_o, msg_d, msg_o, msglen_msw, msglen_lsw, pk_d, pk_o) {
  return h$sodium_withOutBuffer(sig_d, sig_o, CRYPTO_SIGN_ED25519_BYTES, function(sig) {
    return h$sodium_withOutBuffer(msg_d, msg_o, msglen_lsw, function(msg) {
      return h$sodium_withOutBuffer(pk_d, pk_o, CRYPTO_SIGN_ED25519_PUBLICKEYBYTES, function(pk) {
        return _crypto_sign_ed25519_verify_detached(sig, msg, msglen_lsw, msglen_msw, pk);
      });
    });
  });
}
// sodium_compare c_sodium_compare :: Ptr a -> Ptr a -> CSize -> IO Int
function h$sodium_compare(b1_d, b1_o, b2_d, b2_o, len) {
  return h$sodium_withOutBuffer(b1_d, b1_o, len, function(b1) {
    return h$sodium_withOutBuffer(b2_d, b2_o, len, function(b2) {
      return _sodium_compare(b1, b2, len);
    });
  });
}
// &sodium_free c_sodium_free_funptr :: FunPtr (Ptr a -> IO ())
// sodium_free c_sodium_free :: Ptr a -> IO ()
// sodium_init c_sodium_init :: IO Int
// sodium_malloc c_sodium_malloc :: CSize -> IO (Ptr a)
// sodium_memzero c_sodium_memzero :: Ptr a -> CSize -> IO ()
