{-# LANGUAGE CApiFFI             #-}
{-# LANGUAGE DerivingStrategies  #-}

module Cardano.Crypto.Libsodium.C (
    -- * Initialization
    c_sodium_init,
    -- * Memory management
    c_sodium_memzero,
    c_sodium_malloc,
    c_sodium_free,
    c_sodium_free_funptr,
    -- * Hashing
    -- ** SHA256
    c_crypto_hash_sha256,
    c_crypto_hash_sha256_final,
    c_crypto_hash_sha256_init,
    c_crypto_hash_sha256_update,
    -- ** Blake2b 256
    c_crypto_generichash_blake2b,
    c_crypto_generichash_blake2b_final,
    c_crypto_generichash_blake2b_init,
    c_crypto_generichash_blake2b_update,
    -- * ED25519
    c_crypto_sign_ed25519_seed_keypair,
    c_crypto_sign_ed25519_sk_to_seed,
    c_crypto_sign_ed25519_detached,
    c_crypto_sign_ed25519_verify_detached,
    c_crypto_sign_ed25519_sk_to_pk,
    -- * Helpers
    c_sodium_compare,
    -- * Constants
    CRYPTO_SHA256_BYTES,
    CRYPTO_SHA512_BYTES,
    CRYPTO_BLAKE2B_256_BYTES,
    CRYPTO_SHA256_STATE_SIZE,
    CRYPTO_SHA512_STATE_SIZE,
    CRYPTO_BLAKE2B_256_STATE_SIZE,
    CRYPTO_SIGN_ED25519_BYTES,
    CRYPTO_SIGN_ED25519_SEEDBYTES,
    CRYPTO_SIGN_ED25519_PUBLICKEYBYTES,
    CRYPTO_SIGN_ED25519_SECRETKEYBYTES,
    ) where

import Foreign.C.Types
import Foreign.Ptr (FunPtr, Ptr)

import Cardano.Foreign
import Cardano.Crypto.Libsodium.Constants

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

-- | @void sodium_init();@
--
-- <https://libsodium.gitbook.io/doc/usage>
foreign import capi "sodium.h sodium_init"  c_sodium_init :: IO Int

-------------------------------------------------------------------------------
-- Memory management
-------------------------------------------------------------------------------

-- | @void sodium_memzero(void * const pnt, const size_t len);@
--
-- <https://libsodium.gitbook.io/doc/memory_management#zeroing-memory>
foreign import capi unsafe "sodium.h sodium_memzero" c_sodium_memzero :: Ptr a -> CSize -> IO ()

-- | @void *sodium_malloc(size_t size);@
--
-- <https://libsodium.gitbook.io/doc/memory_management>
foreign import capi unsafe "sodium.h sodium_malloc" c_sodium_malloc :: CSize -> IO (Ptr a)
--
-- | @void sodium_free(void *ptr);@
--
-- <https://libsodium.gitbook.io/doc/memory_management>
foreign import capi unsafe "sodium.h sodium_free" c_sodium_free :: Ptr a -> IO ()

-- | @void sodium_free(void *ptr);@
--
-- <https://libsodium.gitbook.io/doc/memory_management>
foreign import capi unsafe "sodium.h &sodium_free" c_sodium_free_funptr :: FunPtr (Ptr a -> IO ())

-------------------------------------------------------------------------------
-- Hashing: SHA256
-------------------------------------------------------------------------------

-- | @int crypto_hash_sha256(unsigned char *out, const unsigned char *in, unsigned long long inlen);@
--
-- <https://libsodium.gitbook.io/doc/advanced/sha-2_hash_function>
foreign import capi unsafe "sodium.h crypto_hash_sha256" c_crypto_hash_sha256 :: SizedPtr CRYPTO_SHA256_BYTES -> Ptr CUChar -> CULLong -> IO Int

-- | @int crypto_hash_sha256_init(crypto_hash_sha256_state *state);@
foreign import capi unsafe "sodium.h crypto_hash_sha256_init" c_crypto_hash_sha256_init :: SizedPtr CRYPTO_SHA256_STATE_SIZE -> IO Int

-- | @int crypto_hash_sha256_update(crypto_hash_sha256_state *state, const unsigned char *in, unsigned long long inlen);@
foreign import capi unsafe "sodium.h crypto_hash_sha256_update" c_crypto_hash_sha256_update :: SizedPtr CRYPTO_SHA256_STATE_SIZE -> Ptr CUChar -> CULLong -> IO Int

-- | @int crypto_hash_sha256_final(crypto_hash_sha256_state *state, unsigned char *out);@
foreign import capi unsafe "sodium.h crypto_hash_sha256_final" c_crypto_hash_sha256_final :: SizedPtr CRYPTO_SHA256_STATE_SIZE -> SizedPtr CRYPTO_SHA256_BYTES -> IO Int

-------------------------------------------------------------------------------
-- Hashing: Blake2b
-------------------------------------------------------------------------------

-- | @int crypto_generichash_blake2b(unsigned char *out, size_t outlen, const unsigned char *in, unsigned long long inlen, const unsigned char *key, size_t keylen);@
--
-- <https://libsodium.gitbook.io/doc/hashing/generic_hashing>
foreign import capi unsafe "sodium.h crypto_generichash_blake2b" c_crypto_generichash_blake2b
    :: Ptr out -> CSize
    -> Ptr CUChar -> CULLong
    -> Ptr key -> CSize
    -> IO Int

-- | @int crypto_generichash_blake2b_init(crypto_generichash_blake2b_state *state, const unsigned char *key, const size_t keylen, const size_t outlen);@
foreign import capi unsafe "sodium.h crypto_generichash_blake2b_init" c_crypto_generichash_blake2b_init :: SizedPtr CRYPTO_BLAKE2B_256_STATE_SIZE -> Ptr key -> CSize -> CSize -> IO Int

-- | @int crypto_generichash_blake2b_update(crypto_generichash_blake2b_state *state, const unsigned char *in, unsigned long long inlen);@
foreign import capi unsafe "sodium.h crypto_generichash_blake2b_update" c_crypto_generichash_blake2b_update :: SizedPtr CRYPTO_BLAKE2B_256_STATE_SIZE -> Ptr CUChar -> CULLong -> IO Int

-- | @int crypto_generichash_blake2b_final(crypto_generichash_blake2b_state *state, unsigned char *out, const size_t outlen);@
foreign import capi unsafe "sodium.h crypto_generichash_blake2b_final" c_crypto_generichash_blake2b_final :: SizedPtr CRYPTO_BLAKE2B_256_STATE_SIZE -> Ptr out -> CSize -> IO Int

-------------------------------------------------------------------------------
-- Signing: ED25519
-------------------------------------------------------------------------------

-- https://github.com/jedisct1/libsodium/blob/7b67cd1b32915bc957d750e7a15229f2a938ff1a/src/libsodium/include/sodium/crypto_sign_ed25519.h

-- | @int crypto_sign_ed25519_seed_keypair(unsigned char *pk, unsigned char *sk, const unsigned char *seed);@
foreign import capi unsafe "sodium.h crypto_sign_ed25519_seed_keypair" c_crypto_sign_ed25519_seed_keypair
    :: SizedPtr CRYPTO_SIGN_ED25519_PUBLICKEYBYTES
    -> SizedPtr CRYPTO_SIGN_ED25519_SECRETKEYBYTES
    -> SizedPtr CRYPTO_SIGN_ED25519_SEEDBYTES
    -> IO Int

-- | @int crypto_sign_ed25519_sk_to_seed(unsigned char *seed, const unsigned char *sk);@
foreign import capi unsafe "sodium.h crypto_sign_ed25519_sk_to_seed" c_crypto_sign_ed25519_sk_to_seed
    :: SizedPtr CRYPTO_SIGN_ED25519_SEEDBYTES
    -> SizedPtr CRYPTO_SIGN_ED25519_SECRETKEYBYTES
    -> IO Int

-- | @int crypto_sign_ed25519_detached(unsigned char *sig, unsigned long long *siglen_p, const unsigned char *m, unsigned long long mlen, const unsigned char *sk);@
foreign import capi unsafe "sodium.h crypto_sign_ed25519_detached" c_crypto_sign_ed25519_detached
    :: SizedPtr CRYPTO_SIGN_ED25519_BYTES
    -> Ptr CULLong
    -> Ptr CUChar
    -> CULLong
    -> SizedPtr CRYPTO_SIGN_ED25519_SECRETKEYBYTES
    -> IO Int

-- | @int crypto_sign_ed25519_verify_detached(const unsigned char *sig, const unsigned char *m, unsigned long long mlen, const unsigned char *pk);@
foreign import capi unsafe "sodium.h crypto_sign_ed25519_verify_detached" c_crypto_sign_ed25519_verify_detached
    :: SizedPtr CRYPTO_SIGN_ED25519_BYTES
    -> Ptr CUChar
    -> CULLong
    -> SizedPtr CRYPTO_SIGN_ED25519_PUBLICKEYBYTES
    -> IO Int

-- | @int crypto_sign_ed25519_sk_to_pk(unsigned char *pk, const unsigned char *sk);@
foreign import capi unsafe "sodium.h crypto_sign_ed25519_sk_to_pk" c_crypto_sign_ed25519_sk_to_pk
    :: SizedPtr CRYPTO_SIGN_ED25519_PUBLICKEYBYTES
    -> SizedPtr CRYPTO_SIGN_ED25519_SECRETKEYBYTES -> IO Int

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | @int sodium_compare(const void * const b1_, const void * const b2_, size_t len);@
--
-- <https://libsodium.gitbook.io/doc/helpers#comparing-large-numbers>
foreign import capi unsafe "sodium.h sodium_compare" c_sodium_compare :: Ptr a -> Ptr a -> CSize -> IO Int
