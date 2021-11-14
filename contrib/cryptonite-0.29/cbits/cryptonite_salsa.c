/*
 * Copyright (c) 2014-2015 Vincent Hanquez <vincent@snarc.org>
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the author nor the names of his contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include "cryptonite_salsa.h"
#include "cryptonite_bitfn.h"
#include "cryptonite_align.h"

static const uint8_t sigma[16] = "expand 32-byte k";
static const uint8_t tau[16] = "expand 16-byte k";

#define QR(a,b,c,d) \
	b ^= rol32(a+d, 7); \
	c ^= rol32(b+a, 9); \
	d ^= rol32(c+b, 13); \
	a ^= rol32(d+c, 18);

#define ALIGNED64(PTR) \
	(((uintptr_t)(const void *)(PTR)) % 8 == 0)

#define SALSA_CORE_LOOP \
	for (i = rounds; i > 0; i -= 2) { \
		QR (x0,x4,x8,x12); \
		QR (x5,x9,x13,x1); \
		QR (x10,x14,x2,x6); \
		QR (x15,x3,x7,x11); \
		QR (x0,x1,x2,x3); \
		QR (x5,x6,x7,x4); \
		QR (x10,x11,x8,x9); \
		QR (x15,x12,x13,x14); \
	}

static void salsa_core(int rounds, block *out, const cryptonite_salsa_state *in)
{
	uint32_t x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15;
	int i;

	x0 = in->d[0]; x1 = in->d[1]; x2 = in->d[2]; x3 = in->d[3];
	x4 = in->d[4]; x5 = in->d[5]; x6 = in->d[6]; x7 = in->d[7];
	x8 = in->d[8]; x9 = in->d[9]; x10 = in->d[10]; x11 = in->d[11];
	x12 = in->d[12]; x13 = in->d[13]; x14 = in->d[14]; x15 = in->d[15];

	SALSA_CORE_LOOP;

	x0 += in->d[0]; x1 += in->d[1]; x2 += in->d[2]; x3 += in->d[3];
	x4 += in->d[4]; x5 += in->d[5]; x6 += in->d[6]; x7 += in->d[7];
	x8 += in->d[8]; x9 += in->d[9]; x10 += in->d[10]; x11 += in->d[11];
	x12 += in->d[12]; x13 += in->d[13]; x14 += in->d[14]; x15 += in->d[15];

	out->d[0] = cpu_to_le32(x0);
	out->d[1] = cpu_to_le32(x1);
	out->d[2] = cpu_to_le32(x2);
	out->d[3] = cpu_to_le32(x3);
	out->d[4] = cpu_to_le32(x4);
	out->d[5] = cpu_to_le32(x5);
	out->d[6] = cpu_to_le32(x6);
	out->d[7] = cpu_to_le32(x7);
	out->d[8] = cpu_to_le32(x8);
	out->d[9] = cpu_to_le32(x9);
	out->d[10] = cpu_to_le32(x10);
	out->d[11] = cpu_to_le32(x11);
	out->d[12] = cpu_to_le32(x12);
	out->d[13] = cpu_to_le32(x13);
	out->d[14] = cpu_to_le32(x14);
	out->d[15] = cpu_to_le32(x15);
}

void cryptonite_salsa_core_xor(int rounds, block *out, block *in)
{
	uint32_t x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15;
	int i;

#define LOAD(i) (out->d[i] ^= in->d[i])
	x0 = LOAD(0); x1 = LOAD(1); x2 = LOAD(2); x3 = LOAD(3);
	x4 = LOAD(4); x5 = LOAD(5); x6 = LOAD(6); x7 = LOAD(7);
	x8 = LOAD(8); x9 = LOAD(9); x10 = LOAD(10); x11 = LOAD(11);
	x12 = LOAD(12); x13 = LOAD(13); x14 = LOAD(14); x15 = LOAD(15);
#undef LOAD

	SALSA_CORE_LOOP;

	out->d[0] += x0; out->d[1] += x1; out->d[2] += x2; out->d[3] += x3;
	out->d[4] += x4; out->d[5] += x5; out->d[6] += x6; out->d[7] += x7;
	out->d[8] += x8; out->d[9] += x9; out->d[10] += x10; out->d[11] += x11;
	out->d[12] += x12; out->d[13] += x13; out->d[14] += x14; out->d[15] += x15;
}

/* only 2 valid values for keylen are 256 (32) and 128 (16) */
void cryptonite_salsa_init_core(cryptonite_salsa_state *st,
                                uint32_t keylen, const uint8_t *key,
                                uint32_t ivlen, const uint8_t *iv)
{
	const uint8_t *constants = (keylen == 32) ? sigma : tau;

	st->d[0] = load_le32_aligned(constants + 0);
	st->d[5] = load_le32_aligned(constants + 4);
	st->d[10] = load_le32_aligned(constants + 8);
	st->d[15] = load_le32_aligned(constants + 12);

	st->d[1] = load_le32(key + 0);
	st->d[2] = load_le32(key + 4);
	st->d[3] = load_le32(key + 8);
	st->d[4] = load_le32(key + 12);
	/* we repeat the key on 128 bits */
	if (keylen == 32)
		key += 16;
	st->d[11] = load_le32(key + 0);
	st->d[12] = load_le32(key + 4);
	st->d[13] = load_le32(key + 8);
	st->d[14] = load_le32(key + 12);

	st->d[9] = 0;
	switch (ivlen) {
	case 8:
		st->d[6] = load_le32(iv + 0);
		st->d[7] = load_le32(iv + 4);
		st->d[8] = 0;
		break;
	case 12:
		st->d[6] = load_le32(iv + 0);
		st->d[7] = load_le32(iv + 4);
		st->d[8] = load_le32(iv + 8);
	default:
		return;
	}
}

void cryptonite_salsa_init(cryptonite_salsa_context *ctx, uint8_t nb_rounds,
                           uint32_t keylen, const uint8_t *key,
                           uint32_t ivlen, const uint8_t *iv)
{
	memset(ctx, 0, sizeof(*ctx));
	ctx->nb_rounds = nb_rounds;
	cryptonite_salsa_init_core(&ctx->st, keylen, key, ivlen, iv);
}

void cryptonite_salsa_combine(uint8_t *dst, cryptonite_salsa_context *ctx, const uint8_t *src, uint32_t bytes)
{
	block out;
	cryptonite_salsa_state *st;
	int i;

	if (!bytes)
		return;

	/* xor the previous buffer first (if any) */
	if (ctx->prev_len > 0) {
		int to_copy = (ctx->prev_len < bytes) ? ctx->prev_len : bytes;
		for (i = 0; i < to_copy; i++)
			dst[i] = src[i] ^ ctx->prev[ctx->prev_ofs+i];
		memset(ctx->prev + ctx->prev_ofs, 0, to_copy);
		ctx->prev_len -= to_copy;
		ctx->prev_ofs += to_copy;
		src += to_copy;
		dst += to_copy;
		bytes -= to_copy;
	}

	if (bytes == 0)
		return;

	st = &ctx->st;

	/* xor new 64-bytes chunks and store the left over if any */
	for (; bytes >= 64; bytes -= 64, src += 64, dst += 64) {
		/* generate new chunk and update state */
		salsa_core(ctx->nb_rounds, &out, st);
		st->d[8] += 1;
		if (st->d[8] == 0)
			st->d[9] += 1;

		for (i = 0; i < 64; ++i)
			dst[i] = src[i] ^ out.b[i];
	}

	if (bytes > 0) {
		/* generate new chunk and update state */
		salsa_core(ctx->nb_rounds, &out, st);
		st->d[8] += 1;
		if (st->d[8] == 0)
			st->d[9] += 1;

		/* xor as much as needed */
		for (i = 0; i < bytes; i++)
			dst[i] = src[i] ^ out.b[i];
		
		/* copy the left over in the buffer */
		ctx->prev_len = 64 - bytes;
		ctx->prev_ofs = i;
		for (; i < 64; i++) {
			ctx->prev[i] = out.b[i];
		}
	}
}

void cryptonite_salsa_generate(uint8_t *dst, cryptonite_salsa_context *ctx, uint32_t bytes)
{
	cryptonite_salsa_state *st;
	block out;
	int i;

	if (!bytes)
		return;

	/* xor the previous buffer first (if any) */
	if (ctx->prev_len > 0) {
		int to_copy = (ctx->prev_len < bytes) ? ctx->prev_len : bytes;
		for (i = 0; i < to_copy; i++)
			dst[i] = ctx->prev[ctx->prev_ofs+i];
		memset(ctx->prev + ctx->prev_ofs, 0, to_copy);
		ctx->prev_len -= to_copy;
		ctx->prev_ofs += to_copy;
		dst += to_copy;
		bytes -= to_copy;
	}

	if (bytes == 0)
		return;

	st = &ctx->st;

	if (ALIGNED64(dst)) {
		/* xor new 64-bytes chunks and store the left over if any */
		for (; bytes >= 64; bytes -= 64, dst += 64) {
			/* generate new chunk and update state */
			salsa_core(ctx->nb_rounds, (block *) dst, st);
			st->d[8] += 1;
			if (st->d[8] == 0)
				st->d[9] += 1;
		}
	} else {
		/* xor new 64-bytes chunks and store the left over if any */
		for (; bytes >= 64; bytes -= 64, dst += 64) {
			/* generate new chunk and update state */
			salsa_core(ctx->nb_rounds, &out, st);
			st->d[8] += 1;
			if (st->d[8] == 0)
				st->d[9] += 1;

			for (i = 0; i < 64; ++i)
				dst[i] = out.b[i];
		}
	}

	if (bytes > 0) {
		/* generate new chunk and update state */
		salsa_core(ctx->nb_rounds, &out, st);
		st->d[8] += 1;
		if (st->d[8] == 0)
			st->d[9] += 1;

		/* xor as much as needed */
		for (i = 0; i < bytes; i++)
			dst[i] = out.b[i];
		
		/* copy the left over in the buffer */
		ctx->prev_len = 64 - bytes;
		ctx->prev_ofs = i;
		for (; i < 64; i++)
			ctx->prev[i] = out.b[i];
	}
}

