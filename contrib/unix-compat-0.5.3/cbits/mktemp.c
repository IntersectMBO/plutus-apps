/*
 * Modified version of 'mktemp.c' from FreeBSD
 * http://www.freebsd.org/cgi/cvsweb.cgi/src/lib/libc/stdio/mktemp.c
 *     ?rev=1.29.2.2.2.1;content-type=text%2Fplain
 */

/*
 * Copyright (c) 1987, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <sys/param.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <windows.h>
#include <wincrypt.h>

static int random(uint32_t *);
static int _gettemp(char *, int *);

static const unsigned char padchar[] =
"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

int unixcompat_mkstemp(char *path)
{
    int fd;

    if (_gettemp(path, &fd))
        return fd;

    return -1;
}

static int _gettemp(char *path, int *doopen)
{
    char *start, *trv, *suffp, *carryp;
    char *pad;
    struct _stat sbuf;
    int rval;
    uint32_t randidx, randval;
    char carrybuf[MAXPATHLEN];

    for (trv = path; *trv != '\0'; ++trv)
        ;
    if (trv - path >= MAXPATHLEN) {
        errno = ENAMETOOLONG;
        return (0);
    }
    suffp = trv;
    --trv;
    if (trv < path || NULL != strchr(suffp, '/')) {
        errno = EINVAL;
        return (0);
    }

    /* Fill space with random characters */
    while (trv >= path && *trv == 'X') {
        if (!random(&randval)) {
            /* this should never happen */
            errno = EIO;
            return 0;
        }
        randidx = randval % (sizeof(padchar) - 1);
        *trv-- = padchar[randidx];
    }
    start = trv + 1;

    /* save first combination of random characters */
    memcpy(carrybuf, start, suffp - start);

    /*
     * check the target directory.
     */
    if (doopen != NULL) {
        for (; trv > path; --trv) {
            if (*trv == '/') {
                *trv = '\0';
                rval = _stat(path, &sbuf);
                *trv = '/';
                if (rval != 0)
                    return (0);
                if (!S_ISDIR(sbuf.st_mode)) {
                    errno = ENOTDIR;
                    return (0);
                }
                break;
            }
        }
    }

    for (;;) {
        if (doopen) {
            if ((*doopen =
                _open(path, O_CREAT|O_EXCL|O_RDWR, 0600)) >= 0)
                return (1);
            if (errno != EEXIST)
                return (0);
        } else if (_stat(path, &sbuf))
            return (errno == ENOENT);

        /* If we have a collision, cycle through the space of filenames */
        for (trv = start, carryp = carrybuf;;) {
            /* have we tried all possible permutations? */
            if (trv == suffp)
                return (0); /* yes - exit with EEXIST */
            pad = strchr(padchar, *trv);
            if (pad == NULL) {
                /* this should never happen */
                errno = EIO;
                return (0);
            }
            /* increment character */
            *trv = (*++pad == '\0') ? padchar[0] : *pad;
            /* carry to next position? */
            if (*trv == *carryp) {
                /* increment position and loop */
                ++trv;
                ++carryp;
            } else {
                /* try with new name */
                break;
            }
        }
    }
    /*NOTREACHED*/
}

static int random(uint32_t *value)
{
    /* This handle is never released. Windows will clean up when the process
     * exits. Python takes this approach when emulating /dev/urandom, and if
     * it's good enough for them, then it's good enough for us. */
    static HCRYPTPROV context = 0;

    if (context == 0)
        if (!CryptAcquireContext(
                &context, NULL, NULL, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT))
            return 0;

    if (!CryptGenRandom(context, sizeof *value, (BYTE *)value))
        return 0;

    return 1;
}
