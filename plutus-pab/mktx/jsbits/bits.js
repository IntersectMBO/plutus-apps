function plutus_apps_init_API(x) {
    // console.log("exporting API ", x);
    (typeof window !== 'undefined' ? window : global).plutus = x;
}

/*
  Convert a date value to a POSIX timestamp in seconds

  This expects either a number that's already a POSIX timestamp
  or a Date object.

  Returns null if the value cannot be converted
 */
function plutus_apps_convert_in_date(x) {
    if(typeof x == 'number') {
        // already a timestamp
        return x|0;
    } else if(typeof x == 'object' && x && x instanceof Date) {
        // Date object
        return x.getTime()|0;
    } else {
        // invalid input
        return null;
    }
}

function plutus_apps_convert_in_maybe_int(x) {
    if(typeof x == 'number') {
        return x|0;
    } else {
        return null;
    }
}

/*
  Convert some binary data to something usable for a ByteString

  Returns null if the value cannot be converted
 */
function plutus_apps_convert_in_bytestring(x) {
    if(typeof x == 'string' && x.length % 2 === 0 && x.match(/^[a-fA-F0-9]*$/)) {
        // convert hexadecimal string
        var b = x.length / 2;
        var res = new Uint8Array(b);
        for(var i = 0; i < b; i++) res[i] = plutus_apps_hex_chars_to_byte(x, i);
        return h$wrapBuffer(res.buffer);
    } else if(typeof x == 'object' && x && x instanceof Uint8Array) {
        // already Uint8Array, copy it
        return h$wrapBuffer(x.slice().buffer);
    } else {
        // invalid input
        return null;
    }
}

function plutus_apps_hex_chars_to_byte(s, i) {
    var c1 = s.charCodeAt(2*i);
    var c2 = s.charCodeAt(2*i+1);
    // convert ASCII codes to nibbles
    c1 = (c1 & 0xf) + 9 * (c1 >> 6);
    c2 = (c2 & 0xf) + 9 * (c2 >> 6);
    return (c1 << 4) + c2;
}

/*
  The GHCJS base library only supports callbacks up to three arguments,
  we just use a wrapper for four arguments
 */
function plutus_apps_wrap_call_4(f) {
    return function(x1, x2, x3, x4) {
        return f([x1, x2, x3, x4]);
    };
}

/*
  Our Haskell callbacks return an object r = { error, value }
  
  These wrappers throw the result value as an exception in error
  situations, while in non-error conditions the value is returned normally.
 */
function plutus_apps_handle_errors_1(f) {
    return function(x) {
        var r = f(x);
        if(r.error) throw r.value; else return r.value;
    };
}

function plutus_apps_handle_errors_2(f) {
    return function(x1, x2) {
        var r = f(x1, x2);
        if(r.error) throw r.value; else return r.value;
    };
}

function plutus_apps_handle_errors_3(f) {
    return function(x1, x2, x3) {
        var r = f(x1, x2, x3);
        if(r.error) throw r.value; else return r.value;
    };
}

function plutus_apps_handle_errors_4(f) {
    return function(x1, x2, x3, x4) {
        var r = f(x1, x2, x3, x4);
        if(r.error) throw r.value; else return r.value;
    };
}
