
function h$foundation_sysrandom_linux(buf_d, buf_o, size) {
    return -19; // ENODEV; foundation returns the same for non-linux hosts.
}
function foundation_rngV1_generate(newkey_d, newkey_o, dst_d, dst_o, key_d, key_o, bytes) {
    return 1;
}