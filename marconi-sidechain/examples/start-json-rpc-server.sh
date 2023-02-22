#!/usr/bin/env sh

PLUTUS_APPS_DIR=../..

DB="$PLUTUS_APPS_DIR/.marconidb/7"
mkdir -p $DB
ls -la $DB

## Noes
## We use db-utils-exe/exe/Main.hs to get a list of most re occuring addresses from utxo-db
## Therefor these addresses my not always be the best addresses to use.
##
cabal run examples-json-rpc-server -- \
    --utxo-db "$DB/utxo-db" \
    --addresses-to-index addr_test1qqwh05w69g95065zlr4fef4yfpjkv8r3dr9h3pu0egy5n7pwhxp4f95svdjr9dmtqumqcs6v49s6pe7ap4h2nv8rcaasgrkndk \
    --addresses-to-index addr_test1wr9gquc23wc7h8k4chyaad268mjft7t0c08wqertwms70sc0fvx8w \
    --addresses-to-index addr_test1qpp0xdvdexl4t3ur4svdkv3jjs2gy8fu6h9mrrsgvm7r20cmrhy4p5ukjknv23jy95nhsjsnud6fxkjqxp5ehvn8h0es2su3gt \
    --addresses-to-index addr_test1qzu6at6s7r7yzpvw3mm2tsas34mc9nzrhda89w59wd78lfaw8084xldqyrvxe38z4wxqqdr9h86t8ruut8rrfezdftpstsnrd8 \
    --addresses-to-index addr_test1vqslp49gcrvah8c9vjpxm4x9j7s2m4zw0gkscqh0pqjg4wcjzvcfr \
    --addresses-to-index addr_test1wrn2wfykuhswv4km08w0zcl5apmnqha0j24fa287vueknasq6t4hc \
    --addresses-to-index addr_test1qr30nkfx28r452r3006kytnpvn39zv7c2m5uqt4zrg35mly35pesdyk43wnxk3edkkw74ak56n4zh67reqjhcfp3mm7qtyekt4 \
    --addresses-to-index addr_test1qrr6urmwy2nle3xppnjg9xcukasrwfyfjv9eqh97km84ra53q4hau90tjeldx0mv9eka2z73t9727xl8jny3cy8zetqsdjctpd \
--addresses-to-index addr_test1vpfwv0ezc5g8a4mkku8hhy3y3vp92t7s3ul8g778g5yegsgalc6gc \
--addresses-to-index addr_test1wz3937ykmlcaqxkf4z7stxpsfwfn4re7ncy48yu8vutcpxgnj28k0 \
--addresses-to-index addr_test1vqeux7xwusdju9dvsj8h7mca9aup2k439kfmwy773xxc2hcu7zy99 \
--addresses-to-index addr_test1qpxtftdcvh55twn2lum4uylxshzls6489q89pft3feesq2m8an0x234jtsqra93hcefmut6cyd6cdn535nkjukhph47ql6uvg7 \
--addresses-to-index addr_test1qz8q8ymsty33sw7mh4s2wxj2pe4mcrlchxxa37z70l348cyjd3dlf08q9usapw5gt5t8cp8lju7wtwqzk5cj0gxaxyss6w8n66 \
--addresses-to-index addr_test1vrvf7yfr2h79mtzqrpcn0ql98xrhs63k85w64u8py7709zsm6tsr6 \
--addresses-to-index addr_test1vz8q8ymsty33sw7mh4s2wxj2pe4mcrlchxxa37z70l348cqk95hdw \
--addresses-to-index addr_test1wqgden0j2d7pkqy3hu6kcj32swazzy6wg93a8c46pndptncdmz6tq \
--addresses-to-index addr_test1qpe6s9amgfwtu9u6lqj998vke6uncswr4dg88qqft5d7f67kfjf77qy57hqhnefcqyy7hmhsygj9j38rj984hn9r57fswc4wg0 \
--addresses-to-index addr_test1qz4ll7yrah8h5t3cv2qptn4mw22judsm9j9zychhmtuuzmszd3hm6w02uxx6h0s3qgd4hxgpvd0qzklnmahcx7v0mcysptyj8l
