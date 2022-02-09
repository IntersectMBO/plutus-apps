const env = require('./testnet-env.json');

module.exports = function atAddressBody(pageSize, txOutRef_id, txOutRef_idx, pubkeyhash) {
    return {
        ...(pageSize || txOutRef_id) && {
            pageQuery: {
                ...(pageSize) && {
                    pageQuerySize: {
                        getPageSize: pageSize
                    }
                },
                ...(txOutRef_id) && {
                    pageQueryLastItem: {
                        txOutRefId: {
                            getTxId: txOutRef_id
                        },
                        txOutRefIdx: txOutRef_idx
                    }
                }
            }
        },
        ...(pubkeyhash) && {
            credential: {
                tag: "PubKeyCredential",
                contents: {
                    getPubKeyHash: pubkeyhash
                }
            }
        }
    }

}
