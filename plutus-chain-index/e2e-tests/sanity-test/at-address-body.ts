export default function atAddressBody(pageSize?: number, txOutRef_id?: string,
    txOutRef_idx?: number, pubkeyhash?: string) {
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
