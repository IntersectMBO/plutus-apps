import * as env from '../testnet-env.json'
import { expect } from "chai";
const request = require("supertest")(env.url);

describe("POST /tx-out", () => {
    let response, resBody;
    before(async () => {
        response = await request
            .post("/tx-out")
            .send({ "txOutRefId": { "getTxId": env.relevant_txOutRef_id }, "txOutRefIdx": env.relevant_txOutRef_idx });
        expect(response.status).to.equal(200);
        resBody = response.body;
    });

    it("returns correct fields", async () => {
        expect(resBody.tag).to.eql("PublicKeyChainIndexTxOut");
        expect(resBody._ciTxOutAddress).to.be.an("object");
        expect(resBody._ciTxOutValue).to.be.an("object");
    });

    it("check body _ciTxOutAddress", async () => {
        expect(resBody._ciTxOutAddress.addressCredential.contents.getPubKeyHash).to.eql(env.relevant_txOut_pubkeyhash);
    });

    it("check body _ciTxOutValue", async () => {
        expect(resBody._ciTxOutValue.getValue[0][1][0][1]).to.eql(2000000);
    });
});
