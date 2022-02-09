const env = require('../testnet-env.json');
const request = require("supertest")(env.url);
const { expect } = require("chai");

describe("POST /txs", () => {
    var response, resBody;
    before(async () => {
        response = await request
            .post("/txs")
            .send([{ "getTxId": env.relevant_txOutRef_id }, { "getTxId": env.txId_withscript }]);
        expect(response.status).to.equal(200);
        resBody = response.body;
    });

    it("returns correct number of Txs", async () => {
        expect(resBody).to.have.lengthOf(2);
    });

    it("returns correct Txs ids", async () => {
        expect(resBody[0]._citxTxId.getTxId).to.be.eql(env.relevant_txOutRef_id);
        expect(resBody[1]._citxTxId.getTxId).to.be.eql(env.txId_withscript);
    });
});