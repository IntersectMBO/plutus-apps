import * as env from '../testnet-env.json'
import { expect } from "chai";
const request = require("supertest")(env.url);

describe("POST /txs", () => {
    let response, resBody;
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