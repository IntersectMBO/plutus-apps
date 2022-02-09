const env = require('../testnet-env.json');
const request = require("supertest")(env.url);
const { expect } = require("chai");

describe("POST /is-utxo", () => {
    describe("relevant utxo", () => {
        var response, resBody;
        before(async () => {
            response = await request
                .post("/is-utxo")
                .send({ "txOutRefId": { "getTxId": env.relevant_txOutRef_id }, "txOutRefIdx": env.relevant_txOutRef_idx });
            expect(response.status).to.equal(200);
            resBody = response.body;
        });

        it("returns currentTip", async () => {
            expect(resBody.currentTip.tag).to.eql("Tip")
        });

        it("returns isUtxo true", async () => {
            expect(resBody.isUtxo).to.eql(true);
        });
    });

    describe("irrelevant utxo ref id", () => {
        var response, resBody;
        before(async () => {
            response = await request
                .post("/is-utxo")
                .send({ "txOutRefId": { "getTxId": env.relevant_txOutRef_id }, "txOutRefIdx": 666 });
            expect(response.status).to.equal(200);
            resBody = response.body;
        });

        it("returns currentTip", async () => {
            expect(resBody.currentTip.tag).to.eql("Tip")
        });

        it("returns isUtxo true", async () => {
            expect(resBody.isUtxo).to.eql(false);
        });
    });
});