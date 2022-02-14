import * as env from '../testnet-env.json'
import { expect } from "chai";
const request = require("supertest")(env.url);

describe("POST /is-utxo", () => {
    describe("relevant utxo", () => {
        let response, resBody;
        before(async () => {
            response = await request
                .post("/is-utxo")
                .send({
                    "txOutRefId": { "getTxId": env.relevant_txOutRef_id },
                    "txOutRefIdx": env.relevant_txOutRef_idx
                });
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
        let response, resBody;
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