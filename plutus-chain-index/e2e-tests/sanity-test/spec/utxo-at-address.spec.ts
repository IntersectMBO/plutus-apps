import * as env from '../testnet-env.json'
import { expect } from "chai";
import atAddressBody from "../at-address-body";
const request = require("supertest")(env.url);

describe("POST /utxo-at-address", () => {
    describe("relevant pubkeyhash & no page query", () => {
        let response, resBody;
        before(async () => {
            response = await request
                .post("/utxo-at-address")
                .send(atAddressBody(undefined, undefined, undefined, env.relevant_txOut_pubkeyhash));
            expect(response.status).to.equal(200);
            resBody = response.body;
        });

        it("returns currentTip", async () => {
            expect(resBody.currentTip.tag).to.eql("Tip")
        });

        it("returns correct number of pageItems", async () => {
            expect(resBody.page.pageItems).to.have.length(2);
        });
    });

    describe("relevant pubkeyhash & last page query", () => {
        let response, resBody;
        before(async () => {
            response = await request
                .post("/utxo-at-address")
                .send(atAddressBody(10, env.relevant_txOutRef_id,
                    env.lastPage_txOutRef_idx, env.relevant_txOut_pubkeyhash));
            expect(response.status).to.equal(200);
            resBody = response.body;
        });

        it("returns correct number of pageItems", async () => {
            expect(resBody.page.pageItems).to.have.length(1);
        });
    });

    describe("relevant pubkeyhash & pageSize query", () => {
        let response, resBody;
        before(async () => {
            response = await request
                .post("/utxo-at-address")
                .send(atAddressBody(1, undefined, undefined, env.relevant_txOut_pubkeyhash));
            expect(response.status).to.equal(200);
            resBody = response.body;
        });

        it("returns correct number of pageItems", async () => {
            expect(resBody.page.pageItems).to.have.length(1);
        });
    });

    describe("not relevant pubkeyhash", () => {
        let response, resBody;
        before(async () => {
            response = await request
                .post("/utxo-at-address")
                .send(atAddressBody(undefined, undefined, undefined, env.not_relevant_txOut_pubkeyhash));
            expect(response.status).to.equal(200);
            resBody = response.body;
        });

        it("returns correct number of pageItems", async () => {
            expect(resBody.page.pageItems).to.have.length(0);
        });
    });
});