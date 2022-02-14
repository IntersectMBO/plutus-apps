import * as env from '../testnet-env.json'
import { expect } from "chai";
import atAddressBody from "../at-address-body";
const request = require("supertest")(env.url);

describe("POST /txo-at-address", () => {
    describe("relevant pubkeyhash & with pageQuery", () => {
        let response, resBody;
        before(async () => {
            response = await request
                .post("/txo-at-address")
                .send(atAddressBody(10, env.relevant_txOutRef_id,
                    env.relevant_txOutRef_idx, env.relevant_txOut_pubkeyhash));
            expect(response.status).to.equal(200);
            resBody = response.body;
        });

        it("returns correct check body field types", async () => {
            expect(resBody.paget).to.be.an("object");
            expect(resBody.paget.currentPageQuery).to.be.an("object");
            expect(resBody.paget.pageItems).to.be.an("array");
        });

        it("returns correct number of pageItems", async () => {
            expect(resBody.paget.pageItems).to.have.length(1);
        });
    });

    describe("relevant pubkeyhash & no pageQuery", () => {
        let response, resBody;
        before(async () => {
            response = await request
                .post("/txo-at-address")
                .send(atAddressBody(undefined, undefined, undefined, env.relevant_txOut_pubkeyhash));
            expect(response.status).to.equal(200);
            resBody = response.body;
        });

        it("returns correct number of pageItems", async () => {
            expect(resBody.paget.pageItems).to.have.length(3);
        });
    });
});