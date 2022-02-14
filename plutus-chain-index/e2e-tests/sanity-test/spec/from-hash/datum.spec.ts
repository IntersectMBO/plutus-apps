import * as env from '../../testnet-env.json';
import { expect } from "chai";
const request = require("supertest")(env.url);

describe("POST /from-hash/datum", () => {
    describe("relevant datum hash", () => {
        let response, resBody;
        before(async () => {
            response = await request
                .post("/from-hash/datum")
                .type("application/json")
                .send(`"${env.relevant_datum_hash}"`)
            expect(response.status).to.equal(200);
            resBody = response.body;
        });

        it("returns correct datum value", async () => {
            expect(resBody).to.eql("19029a"); // uint16_t 666
        });
    });

    describe("not relevant datum hash", () => {
        let response, resBody;
        before(async () => {
            response = await request
                .post("/from-hash/datum")
                .type("application/json")
                .send(`"${env.not_relevant_datum_hash}"`)
            expect(response.status).to.equal(404);
            /* eslint-disable @typescript-eslint/no-unused-vars */
            resBody = response.body;
        });

        it("returns correct datum value", async () => {
            expect(response.body).to.be.eql({}); // undefined?
        });
    });
});