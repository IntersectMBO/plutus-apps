const env = require('../../testnet-env.json')
const request = require("supertest")(env.url);
const { expect } = require("chai");

describe("POST /from-hash/redeemer", () => {
    var response, resBody;
    before(async () => {
        response = await request
            .post("/from-hash/redeemer")
            .type("application/json")
            .send(`"${env.relevant_redeemer_hash}"`)
        expect(response.status).to.equal(200);
        resBody = response.body;
    });

    it("returns correct minting policy bytecode", async () => {
        expect(resBody).to.eql("182a"); // uint8_t 42
    });
});