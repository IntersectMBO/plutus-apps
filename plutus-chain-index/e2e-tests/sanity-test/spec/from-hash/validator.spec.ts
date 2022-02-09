const env = require('../../testnet-env.json')
const request = require("supertest")(env.url);
const { expect } = require("chai");

describe("POST /from-hash/validator", () => {
    var response, resBody;
    before(async () => {
        response = await request
            .post("/from-hash/validator")
            .type("application/json")
            .send(`"${env.validator_hash}"`)
        expect(response.status).to.equal(200);
        resBody = response.body;
    });

    it("returns correct validator bytecode", async () => {
        expect(resBody.getValidator).to.eql("4d01000033222220051200120011"); // cbor encoded has prefix '4e'
    });
});