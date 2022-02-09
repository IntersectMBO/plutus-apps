const env = require('../../testnet-env.json')
const request = require("supertest")(env.url);
const { expect } = require("chai");

//there's any example of stake validator onchain yet
describe("POST /from-hash/stake-validator", () => {
    var response, resBody;
    before(async () => {
        response = await request
            .post("/from-hash/minting-policy")
            .type("application/json")
            .send(`"${env.minting_policy_hash}"`)
        expect(response.status).to.equal(200);
        resBody = response.body;
    });

    it("returns correct minting policy bytecode");
});