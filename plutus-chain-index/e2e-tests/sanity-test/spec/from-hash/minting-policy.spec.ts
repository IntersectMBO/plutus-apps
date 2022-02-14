import * as env from '../../testnet-env.json';
import { expect } from "chai";
const request = require("supertest")(env.url);

describe("POST /from-hash/minting-policy", () => {
    let response, resBody;
    before(async () => {
        response = await request
            .post("/from-hash/minting-policy")
            .type("application/json")
            .send(`"${env.minting_policy_hash}"`)
        expect(response.status).to.equal(200);
        resBody = response.body;
    });

    it("returns correct minting policy bytecode", async () => {
        expect(resBody.getMintingPolicy).to.eql(env.minting_policy);
    });
});