import * as env from '../../testnet-env.json';
import { expect } from "chai";
const request = require("supertest")(env.url);

describe("POST /from-hash/validator", () => {
    let response, resBody;
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