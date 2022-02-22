import * as env from './testnet-env.json';
import { expect } from "chai";
const request = require("supertest")(env.url);

before(async () => {
    const response = await request.get("/tip");
    expect(response.status).to.equal(200);
    const resBody = response.body;
    expect(resBody.tipSlot.getSlot,
        `Must be synced at least to slot ${env.min_slot} or other tests will fail`).to.be.gte(env.min_slot);
    console.log(`✔️ synced past slot ${env.min_slot}`);
});