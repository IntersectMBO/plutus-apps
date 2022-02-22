import * as env from '../testnet-env.json';
import { expect } from "chai";
const request = require("supertest")(env.url);

// todo
describe("GET /collect-garbage", async () => {
  let response, resBody;
  before(async () => {
    response = await request.get("/collect-garbage");
    expect(response.status).to.equal(200);
    /* eslint-disable @typescript-eslint/no-unused-vars */
    resBody = response.body;
  });
});