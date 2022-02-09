const env = require('../testnet-env.json');;
const request = require("supertest")(env.url);
const { expect } = require("chai");

// todo
describe("GET /collect-garbage", async () => {
  var response, resBody;
  before(async () => {
    response = await request.get("/collect-garbage");
    expect(response.status).to.equal(200);
    resBody = response.body;
  });
});