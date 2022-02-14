import * as env from '../testnet-env.json'
import { expect } from "chai";
const request = require("supertest")(env.url);

describe("GET /tip", async () => {
  let response, resBody;
  before(async () => {
    response = await request.get("/tip");
    expect(response.status).to.equal(200);
    resBody = response.body;
  });

  it("returns correct fields", async () => {
    expect(resBody.tag).to.equal("Tip");
    expect(resBody.tipBlockNo).to.be.a("number");
    expect(resBody.tipBlockId).to.be.a("string");
    expect(resBody.tipSlot).to.be.an("object");
    expect(resBody.tipSlot.getSlot).to.be.a("number");
  });
});