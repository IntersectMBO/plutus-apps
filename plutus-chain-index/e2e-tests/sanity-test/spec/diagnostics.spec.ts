import * as env from '../testnet-env.json';
import { expect } from "chai";
const request = require("supertest")(env.url);

describe("GET /diagnostics", async () => {
  let response, resBody;
  before(async () => {
    response = await request.get("/diagnostics");
    expect(response.status).to.equal(200);
    resBody = response.body;
  });

  it("returns correct fields", async () => {
    expect(resBody.numAssetClasses).to.be.a("number");
    expect(resBody.someTransactions).to.be.an("array");
    expect(resBody.numUnspentOutputs).to.be.a("number");
    expect(resBody.numAddresses).to.be.a("number");
    expect(resBody.numUnmatchedInputs).to.be.a("number");
    expect(resBody.numTransactions).to.be.a("number");
    expect(resBody.numScripts).to.be.a("number");
  });

  it("has some positive field values", async () => {
    expect(resBody.numAssetClasses).to.be.greaterThan(1000);
    expect(resBody.numUnspentOutputs).to.be.greaterThan(1000);
    expect(resBody.numAddresses).to.be.greaterThan(1000);
    expect(resBody.numUnmatchedInputs).to.be.greaterThan(1);
    expect(resBody.numScripts).to.be.greaterThan(100);
  });

});