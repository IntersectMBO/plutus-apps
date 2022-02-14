import * as env from '../testnet-env.json'
import { expect } from "chai";
const request = require("supertest")(env.url);

describe("POST /tx", () => {
    let response, resBody;
    before(async () => {
        response = await request
            .post("/tx")
            .send({ "getTxId": env.txId_withscript });
        expect(response.status).to.equal(200);
        resBody = response.body;
    });

    it("returns correct fields", async () => {
        expect(resBody._citxInputs).to.be.an("array");
        expect(resBody._citxRedeemers).to.be.an("array");
        expect(resBody._citxCardanoTx).to.be.an("object");
        expect(resBody._citxValidRange).to.be.an("object");
        expect(resBody._citxOutputs).to.be.an("object");
        expect(resBody._citxData).to.be.an("array");
        expect(resBody._citxScripts).to.be.an("array");
        expect(resBody._citxTxId).to.be.an("object");
    });

    it("returns correct _citxInputs"); //pending

    it("returns correct _citxCardanoTx", async () => {
        expect(resBody._citxRedeemers).has.lengthOf.greaterThan(0);
    });

    it("returns correct fields", async () => {
        expect(resBody._citxCardanoTx.eraInMode).is.eql("AlonzoEraInCardanoMode");
        expect(resBody._citxCardanoTx.tx.cborHex).has.lengthOf.greaterThan(0);
    });


    it("returns correct _citxValidRange", async () => {
        expect(resBody._citxValidRange.ivFrom[1]).is.eql(true);
        expect(resBody._citxValidRange.ivTo[1]).is.eql(true);
    });

    it("returns correct _citxOutputs"); //pending

    it("returns correct _citxData", async () => {
        expect(resBody._citxData[0]).has.lengthOf.greaterThan(0);
    });

    it("returns correct _citxScripts", async () => {
        expect(resBody._citxScripts).to.have.lengthOf.greaterThan(0);
        expect(resBody._citxScripts[0][0].getScriptHash).is.eql(env.scriptHash);
        expect(resBody._citxScripts[0][1]).is.eql(env.script);
    });

    it("returns correct _citxTxId", async () => {
        expect(resBody._citxTxId.getTxId).is.eql(env.txId_withscript);
    });
});
