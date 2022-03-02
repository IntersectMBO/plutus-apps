import * as env from '../../testnet-env.json';
import { expect } from "chai";
const request = require("supertest")(env.url);

// TODO: update when there are stake validators onchain to query
// describe("POST /from-hash/stake-validator", () => {
//     let response, resBody;
//     before(async () => {
//         response = await request
//             .post("/from-hash/stake-validator")
//             .type("application/json")
//             .send(`${env.stake_validator_hash}`)
//         expect(response.status).to.equal(200);
//         /* eslint-disable @typescript-eslint/no-unused-vars */
//         resBody = response.body;
//     });

//     it("returns correct stake validator bytecode");
// });