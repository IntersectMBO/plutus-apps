import { test, expect } from '@playwright/test';
//import envs from '../envs.json';
import envs from '../envs.json';

const { EditorPage } = require('../pageobject/editor');
const { SimulatorPage } = require('../pageobject/simulator');

/*const prod: { name: string, url: string }[] = [
  //{ "name" : "local",  "url" : "https://localhost:8009/" }
  { "name" : "production",  "url" : "http://playground.plutus.iohkdev.io/" }
]
//prod.forEach(function (value) {*/

envs.forEach(function (env) {
  const baseUrl = env.url

  test(`crowd funding test - ${baseUrl}`, async ({ page }) => {
    // given
    const editorPage = new EditorPage(page);
    const simulatorPage = new SimulatorPage(page);
    await editorPage.navigate(baseUrl);
    // when
    await editorPage.compileCrowdFunding();
    await editorPage.simulate();
    await simulatorPage.isOpen();
    await simulatorPage.evaluate();
    //then
    await simulatorPage.confirmCrowdFundingBlockchainTransactions()
  });

  test(`hello world test - ${baseUrl}`, async ({ page }) => {
    // given
    const editorPage = new EditorPage(page);
    const simulatorPage = new SimulatorPage(page);
    await editorPage.navigate(baseUrl);
    // when
    const insertedText = await editorPage.compileHelloWorld();
    await editorPage.simulate();
    await simulatorPage.isOpen();
    await simulatorPage.evaluate();
    //then
    await simulatorPage.checkLogsContainsText(insertedText, 2); // 2 wallets
  });

});

