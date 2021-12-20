import { test, expect } from '@playwright/test';
import envs from '../envs';

const { EditorPage } = require('../pageobject/editor');
const { SimulatorPage } = require('../pageobject/simulator');

const prod: { name: string, url: string }[] = [
  { "name" : "local",  "url" : "https://localhost:8009/" }
  //{ "name" : "production",  "url" : "http://playground.plutus.iohkdev.io/" }
]
//envs.forEach(function (value: string) {
prod.forEach(function (value: string) {
  const baseUrl = value.url

  test(`sanity - ${baseUrl}`, async ({ page }) => {
    test.skip()
    const editorPage = new EditorPage(page);
    await editorPage.navigate(baseUrl);
  });

  test(`hello world - ${baseUrl}`, async ({ page }) => {
//    test.fail("DecodingError");
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
    await simulatorPage.checkLogsContainsText(insertedText);
  });

});

