import { test, expect } from '@playwright/test';
import envs from '../envs';

const { EditorPage } = require('../pageobject/editor');

const prod: { name: string, url: string }[] = [
  { "name" : "production",  "url" : "http://playground.plutus.iohkdev.io/" }
]
//envs.forEach(function (value: string) {
prod.forEach(function (value: string) {
  const baseUrl = value.url

  test(`sanity - ${baseUrl}`, async ({ page }) => {
    const editorPage = new EditorPage(page);
    await editorPage.navigate(baseUrl);
  });

  test(`hello world - ${baseUrl}`, async ({ page }) => {
    // given
    const editorPage = new EditorPage(page);
    await editorPage.navigate(baseUrl);
    // when
    await editorPage.compileHelloWorld();
  });

});

