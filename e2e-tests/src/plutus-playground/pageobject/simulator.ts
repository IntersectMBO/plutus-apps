import { test, expect } from '@playwright/test';

export class SimulatorPage {
  constructor(page) {
    this.page = page;
    this.walletsHeader = this.page.locator('h2 >> text=Wallets');
    this.evaluateBtn = this.page.locator('button >> text=Evaluate').first();
    this.transactionHeader = this.page.locator('h2 >> text=Transactions');
  }

  async isOpen() {
    await expect(this.walletsHeader).toBeVisible();
  }

  async evaluate() {
    await this.evaluateBtn.click();
    await expect(this.transactionHeader).toBeVisible();
  }

  async checkLogsContainsText(text: string) {
    const logsTextXpath = "//div[@class='logs']//descendant::div[contains(text(),'" + text + "')]"
    await expect(this.page.locator(logsTextXpath)).toBeVisible();
  }

}
