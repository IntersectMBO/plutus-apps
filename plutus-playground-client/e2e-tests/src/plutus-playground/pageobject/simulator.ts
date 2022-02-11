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

  async checkLogsContainsText(text: string, occurances: number) {
    const logsTextXPath = "//div[@class='logs']//descendant::div[contains(text(),'" + text + "')]"
    await expect(this.page.locator(logsTextXPath)).toHaveCount(occurances)
  }

  async confirmCrowdFundingBlockchainTransactions() {
    await this.confirmBlockchainTransaction(0, 0);
    await this.confirmBlockchainTransaction(1, 0);
    await this.confirmBlockchainTransaction(1, 1);
    await this.confirmBlockchainTransaction(1, 2);
    await this.confirmBlockchainTransaction(40, 0);
  }

  private async confirmBlockchainTransaction(slot: number, tx: number) {
    const blockchainTransactionXPath = "//div[@class='row blocks']//div[@class='card-header' and " +
    "descendant::span[text()='Slot' and following-sibling::span[1][text()='" + slot +
    "']] and descendant::span[text()='Tx' and following-sibling::span[text()='" + tx + "']]]";
    await expect(this.page.locator(blockchainTransactionXPath)).toBeVisible()
  }

}
