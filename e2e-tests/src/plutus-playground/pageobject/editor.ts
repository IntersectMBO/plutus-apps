import { test, expect } from '@playwright/test';

export class EditorPage {
  element = class {
    static readonly title = 'text=Plutus playground';
    static readonly h1 = 'h1 >> text=Editor';
    static readonly helloWorld = 'a >> text=Hello, world';
    static readonly starter = 'a >> text=Starter';
    static readonly game = 'a >> text=Starter';
    static readonly vesting = 'a >> text=Starter';
    static readonly crowdFunding = 'a >> text=Starter';
    static readonly errorHandling = 'a >> text=Starter';
    static readonly helloWorldText = "//div[@class='code-editor']/descendant::span[contains(text(), 'Hello') and contains(text(), 'world')]"; // "Hello, world" text in code editor
    static readonly compileBtn = 'button >> text=Compile';
  }

  constructor(page) {
    this.page = page;
    this.title = this.page.locator(this.element.title);
    this.h1 = this.page.locator(this.element.h1);
    this.helloWorld = this.page.locator(this.element.helloWorld);
    this.starter = this.page.locator(this.element.starter);
    this.game = this.page.locator(this.element.game);
    this.vesting = this.page.locator(this.element.vesting);
    this.crowdFunding = this.page.locator(this.element.crowdFunding);
    this.errorHandling = this.page.locator(this.element.errorHandling);
    this.helloWorldText = this.page.locator(this.element.helloWorldText);
    this.compileBtn = this.page.locator(this.element.compileBtn);
  }

  async navigate(base: string) {
    await this.page.goto(base);
    await expect(this.title).toHaveText('Plutus playground');
    await expect(this.h1).toHaveText('Editor');
  }

  async compileHelloWorld() {
    await this.helloWorld.click();
    await expect(this.helloWorldText).toBeVisible();
    await this.compileBtn.click();
  }

}
