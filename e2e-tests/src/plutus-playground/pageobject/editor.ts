import { test, expect } from '@playwright/test';

export class EditorPage {
  readonly wonderfulText :string = 'wonderful';
  readonly notCompiledText = 'Not compiled';
  readonly compilingText = 'Compiling ...';
  readonly compilationSuccessfulText = 'Compilation successful';

  constructor(page) {
    this.page = page;
    this.title = this.page.locator("text=Plutus playground");
    this.h1 = this.page.locator("h1 >> text=Editor");
    this.helloWorld = this.page.locator("a >> text=Hello, world");
    this.starter = this.page.locator("a >> text=Starter");
    this.game = this.page.locator("a >> text=Game");
    this.vesting = this.page.locator("a >> text=Vesting");
    this.crowdFunding = this.page.locator("a >> text=Crowd Funding");
    this.errorHandling = this.page.locator("a >> text=Error Handling");
    this.helloWorldText = this.page.locator("//div[@class='code-editor']/descendant::span[contains(text(), 'Hello') and contains(text(), 'world')]"); // text in code editor
    this.compileBtn = this.page.locator("button >> text=Compile");
    this.simulateBtn = this.page.locator("//button >> text=Simulate");
    this.feedbackHeader = this.page.locator("//div[@class='editor-feedback-header']");
  }

  async navigate(base: string) {
    await this.page.goto(base);
    await expect(this.title).toHaveText('Plutus playground');
    await expect(this.h1).toHaveText('Editor');
  }

  async modifyHelloWorldText(text: string) {
    await expect(this.feedbackHeader).toHaveText(this.compilationSuccessfulText); // it's already compiled becuse it's an example?
    await this.helloWorldText.click();
    await this.page.keyboard.type(text);
  }

  async compileHelloWorld(): string {
    const textToInsert = this.wonderfulText;
    await this.helloWorld.click();
    await this.modifyHelloWorldText(textToInsert);
    await expect(this.helloWorldText).toBeVisible();
    await this.compile();
    return textToInsert;
  }

  async compile() {
    await this.compileBtn.click();
    await expect(this.feedbackHeader).toHaveText(this.compilingText);
    await expect(this.feedbackHeader).toHaveText(this.compilationSuccessfulText);
  }

  async simulate() {
    await this.simulateBtn.click();
  }

}
