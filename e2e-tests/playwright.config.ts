import { PlaywrightTestConfig, devices } from '@playwright/test';

const config: PlaywrightTestConfig = {
  timeout: 60000,
  expect: {
    timeout: 40000,
    toMatchSnapshot: {
      threshold: 0.3,
    },
  },
  reporter: [
    //[process.env.CI ? 'github' : 'list'],
    ['list'],
    ['html', { open: 'never' , outputFolder: 'html-report'}]
  ],
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  workers: 1,
  use: {
    trace: 'on-first-retry',
    ignoreHTTPSErrors: true
  },
  projects: [
    {
      name: 'chromium',
      use: {
        browserName: 'chromium',
        viewport: { width: 1280, height: 720 },
      },
    },
    {
      name: 'firefox',
      use: {
        browserName: 'firefox',
        viewport: { width: 1280, height: 720 },
      }
    },
/*    {
      name: 'safari',
      use: {
        browserName: 'webkit',
        viewport: { width: 1280, height: 720 },
      }
    }*/
  ],
};
export default config;
