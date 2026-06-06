// Playwright config for the vectors WebGL2 demo.
// Boots the dev server (spago bundle + static server on 0.0.0.0:47474) and runs
// canvas-verification specs against it. Artifacts (screenshots/video/trace) land
// under .a5c/runs artifacts via the html reporter / test output.
import { defineConfig, devices } from '@playwright/test';

const PORT = 47474;

export default defineConfig({
  testDir: './e2e',
  fullyParallel: false,
  workers: 1,
  timeout: 30_000,
  expect: { timeout: 10_000 },
  reporter: [['list'], ['html', { outputFolder: 'artifacts/e2e-report', open: 'never' }]],
  use: {
    baseURL: `http://localhost:${PORT}`,
    trace: 'retain-on-failure',
    screenshot: 'only-on-failure',
    video: 'retain-on-failure',
  },
  projects: [{
    name: 'chromium',
    use: {
      ...devices['Desktop Chrome'],
      // Headless Chromium needs SwiftShader/ANGLE to keep a WebGL2 context alive.
      launchOptions: {
        args: [
          '--use-gl=angle',
          '--use-angle=swiftshader',
          '--ignore-gpu-blocklist',
          '--enable-unsafe-swiftshader',
          '--enable-webgl',
        ],
      },
    },
  }],
  webServer: {
    command: 'npm run dev',
    url: `http://localhost:${PORT}`,
    reuseExistingServer: true,
    timeout: 120_000,
  },
});
