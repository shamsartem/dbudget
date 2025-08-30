import { VitePWA } from 'vite-plugin-pwa'
import topLevelAwait from 'vite-plugin-top-level-await'
import wasm from 'vite-plugin-wasm'
import { defineConfig } from 'vitest/config'

// eslint-disable-next-line import/no-default-export
export default defineConfig({
  plugins: [
    topLevelAwait(),
    wasm(),
    VitePWA({
      registerType: 'autoUpdate',
      injectRegister: 'auto',
      manifest: {
        name: 'dbudget',
        short_name: 'dbudget',
        description: 'decentralized budgeting app',
        icons: [
          {
            src: '/android-chrome-192x192.png',
            sizes: '192x192',
            type: 'image/png',
          },
          {
            src: '/android-chrome-512x512.png',
            sizes: '512x512',
            type: 'image/png',
          },
        ],
      },
    }),
  ],
  worker: {
    format: 'es',
    plugins: [wasm()],
  },
  optimizeDeps: {
    // This is necessary because otherwise `vite dev` includes two separate
    // versions of the JS wrapper. This causes problems because the JS
    // wrapper has a module level variable to track JS side heap
    // allocations, and initializing this twice causes horrible breakage
    exclude: [
      '@automerge/automerge-wasm',
      '@automerge/automerge-wasm/bundler/bindgen_bg.wasm',
      '@syntect/wasm',
    ],
  },
  test: {},
})
