import { defineConfig } from 'vite'
import elmPlugin from 'vite-plugin-elm'
import { VitePWA } from 'vite-plugin-pwa'

// eslint-disable-next-line import/no-default-export
export default defineConfig({
  plugins: [
    elmPlugin({ debug: false }),
    VitePWA({
      includeAssets: [
        'apple-touch-icon.png',
        'browserconfig.xml',
        'favicon-16x16.png',
        'favicon-32x32',
        'favicon.ico',
        'mstile-150x150.png',
        'robots.txt',
        'safari-pinned-tab.svg',
      ],
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
        theme_color: '#161d1d',
        background_color: '#161d1d',
        display: 'standalone',
      },
    }),
  ],
})
