// eslint-disable-next-line no-undef
module.exports = {
  root: true,
  parser: '@typescript-eslint/parser',
  plugins: ['@typescript-eslint'],
  extends: [
    'plugin:compat/recommended',
    'eslint:recommended',
    'plugin:@typescript-eslint/recommended',
    'plugin:import/typescript',
  ],
  rules: {
    '@typescript-eslint/no-extra-semi': ['off'],
    'no-console': 'error',
  },
  env: {
    browser: true,
  },
}
