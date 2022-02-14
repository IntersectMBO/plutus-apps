module.exports = {
  root: true,
  parser: '@typescript-eslint/parser',
  parserOptions: {
    project: "./tsconfig.json",
  },
  plugins: [
    '@typescript-eslint',
    'sort-imports-es6-autofix',
  ],
  extends: [
    'eslint:recommended',
    'plugin:@typescript-eslint/recommended',
  ],
  settings: {
    "import/resolver": {
      "typescript": {}
    }
  },
  rules: {
    "no-unused-vars": 0,
    "linebreak-style": [
      2,
      "unix"
    ],
    "new-cap": 0,
    "no-unused-expressions": 0,
    "no-useless-constructor": 0,
    "sort-imports-es6-autofix/sort-imports-es6": "warn",
    "sort-imports": ["warn", { ignoreDeclarationSort: true }],
    "@typescript-eslint/no-floating-promises": ["error"],
    '@typescript-eslint/explicit-module-boundary-types': 0,
    "max-len": ["error", { "code": 120 }],
    "@typescript-eslint/ban-types": 0,
    '@typescript-eslint/no-non-null-assertion': 0,
    "@typescript-eslint/no-shadow": ["error"],
    "@typescript-eslint/no-unused-vars": ['warn', { argsIgnorePattern: '^_', varsIgnorePattern: '^_' }],
    "@typescript-eslint/no-var-requires": 0,
    "template-tag-spacing": 0,
    'promise/avoid-new': 0,
    'consistent-return': 0, // typescript checks return types
    "no-shadow": "off", // eslint compains about TS enums hence disable here and enable @typescript-eslint/no-shadow
    "import/no-unresolved": 0
  }
};