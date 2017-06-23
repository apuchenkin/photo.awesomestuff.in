module.exports = {
  "root": true,
  // "parser": "babel-eslint",
  "env": {
    "browser": true,
    "node": true
  },
  "extends": "airbnb",
  "plugins": [
    "react",
    "jsx-a11y",
    "import"
  ],
  "globals": {
    "ga": true, //# google analytics
    "__DEV__": true,
    "isBrowser": true,
  },
  "rules": {
    "react/jsx-filename-extension": "off",
  }
};
