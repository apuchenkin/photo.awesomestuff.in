module.exports = ({ file }) => ({
  plugins: {
    'postcss-import': { root: file.dirname },
    'postcss-cssnext': { browsers: 'last 2 version' },
    cssnano: {},
  },
});
