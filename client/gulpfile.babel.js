/* eslint-disable import/no-extraneous-dependencies */
import gulp from 'gulp';
import gutil from 'gulp-util';
import webpack from 'webpack';
import runWebpack from 'webpack-stream';
import nodemon from 'gulp-nodemon';
import del from 'del';
import file from 'gulp-file';
import eslint from 'gulp-eslint';
import WebpackDevServer from 'webpack-dev-server';

import sitemap from './sitemap.babel.js';
import clientConfig from './etc/webpack.config.client.babel';
import serverConfig from './etc/webpack.config.server.babel';

const SRC = 'src';
const DIST = 'dist';

gulp.task('webpack-dev-server', () => {
    // Start a webpack-dev-server
  new WebpackDevServer(webpack(clientConfig), {
    debug: true,
    hot: true,
    inline: true,
    historyApiFallback: true,
  }).listen(8080, 'localhost', (err) => {
    if (err) throw new gutil.PluginError('webpack-dev-server', err);
    // Server listening
    gutil.log('[webpack-dev-server]', 'http://localhost:8080');

    // keep the server alive or continue?
    // callback();
  });
});

// Rerun the task when a file changes
gulp.task('serve', ['build-server'], () => nodemon({
  script: `${DIST}/server.js`,
  watch: SRC,
  tasks: ['build'],
}));

gulp.task('watch', ['webpack-dev-server', 'serve']);

gulp.task('build-server', () => gulp
  .src(`${SRC}/server.js`)
  .pipe(runWebpack(serverConfig, webpack))
  .pipe(gulp.dest(`${DIST}/`))
);

gulp.task('build-client', ['copy'], () => gulp
  .src(`${SRC}/index.js`)
  .pipe(runWebpack(clientConfig, webpack))
  .pipe(gulp.dest(`${DIST}/assets`))
);

gulp.task('clean', () => del(DIST));

gulp.task('copy', ['clean'], () => gulp
  .src(`${SRC}/assets/**/*`)
  .pipe(gulp.dest(`${DIST}/assets`))
);

gulp.task('sitemap', cb =>
  sitemap()
    .catch(cb)
    .then(stream => file('sitemap.xml', stream, { src: true })
      .pipe(gulp.dest(`${DIST}/assets`))
    )
);

gulp.task('lint', () =>
  gulp.src(`${SRC}/**/*.js`)
    // eslint() attaches the lint output to the "eslint" property
    // of the file object so it can be used by other modules.
    .pipe(eslint())
    // eslint.format() outputs the lint results to the console.
    // Alternatively use eslint.formatEach() (see Docs).
    .pipe(eslint.format())
    // To have the process exit with an error code (1) on
    // lint error, return the stream and pipe to failAfterError last.
    .pipe(eslint.failAfterError())
);

gulp.task('build', ['clean', 'copy', 'build-client'], () => {
  gulp.start('lint');
  gulp.start('build-server');
  if (process.argv.includes('--release')) {
    gulp.start('sitemap');
  }
});
