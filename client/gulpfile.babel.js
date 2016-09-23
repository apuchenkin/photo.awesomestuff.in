/* eslint-disable import/no-extraneous-dependencies */
import gulp from 'gulp';
import webpack from 'webpack';
import runWebpack from 'webpack-stream';
import nodemon from 'gulp-nodemon';
import del from 'del';

import clientConfig from './etc/webpack.config.client.babel';
import serverConfig from './etc/webpack.config.server.babel';

const SRC = 'src';
const DIST = 'dist';

// Rerun the task when a file changes
gulp.task('watch', ['build-server'], () => nodemon({
  script: `${DIST}/server.js`,
  watch: SRC,
  tasks: ['build'],
}));

gulp.task('build-server', ['copy', 'build-client'], () => gulp
  .src(`${SRC}/server.js`)
  .pipe(runWebpack(serverConfig, webpack))
  .pipe(gulp.dest(`${DIST}/`))
);

gulp.task('build-client', ['copy'], () => gulp
  .src(`${SRC}/index.js`)
  .pipe(runWebpack(clientConfig, webpack))
  .pipe(gulp.dest(`${DIST}/assets`))
);

gulp.task('clean', () =>
  del(DIST)
);

gulp.task('copy', ['clean'], () => gulp
  .src(`${SRC}/assets/**/*`)
  .pipe(gulp.dest(`${DIST}/assets`))
);

gulp.task('build', ['clean', 'copy', 'build-client', 'build-server']);
