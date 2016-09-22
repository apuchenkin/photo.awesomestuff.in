/* eslint-disable import/no-extraneous-dependencies */
import gulp from 'gulp';
import webpack from 'webpack-stream';
import nodemon from 'gulp-nodemon';

import config from './etc/webpack.config.server.babel';

// Rerun the task when a file changes
gulp.task('watch', ['build'], () => nodemon({
  script: 'dist/server.js',
  watch: 'src',
  tasks: ['build'],
}));

gulp.task('build', () => gulp.src('src/entry.js')
  .pipe(webpack(config))
  .pipe(gulp.dest('dist/'))
);
