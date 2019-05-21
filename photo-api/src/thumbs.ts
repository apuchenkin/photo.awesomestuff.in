import * as glob from 'glob';
import { generateThumb } from './handler/admin/thumbs';

// options is optional
glob("./static/photo/**/*.@(jpg|jpeg)", (er, files) => {
  files.map(file => {
    console.log(`persist ${file}`);
    generateThumb(file.replace('./static/photo/', ''))
  });
});