import "reflect-metadata";
import { createConnection } from "typeorm";
import fetchData from './fetchData';
import {
  migrateCategories,
  migratePages,
  migratePhotos,
} from './migrate';

(async () => {
  const connection = await createConnection();
  const data = await fetchData();

  if (data.pages) {
    await migratePages(connection)(data.pages);
  }

  if (data.categories) {
    await migrateCategories(connection)(data.categories);
  }

  if (data.photos) {
    await migratePhotos(connection)(data.photos);
  }

  console.log('Done!');
})();
