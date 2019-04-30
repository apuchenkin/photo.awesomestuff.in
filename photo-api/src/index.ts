import "reflect-metadata";
import { createConnection } from "typeorm";
import app from './app';

const PORT = process.env.PORT || 3000;

createConnection().then(async connection => {
  app.locals.connection = connection;
  app.listen(PORT, () => {
    // eslint-disable-next-line no-console
    console.log(`Server listening on: ${PORT}`);
  });
}).catch(error => console.log(error));
