import "reflect-metadata";
import { getConnectionOptions, createConnection } from "typeorm";
import app from './app';

const PORT = process.env.PORT || 3000;

(async () => {
  const env = process.env.NODE_ENV === 'production' ? 'production' : 'default';
  const connectionOptions = await getConnectionOptions(env);

  // create a connection using modified connection options
  const connection = await createConnection(connectionOptions);

  app.locals.connection = connection;
  app.listen(PORT, () => {
    // eslint-disable-next-line no-console
    console.log(`Server listening on: ${PORT}`);
  });
})()

