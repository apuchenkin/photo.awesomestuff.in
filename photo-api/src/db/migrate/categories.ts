import { Connection } from "typeorm";

export default (connection: Connection) => categories => {
  console.log(categories);
}