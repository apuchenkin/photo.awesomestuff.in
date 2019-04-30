import { Connection } from "typeorm";

export default (connection: Connection) => photos => {
  console.log(photos);
}