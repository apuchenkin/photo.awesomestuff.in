import { RequestHandler } from "express";
import * as basic from 'basic-auth';
import { Connection } from "typeorm";
import { User } from "@app/entity";

const authHandler: RequestHandler = async (req, res, next) => {
  const credentials = basic(req);
  const connection: Connection = req.app.locals.connection;

  if (!credentials) {
    res.sendStatus(401);
    return;
  }

  const user = await connection.manager.findOne(User, {
    where: {
      email: credentials.name,
      password: credentials.pass,
    }
  });

  req.app.locals.user = user;

  if (!user) {
    res.sendStatus(401);
    return;
  }

  next();
};

export default authHandler;