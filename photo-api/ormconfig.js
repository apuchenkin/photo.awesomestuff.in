module.exports = {
   "type": "mysql",
   "host": process.env.TYPEORM_HOST || "127.0.0.1",
   "port": process.env.TYPORM_PORT || 3306,
   "username": process.env.TYPORM_USERNAME || "root",
   "password": process.env.TYPORM_PASSWORD || "aws-photo",
   "database": process.env.TYPORM_DATABASE || "aws-photo",
   "synchronize": true,
   "logging": true,
   "entities": [
      "build/entity/**/*.js"
   ],
   "migrations": [
      "build/migration/**/*.js"
   ],
   "subscribers": [
      "build/subscriber/**/*.js"
   ],
   "cli": {
      "entitiesDir": "build/entity",
      "migrationsDir": "build/migration",
      "subscribersDir": "build/subscriber"
   }
}