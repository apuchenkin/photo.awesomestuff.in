module.exports = [
   {
      "name": "default",
      "type": "mysql",
      "host": "127.0.0.1",
      "port": 3306,
      "username": "root",
      "password": "aws-photo",
      "database": "aws-photo",
      "synchronize": true,
      "logging": true,
      "entities": [
         "src/entity/**/*.ts"
      ],
      "migrations": [
         "src/migration/**/*.ts"
      ],
      "subscribers": [
         "src/subscriber/**/*.ts"
      ],
      "cli": {
         "entitiesDir": "src/entity",
         "migrationsDir": "src/migration",
         "subscribersDir": "src/subscriber"
      }
   },
   {
      "name": "production",
      "type": "mysql",
      "host": process.env.TYPEORM_HOST || "127.0.0.1",
      "port": process.env.TYPORM_PORT || 3306,
      "username": process.env.TYPORM_USERNAME || "aws-photo",
      "password": process.env.TYPORM_PASSWORD || "aws-photo",
      "database": process.env.TYPORM_DATABASE || "aws-photo",
      "synchronize": process.env.TYPEORM_SYNCHRONIZE || true,
      "logging": process.env.TYPEORM_LOGGING || false,
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
   },
]