import Prelude     (IO)
import Application (getAppSettings)

import Import
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql          (createPostgresqlPool, pgConnStr,
                                            pgPoolSize, runSqlPool)

import Lib.Install

main :: IO ()
main = do
  args <- getArgs
  settings <- getAppSettings
  -- Create the database connection pool
  pool <- runStdoutLoggingT $ createPostgresqlPool
      (pgConnStr  $ appDatabaseConf settings)
      (pgPoolSize $ appDatabaseConf settings)

  -- Perform database migration using our application's logging settings.
  runSqlPool (runMigration migrateAll) pool
  runSqlPool doInstall pool
