import Prelude     (IO)
import Application (getAppSettings)

import Import
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.MySQL               (createMySQLPool, myConnInfo,
                                             myPoolSize, runSqlPool)

import Lib.Install

main :: IO ()
main = do
  args <- getArgs
  settings <- getAppSettings
  -- Create the database connection pool
  pool <- runStdoutLoggingT $ createMySQLPool
    (myConnInfo $ appDatabaseConf settings)
    (myPoolSize $ appDatabaseConf settings)

  -- Perform database migration using our application's logging settings.
  runSqlPool (runMigration migrateAll) pool
  runSqlPool doInstall pool
