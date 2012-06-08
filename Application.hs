{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Yesod.Logger (Logger, logBS, toProduction)
import Network.Wai.Middleware.RequestLogger (logCallback, logCallbackDev)
import qualified Database.Persist.Store
import Database.Persist.GenericSql (runMigration)
import Network.HTTP.Conduit (newManager, def)
import Database.Persist.GenericSql.Raw (execute)

-- Import all relevant handler modules here.
import Handler.AssetGroupElement 
import Handler.AssetGroup        
import Handler.Asset             
import Handler.Home              
import Handler.Rent              
import Handler.Review            
import Handler.File            
import Handler.User

#ifdef HEROKU
import qualified Data.Aeson as AT
import qualified Data.HashMap.Strict as M
import qualified Web.Heroku
#endif

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.

#ifdef HEROKU
makeApplication :: AppConfig DefaultEnv Extra -> Logger -> IO Application
makeApplication conf logger = do
  manager <- newManager def
  s <- staticSite
  hconfig <- loadHerokuConfig
  dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
            (Database.Persist.Store.loadConfig . combineMappings hconfig) >>=
            Database.Persist.Store.applyEnv
  p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
  Database.Persist.Store.runPool dbconf (runMigration migrateAll) p
  let foundation = App conf setLogger s p manager dbconf
  app <- toWaiAppPlain foundation
  return $ logWare app
  where
#ifdef DEVELOPMENT
   logWare = logCallbackDev (logBS setLogger)
   setLogger = logger
#else
   logWare = logCallback (logBS setLogger)
   setLogger = toProduction logger -- by default the logger is set for development
#endif

#ifndef DEVELOPMENT
   canonicalizeKey :: (Text, val) -> (Text, val)
   canonicalizeKey ("dbname", val) = ("database", val)
   canonicalizeKey pair = pair

   toMapping :: [(Text, Text)] -> AT.Value
   toMapping xs = AT.Object $ M.fromList $ map (\(key, val) -> (key, AT.String val)) xs
#endif

   combineMappings :: AT.Value -> AT.Value -> AT.Value
   combineMappings (AT.Object m1) (AT.Object m2) = AT.Object $ m1 `M.union` m2
   combineMappings _ _ = error "Data.Object is not a Mapping."

   loadHerokuConfig :: IO AT.Value
   loadHerokuConfig = do
#ifdef DEVELOPMENT
    return $ AT.Object M.empty
#else
    Web.Heroku.dbConnParams >>= return . toMapping . map canonicalizeKey
#endif

#else
makeApplication :: AppConfig DefaultEnv Extra -> Logger -> IO Application
makeApplication conf logger = do
    foundation <- makeFoundation conf setLogger
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
    setLogger = if development then logger else toProduction logger
    logWare   = if development then logCallbackDev (logBS setLogger)
                               else logCallback    (logBS setLogger)
#endif

makeFoundation :: AppConfig DefaultEnv Extra -> Logger -> IO App
makeFoundation conf setLogger = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    Database.Persist.Store.runPool dbconf (runMigration migrateAll) p

    -- Raw sql for setting "SET NULL" on specific constraints
    let raw_sql'1 = "ALTER TABLE asset DROP CONSTRAINT asset_next_fkey; ALTER TABLE asset ADD CONSTRAINT asset_next_fkey FOREIGN KEY (next) REFERENCES asset (id) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE SET NULL;"
        raw_sql'2 = "UPDATE \"user\" SET role='AdminRole' WHERE id=1;"
    putStrLn "Running first query..."
    Database.Persist.Store.runPool dbconf (execute raw_sql'1 []) p
    putStrLn "Running first query..."
    Database.Persist.Store.runPool dbconf (execute raw_sql'2 []) p
    putStrLn "--= Both raw SQL queries OK =--"


    return $ App conf setLogger s p manager dbconf

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
