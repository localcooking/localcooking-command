{-# LANGUAGE
    NamedFieldPuns
  , OverloadedLists
  , OverloadedStrings
  #-}

module Main.Options where

import Lib (Command (..))
-- import LocalCooking.Database.Query.Salt (getPasswordSalt)
import LocalCooking.Database.Schema (migrateAll)


import Options.Applicative (Parser, strOption, option, auto, long, help, value, showDefault, subparser, command, argument, str, progDesc, metavar, info)
import qualified Data.ByteString.UTF8 as BS8
import Data.Monoid ((<>))
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Sql (ConnectionPool, runMigration, runSqlPool)
import Database.Persist.Postgresql (createPostgresqlPool)



commandParser :: Parser Command
commandParser = subparser $
     command "add-role" (info addParser (progDesc "Add a role to a user"))
  <> command "get-user" (info getUserParser (progDesc "Get a stored user"))
  <> command "get-users" (info getUsersParser (progDesc "Get a stored user"))
  where
    addParser = AddRole
            <$> argument str (metavar "EMAIL")
            <*> argument str (metavar "ROLE")
    getUserParser = GetUser
            <$> argument str (metavar "EMAIL")
    getUsersParser = pure GetUsers


data ArgsImpl = ArgsImpl
  { argsImplDbHost     :: String
  , argsImplDbPort     :: Int
  , argsImplDbUser     :: String
  , argsImplDbPassword :: String
  , argsImplDbName     :: String
  , argsImplCommand    :: Command
  }


args :: String -> Parser ArgsImpl
args username = ArgsImpl
             <$> parseDbHost
             <*> parseDbPort
             <*> parseDbUser
             <*> parseDbPassword
             <*> parseDbName
             <*> commandParser
  where
    parseDbHost = strOption $
      long "db-host" <> help "Hostname of the PostgreSQL database"
        <> value "localhost" <> showDefault
    parseDbPort = option auto $
      long "db-port" <> help "Port of the PostgreSQL database"
        <> value 5432 <> showDefault
    parseDbUser = strOption $
      long "db-user" <> help "User for the PostgreSQL database"
    parseDbPassword = strOption $
      long "db-password" <> help "Password for the PostgreSQL database"
    parseDbName = strOption $
      long "db-name" <> help "Database name for the PostgreSQL pooled connection"


mkEnv :: ArgsImpl -> IO (ConnectionPool, Command)
mkEnv
  ArgsImpl
    { argsImplDbHost
    , argsImplDbUser
    , argsImplDbPassword
    , argsImplDbPort
    , argsImplDbName
    , argsImplCommand
    } = do
  db <- do
    let connStr = "host=" <> BS8.fromString argsImplDbHost
               <> " port=" <> BS8.fromString (show argsImplDbPort)
               <> " user=" <> BS8.fromString argsImplDbUser
               <> " password=" <> BS8.fromString argsImplDbPassword
               <> " dbname=" <> BS8.fromString argsImplDbName
    runStderrLoggingT (createPostgresqlPool connStr 10)

  migrateAll db

  -- envSalt <- getPasswordSalt envDatabase

  pure (db, argsImplCommand)
