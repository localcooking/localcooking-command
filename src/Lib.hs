{-# LANGUAGE
    OverloadedStrings
  #-}

module Lib where

import LocalCooking.Function.Admin (getUsers)
import LocalCooking.Common.User.Role (userRoleParser)
import LocalCooking.Database.Schema.User (Unique (UniqueEmail))
import LocalCooking.Database.Query.Semantics.Admin (addRole)

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as BS8
import Data.Attoparsec.Text (parseOnly)
import Text.EmailAddress (emailAddress)
import Database.Persist (Entity (..))
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Database.Persist.Class (get, getBy)
import System.Exit (exitFailure)


data Command
  = AddRole
    { addRoleUserEmail :: String
    , addRoleUserRole  :: String
    }
  | GetUser
    { getUserEmail :: String
    }
  -- | AddUser
  --   { getUserEmail :: String
  --   }
  -- | DelRole
  --   { addRoleUserEmail :: String
  --   , addRoleUserRole  :: String
  --   }



runCommand :: (ConnectionPool, Command) -> IO ()
runCommand (backend,cmd) = case cmd of
  AddRole emailString roleString -> case emailAddress (BS8.fromString emailString) of
    Nothing -> do
      putStrLn $ "Couldn't parse email: " ++ emailString
      exitFailure
    Just email -> case parseOnly userRoleParser (T.pack roleString) of
      Left e -> do
        putStrLn $ "Couldn't parse role: " ++ roleString ++ ", " ++ e
        exitFailure
      Right role -> do
        mUserId <- flip runSqlPool backend $ getBy $ UniqueEmail email
        case mUserId of
          Nothing -> do
            putStrLn $ "Couldn't find user: " ++ show email
            exitFailure
          Just (Entity userId _) -> do
            addRole backend userId role
            putStrLn "Role added."
  GetUser emailString -> case emailAddress (BS8.fromString emailString) of
    Nothing -> do
      putStrLn $ "Couldn't parse email: " ++ emailString
      exitFailure
    Just email -> do
      mUserId <- flip runSqlPool backend $ getBy $ UniqueEmail email
      case mUserId of
        Nothing -> do
          putStrLn $ "Couldn't find user: " ++ show email
          exitFailure
        Just (Entity userId _) -> do
          storedUserEnt <- flip runSqlPool backend (get userId)
          print storedUserEnt
