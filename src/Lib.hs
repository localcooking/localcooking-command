{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  #-}

module Lib where

import LocalCooking.Common.User.Role (userRoleParser)
import LocalCooking.Database.Schema
  ( StoredUser, Unique (UniqueEmail), addRole)

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.ByteString.UTF8 as BS8
import Data.Attoparsec.Text (parseOnly)
import Text.EmailAddress (emailAddress)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Database.Persist (Entity (..))
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Database.Persist.Class (get, getBy, selectList)
import System.Exit (exitFailure)


data Command
  = AddRole
    { addRoleUserEmail :: String
    , addRoleUserRole  :: String
    }
  | GetUser
    { getUserEmail :: String
    }
  | GetUsers
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
        flip runSqlPool backend $ do
          mUserId <- getBy $ UniqueEmail email
          case mUserId of
            Nothing -> liftIO $ do
              putStrLn $ "Couldn't find user: " ++ show email
              exitFailure
            Just (Entity userId _) -> do
              addRole userId role
              liftIO $ putStrLn "Role added."
  GetUser emailString -> case emailAddress (BS8.fromString emailString) of
    Nothing -> do
      putStrLn $ "Couldn't parse email: " ++ emailString
      exitFailure
    Just email -> do
      flip runSqlPool backend $ do
        mUserId <- getBy $ UniqueEmail email
        case mUserId of
          Nothing -> liftIO $ do
            putStrLn $ "Couldn't find user: " ++ show email
            exitFailure
          Just (Entity userId _) -> do
            storedUserEnt <- get userId
            liftIO $ print storedUserEnt
  GetUsers -> do
    -- TODO prettyprint
    (us :: [Entity StoredUser]) <- flip runSqlPool backend $ selectList [] []
    forM_ us print
