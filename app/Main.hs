module Main where

import Main.Options (args, mkEnv)
import Lib (Command, runCommand)

import Options.Applicative (execParser, info, helper, fullDesc, progDesc, header)
import Data.Monoid ((<>))
import Data.Pool (destroyAllResources)
import Control.Exception.Safe (bracket)
-- import System.Posix.User (getLoginName)
import System.Environment (getEnv)
import Control.Logging (withStderrLogging)
import Path.Extended (ToLocation, FromLocation)
import Database.Persist.Sql (ConnectionPool)





main :: IO ()
main = do
  username <- getEnv "USER"

  let allocate :: IO (ConnectionPool, Command)
      allocate = do
        as <- execParser (opts username)
        mkEnv as

      release :: (ConnectionPool, Command) -> IO ()
      release (backend,_) =
        destroyAllResources backend

  withStderrLogging $
    bracket allocate release runCommand

  where
    head' = "Local Cooking Command"
    opts u = info (helper <*> args u) $ fullDesc <> progDesc desc <> header head'
    desc = "run a one-off command"
