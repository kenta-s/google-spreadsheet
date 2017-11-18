{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import qualified Data.Text as T
import Spreadsheet.Reader
import System.Environment  (getEnv, getArgs)
import Data.Aeson          hiding(json)
import GHC.Generics

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

data SpreadsheetResult = SpreadsheetResult {
  sheetresult :: [[T.Text]]
} deriving (Generic, Show)
instance ToJSON SpreadsheetResult
instance FromJSON SpreadsheetResult

main :: IO ()
main = do
    ref <- newIORef 0
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
    runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app =
    do get root $
           text "Hello World!"
       get ("hello" <//> var) $ \name ->
           do (DummyAppState ref) <- getState
              visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
              text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))

       get ("spreadsheet" <//> var <//> var) $ \sheetId range ->
           do (DummyAppState ref) <- getState
              results <- liftIO $ fetchValueRanges (T.unpack sheetId) (T.unpack range)
              json results
