{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
module Main (main, getLink, cliGet) where

import Control.Exception        (throwIO)
import Control.Monad.IO.Class   (MonadIO (..))
import GHC.Generics             (Generic)
import Network.Wai.Handler.Warp (run)
import System.Environment       (getArgs)

import Servant
import Servant.Client (runClientM)
import Servant.Record

-------------------------------------------------------------------------------
-- "Record"
-------------------------------------------------------------------------------

data Record m = Record
    { _get :: m (Capture "id" Int :> Get '[JSON] String)
    , _put :: m (ReqBody '[JSON] Int :> Put '[JSON] Bool)
    }
  deriving (Generic)

record :: Record (Srv IO)
record = Record
    { _get = Srv $ return . show
    , _put = Srv $ return . odd
    }

-------------------------------------------------------------------------------
-- example of safe link
-------------------------------------------------------------------------------

getLink :: Int -> Link
getLink = recordLink id _get

-------------------------------------------------------------------------------
-- Example of client
-------------------------------------------------------------------------------

cliRecord :: Record (Cli IO)
cliRecord = recordClient (\x -> runClientM x env >>= either throwIO return)
  where
    env = error "example environment"

cliGet :: Int -> IO String
cliGet = unCli (_get cliRecord)

-------------------------------------------------------------------------------
-- app
-------------------------------------------------------------------------------

app :: Application
app = serveRecord liftIO record

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("run":_) -> do
            putStrLn "Starting servant-record:example at http://localhost:8000"
            run 8000 app
        _ -> putStrLn "To run, pass 'run' argument: cabal new-run servant-record run"

