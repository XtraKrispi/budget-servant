{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Init where

import           Api
import           Servant
import           Control.Monad.Reader
import           Network.Wai.Handler.Warp
import           Types
import Db (executeDb, createTemplatesTable, createInstancesTable)
  

runApp :: IO ()
runApp = do
  let config = Config (DbConfiguration "budget.db" ConsoleTracing)
  executeDb (createTemplatesTable >> createInstancesTable) (_configDb config)
  run 8081 (app1 config)

api :: Proxy API
api = Proxy

nt :: Config -> App a -> Handler a
nt s x = runReaderT (unAppT x) s

app1 :: Config -> Application
app1 config = serve api $ hoistServer api (nt config) server