{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Init where

import Api
import Control.Monad.Reader
import Db (executeDb, createTemplatesTable, createInstancesTable)
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Types

corsPolicy :: CorsResourcePolicy
corsPolicy = 
  simpleCorsResourcePolicy {  
    corsOrigins = Nothing
  , corsMethods = [methodGet, methodPost,methodHead,methodPut,methodDelete,methodTrace,methodConnect,methodOptions,methodPatch]  
  , corsRequestHeaders = ["Content-Type"]
  }


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
app1 config = cors ( const $ Just corsPolicy ) $ serve api $ hoistServer api (nt config) server