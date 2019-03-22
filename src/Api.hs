{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module Api where

import qualified BusinessLogic                 as BL
import           Control.Monad.Reader
import           Data.Int
import           Data.Time.Calendar
import qualified Db                            as Db
import           Db                             ( Db )
import           Servant
import           Types

type API =  "templates" :> TemplateAPI
       :<|> "instances" :> InstanceAPI

type TemplateAPI = Get '[JSON] [SavedTemplate]
              :<|> Capture "templateId" Int64 :> Get '[JSON] (Maybe SavedTemplate)
              :<|> ReqBody '[JSON] Template :> Post '[JSON] SavedTemplate
              :<|> Capture "templateId" Int64 :> ReqBody '[JSON] TemplateUpdateRequest :> Put '[JSON] ()
              :<|> Capture "templateId" Int64 :> Delete '[JSON] ()
type InstanceAPI = Capture "endDate" Day :> Get '[JSON] [Instance]
              :<|> ReqBody '[JSON] Instance :> Post '[JSON] ()
              :<|> Capture "templateId" Int64 :> Capture "date" Day :> Delete '[JSON] ()

server :: ServerT API App
server = templateServer :<|> instanceServer

templateServer :: ServerT TemplateAPI App
templateServer =
  Api.getTemplates
    :<|> Api.getTemplate
    :<|> insertTemplate
    :<|> updateTemplate
    :<|> deleteTemplate

instanceServer :: ServerT InstanceAPI App
instanceServer =
  Api.getInstances :<|> Api.createInstance :<|> Api.deleteInstance

getTemplates :: App [SavedTemplate]
getTemplates = runDbCommand Db.getAllTemplates

getTemplate :: Int64 -> App (Maybe SavedTemplate)
getTemplate = runDbCommand . Db.getTemplateById . TemplateId

insertTemplate :: Template -> App SavedTemplate
insertTemplate = runDbCommand . Db.insertTemplate

updateTemplate :: Int64 -> TemplateUpdateRequest -> App ()
updateTemplate tId t = runDbCommand $ Db.updateTemplate (TemplateId tId) t

deleteTemplate :: Int64 -> App ()
deleteTemplate = runDbCommand . Db.deleteTemplate . TemplateId

getInstances :: Day -> App [Instance]
getInstances e =
  runDbCommand $ BL.getInstances e <$> Db.getAllInstances <*> Db.getAllTemplates

createInstance :: Instance -> App ()
createInstance = runDbCommand . Db.createInstance

deleteInstance :: Int64 -> Day -> App ()
deleteInstance tId d = runDbCommand $ Db.deleteInstance (TemplateId tId) d

runDbCommand :: Db a -> App a
runDbCommand db = asks _configDb >>= Db.executeDb db
