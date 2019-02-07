{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module Api where

import           Servant
import           Types
import           Data.Time.Calendar
import qualified BusinessLogic                 as BL
import           Control.Monad.Reader
import Db (Db)
import qualified Db as Db
import Data.Int

type API =  "templates" :> TemplateAPI
       :<|> "instances" :> InstanceAPI

type TemplateAPI = Get '[JSON] [SavedTemplate]
              :<|> ReqBody '[JSON] Template :> Post '[JSON] TemplateId
              :<|> Capture "templateId" Int64 :> ReqBody '[JSON] Template :> Put '[JSON] ()
              :<|> Capture "templateId" Int64 :> Delete '[JSON] ()
type InstanceAPI = Capture "startDate" Day :> Capture "endDate" Day :> Get '[JSON] [Instance]

server :: ServerT API App
server = templateServer :<|> instanceServer

templateServer :: ServerT TemplateAPI App
templateServer = Api.getTemplates 
            :<|> insertTemplate 
            :<|> updateTemplate
            :<|> deleteTemplate

instanceServer :: ServerT InstanceAPI App
instanceServer = Api.getInstances

getTemplates :: App [SavedTemplate]
getTemplates = runDbCommand BL.getTemplates

insertTemplate :: Template -> App TemplateId
insertTemplate = runDbCommand . BL.insertTemplate 

updateTemplate :: Int64 -> Template -> App ()
updateTemplate tId t = runDbCommand $ BL.updateTemplate (TemplateId tId) t

deleteTemplate :: Int64 -> App ()
deleteTemplate = runDbCommand . BL.deleteTemplate . TemplateId

getInstances :: Day -> Day -> App [Instance]
getInstances = undefined

runDbCommand :: Db a -> App a
runDbCommand db = asks _configDb >>= Db.executeDb db
