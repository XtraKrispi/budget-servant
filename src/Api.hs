{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module Api where

import qualified BusinessLogic                 as BL
import           Data.Time.Calendar
import           Servant
import           Types
import Capability

type API =  "templates" :> TemplateAPI
       :<|> "instances" :> InstanceAPI

type TemplateAPI = Get '[JSON] [SavedTemplate]
              :<|> Capture "templateId" TemplateId :> Get '[JSON] (Maybe SavedTemplate)
              :<|> ReqBody '[JSON] Template :> Post '[JSON] SavedTemplate
              :<|> Capture "templateId" TemplateId :> ReqBody '[JSON] TemplateUpdateRequest :> Put '[JSON] ()
              :<|> Capture "templateId" TemplateId :> Delete '[JSON] ()
type InstanceAPI = Capture "endDate" Day :> Get '[JSON] [Instance]
              :<|> ReqBody '[JSON] Instance :> Post '[JSON] ()
              :<|> Capture "templateId" TemplateId :> Capture "date" Day :> Delete '[JSON] ()

server :: ServerT API App
server = templateServer :<|> instanceServer

templateServer :: ServerT TemplateAPI App
templateServer =
  Capability.getTemplates
    :<|> Capability.getTemplate
    :<|> Capability.insertTemplate
    :<|> Capability.updateTemplate
    :<|> Capability.deleteTemplate

instanceServer :: ServerT InstanceAPI App
instanceServer =
  BL.getInstances :<|> Capability.createInstance :<|> Capability.deleteInstance
