{-# LANGUAGE FlexibleInstances #-}
module Capability (TemplateQuery(..), TemplateCommand(..), InstanceQuery(..), InstanceCommand(..)) where

import Types
import           Data.Time.Calendar
import Db
import           Servant
import           Control.Monad.Reader

class Monad m => TemplateQuery m where
  getTemplates :: m [SavedTemplate]
  getTemplate  :: TemplateId -> m (Maybe SavedTemplate)

class Monad m => TemplateCommand m where
  insertTemplate :: Template -> m SavedTemplate
  updateTemplate :: TemplateId -> TemplateUpdateRequest -> m ()
  deleteTemplate :: TemplateId -> m ()

class Monad m => InstanceQuery m where
  getInstances :: m [Instance]

class Monad m => InstanceCommand m where
  createInstance :: Instance -> m ()
  deleteInstance :: TemplateId -> Day -> m ()

instance TemplateQuery (AppT Handler) where
  getTemplates = runDbCommand Db.getAllTemplates
  getTemplate = runDbCommand . Db.getTemplateById

instance TemplateCommand (AppT Handler) where
  insertTemplate = runDbCommand . Db.insertTemplate
  updateTemplate tId req = runDbCommand $ Db.updateTemplate tId req
  deleteTemplate = runDbCommand . Db.deleteTemplate

instance InstanceQuery (AppT Handler) where
  getInstances = runDbCommand Db.getAllInstances

instance InstanceCommand (AppT Handler) where
  createInstance = runDbCommand . Db.createInstance
  deleteInstance tId d = runDbCommand $ Db.deleteInstance tId d

runDbCommand :: Db a -> App a
runDbCommand db = do
  config <- asks _configDb
  Db.executeDb config db