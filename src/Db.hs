{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Db where

import Control.Monad.IO.Class
import Data.Int
import Data.Time.Calendar
import Database.SQLite.Simple
import Text.RawString.QQ
import Types

newtype Db a = Db { runDb :: Connection -> IO a }
  deriving (Functor)

instance Applicative Db where
  pure a = Db $ const (pure a)
  fn <*> a = 
    Db $ \conn -> do res <- runDb a conn
                     fn' <- runDb fn conn
                     pure $ fn' res

instance Monad Db where
  a >>= fn = 
    Db $ \conn -> do res <- runDb a conn
                     runDb (fn res) conn

-- Helper functions                             
executeDb :: MonadIO m => DbConfiguration -> Db a -> m a
executeDb (DbConfiguration path t) db = 
  liftIO $ withConnection path (\conn -> withTransaction conn (runDb (tracing t >> db) conn))

executeQuery_ :: FromRow a => Query -> Db [a]
executeQuery_ q = Db $ \conn -> query_ conn q

executeQuery :: (ToRow d, FromRow a) => Query -> d -> Db [a]
executeQuery q d = Db $ \conn -> query conn q d

executeCommand_ :: Query -> Db ()
executeCommand_ q = Db $ \conn -> execute_ conn q

executeCommand :: ToRow d => Query -> d -> Db ()
executeCommand q d = Db $ \conn -> execute conn q d

tracing :: DbTracingOptions -> Db ()
tracing NoTracing = Db $ \conn -> setTrace conn Nothing
tracing ConsoleTracing = Db $ \conn -> setTrace conn (Just print)

-- Application specific code
createTemplatesTable :: Db ()
createTemplatesTable =
  executeCommand_ [r| CREATE TABLE IF NOT EXISTS templates (
                  templateId INTEGER PRIMARY KEY
                 ,description TEXT
                 ,amount REAL
                 ,startDate TEXT
                 ,frequency TEXT
                 ,isDeleted INTEGER ) |]

createInstancesTable :: Db ()
createInstancesTable =
  executeCommand_ [r| CREATE TABLE IF NOT EXISTS instances (
                  originalTemplateId INTEGER
                 ,description TEXT
                 ,amount REAL
                 ,type TEXT
                 ,date TEXT
                 ,FOREIGN KEY(originalTemplateId) REFERENCES templates(templateId)) |]
                 

getLastRecordId :: Db Int64
getLastRecordId = Db lastInsertRowId

insertTemplate :: Template -> Db SavedTemplate
insertTemplate t@Template{..} = do
  executeCommand [r|  INSERT INTO templates (
                       description
                      ,amount
                      ,startDate
                      ,frequency
                      ,isDeleted)
                    VALUES(?, ?, ?, ?, ?) |]   
               (_templateDescription, _templateAmount, _templateStartDate, _templateFrequency, _templateIsDeleted) 
  templateId <- TemplateId <$> getLastRecordId 
  pure $ SavedTemplate (templateId, t)

updateTemplate :: TemplateId -> TemplateUpdateRequest -> Db ()
updateTemplate (TemplateId tId) (TemplateUpdateRequest (des, amt)) = 
  executeCommand [r| UPDATE templates
                   SET  description = ? 
                       ,amount = ? 
                   WHERE templateId = ? |] 
               (des, amt, tId)

deleteTemplate :: TemplateId -> Db ()
deleteTemplate (TemplateId tId) = 
  executeCommand [r| UPDATE templates
                     SET isDeleted = 1
                     WHERE templateId = ? |]
               (Only tId)

getAllTemplates :: Db [SavedTemplate]
getAllTemplates = do
  res <- executeQuery_ "SELECT * FROM templates WHERE isDeleted = 0" :: Db [Only TemplateId :. Template]
  pure $ (\(Only tId :. t) -> SavedTemplate (tId, t)) <$> res 
  
getTemplateById :: TemplateId -> Db (Maybe SavedTemplate)
getTemplateById (TemplateId tId) = do
  res <- executeQuery "SELECT * FROM templates WHERE templateId = ?" (Only tId) :: Db [Only TemplateId :. Template]
  case res of
    [Only tId' :. t] -> pure . Just $ SavedTemplate (tId', t)
    _ -> pure Nothing

getAllInstances :: Db [Instance]
getAllInstances = executeQuery_ "SELECT * FROM instances"

createInstance :: Instance -> Db ()
createInstance Instance{..} = 
  executeCommand [r| INSERT INTO instances (
                      originalTemplateId
                     ,description
                     ,amount
                     ,type
                     ,date)
                     VALUES (?, ?, ?, ?, ?) |]
                 (_instanceOriginalTemplateId, _instanceDescription, _instanceAmount, _instanceType, _instanceDate)                 

deleteInstance :: TemplateId -> Day -> Db ()
deleteInstance tId d = 
  executeCommand [r| DELETE FROM instances 
                     WHERE originalTemplateId = ? AND date = ? |] 
                 (tId, d)