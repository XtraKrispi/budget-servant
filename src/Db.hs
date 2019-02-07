{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Db where

import Database.SQLite.Simple
import Types
import Control.Monad.IO.Class
import Data.Int
import Text.RawString.QQ

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
executeDb :: MonadIO m => Db a -> DbConfiguration -> m a
executeDb db (DbConfiguration path t) = liftIO $ withConnection path $ runDb (tracing t >> db)

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

getLastRecordId :: Db Int64
getLastRecordId = Db lastInsertRowId

insertTemplate :: Template -> Db ()
insertTemplate Template{..} = 
  executeCommand [r|  INSERT INTO templates (
                       description
                      ,amount
                      ,startDate
                      ,frequency
                      ,isDeleted)
                    VALUES(?, ?, ?, ?, ?) |]   
               (_templateDescription, _templateAmount, _templateStartDate, _templateFrequency, _templateIsDeleted)  

updateTemplate :: TemplateId -> Template -> Db ()
updateTemplate (TemplateId tId) Template{..} = 
  executeCommand [r| UPDATE templates
                   SET  description = ? 
                       ,amount = ? 
                       ,startDate = ?
                       ,frequency = ?
                       ,isDeleted = ?
                   WHERE templateId = ? |] 
               (_templateDescription, _templateAmount, _templateStartDate, _templateFrequency, _templateIsDeleted, tId)

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
  
instance TemplateQuery Db where
  getTemplates = getAllTemplates

instance TemplateCommand Db where
  insert t = insertTemplate t >> TemplateId <$> getLastRecordId
  update = updateTemplate
  delete = deleteTemplate