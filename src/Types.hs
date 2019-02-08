{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import           Data.Text                      ( Text )
import           Data.Time.Calendar
import           Data.Aeson
import           GHC.Generics
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Time ( )
import           Test.QuickCheck.Instances.Text ( )
import           Control.Monad
import           Utils
import           Control.Monad.Reader
import           Servant
import           Database.SQLite.Simple
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.FromField
import           Data.Int
import           Data.Scientific

data Frequency = OneTime | BiWeekly | Monthly
  deriving (Eq, Show, Generic, Bounded, Enum, Read)

instance ToJSON Frequency
instance FromJSON Frequency

instance ToField Frequency where
  toField = toField . show

instance FromField Frequency where
  fromField = (read <$>) . fromField

instance Arbitrary Frequency where
  arbitrary = arbitraryBoundedEnum

data InstanceType = NotActioned | Completed | Skipped
  deriving (Show, Eq, Generic, Bounded, Enum, Read)

instance ToJSON InstanceType
instance FromJSON InstanceType

instance Arbitrary InstanceType where
  arbitrary = arbitraryBoundedEnum

instance ToField InstanceType where
  toField i = toField (show i)

instance FromField InstanceType where
  fromField = (read <$>) . fromField

newtype Currency = Currency { unCurrency :: Double }
  deriving (Eq, Show, Generic)

instance ToJSON Currency where
  toJSON (Currency amt) = toJSON amt

instance FromJSON Currency where
  parseJSON (Number n) = pure $ Currency (toRealFloat n)
  parseJSON _ = mzero

instance ToField Currency where
  toField (Currency d) = toField d

instance FromField Currency where
  fromField = (Currency <$>) . fromField

instance Arbitrary Currency where
  arbitrary = Currency <$> arbitrary

newtype TemplateId = TemplateId Int64
  deriving (Eq, Show, Generic)

instance ToJSON TemplateId

instance FromJSON TemplateId where
  parseJSON (Number n) = maybe mzero (pure . TemplateId) $ toBoundedInteger n
  parseJSON _ = mzero

instance FromField TemplateId where
  fromField = (TemplateId <$>) . fromField

instance ToField TemplateId where
  toField (TemplateId tId) = toField tId

instance Arbitrary TemplateId where
  arbitrary = TemplateId <$> arbitrary

data Template = Template { _templateDescription :: Text
                         , _templateAmount      :: Currency
                         , _templateStartDate   :: Day
                         , _templateFrequency   :: Frequency
                         , _templateIsDeleted   :: Bool }
  deriving (Eq, Show, Generic)

instance ToJSON Template where
  toEncoding =
    genericToEncoding (defaultOptions { fieldLabelModifier = convertToCamelCase . drop 9 })

instance FromJSON Template where
  parseJSON (Object v) =
    Template <$> v .: "description"
             <*> v .: "amount"
             <*> v .: "startDate"
             <*> v .: "frequency"
             <*> v .:? "isDeleted" .!= False
  parseJSON _ = mzero

instance FromRow Template where
  fromRow = Template <$> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field

instance ToRow Template where
  toRow Template{..} =
    toRow (_templateDescription, _templateAmount, _templateStartDate, _templateFrequency)

instance Arbitrary Template where
  arbitrary = Template <$> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary

newtype SavedTemplate = SavedTemplate { extractSavedTemplate :: (TemplateId, Template) }
  deriving (Generic)

instance ToJSON SavedTemplate where
  toEncoding (SavedTemplate (tId, t)) =
    pairs ("templateId" .= tId <> "template" .= t)

data Instance = Instance { _instanceOriginalTemplateId :: TemplateId
                         , _instanceDescription        :: Text
                         , _instanceAmount             :: Currency
                         , _instanceType               :: InstanceType
                         , _instanceDate               :: Day }
  deriving (Show, Generic)

instance Eq Instance where
  i1 == i2 = (_instanceOriginalTemplateId i1 == _instanceOriginalTemplateId i2) && (_instanceDate i1 == _instanceDate i2) 

instance ToJSON Instance where
  toEncoding =
    genericToEncoding (defaultOptions { fieldLabelModifier =
                                          convertToCamelCase . drop 9 })

instance FromJSON Instance where
  parseJSON (Object v) =
    Instance <$> v .: "originalTemplateId"
             <*> v .: "description"
             <*> v .: "amount"
             <*> v .: "type"
             <*> v .: "date"
             
  parseJSON _ = mzero

instance Arbitrary Instance where
  arbitrary = Instance <$> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary

instance FromRow Instance where
  fromRow = Instance <$> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field

instance ToRow Instance where
  toRow Instance{..} =
    toRow (_instanceOriginalTemplateId, _instanceDescription,  _instanceAmount, _instanceDate, _instanceType) 

data DbTracingOptions = NoTracing | ConsoleTracing

data DbConfiguration = DbConfiguration { _dbConfigurationPath :: FilePath
                                       , _dbConfigurationTracingOptions :: DbTracingOptions }

data Config = Config { _configDb :: DbConfiguration }

newtype AppT m a
   = AppT
   { unAppT :: ReaderT Config m a
   } deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO)

type App = AppT Handler

newtype TemplateUpdateRequest = 
  TemplateUpdateRequest { unTemplateUpdateRequest :: (Text, Currency) }
  deriving (Generic)

instance FromJSON TemplateUpdateRequest where
  parseJSON (Object v) =
       curry TemplateUpdateRequest <$> v .: "description"
                                   <*> v .: "amount"
  parseJSON _ = mzero

class TemplateQuery m where
  getTemplates :: m [SavedTemplate]

class TemplateCommand m where
  insert :: Template -> m TemplateId
  update :: TemplateId -> TemplateUpdateRequest -> m ()
  delete :: TemplateId -> m ()

class InstanceQuery m where
  getInstances :: m [Instance]

class InstanceCommand m where
  updateInstance :: InstanceType -> Instance -> m ()
