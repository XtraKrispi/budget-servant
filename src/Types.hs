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

data InstanceType = Completed | Skipped

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
  deriving (Generic)

instance ToJSON TemplateId

instance FromField TemplateId where
  fromField = (TemplateId <$>) . fromField

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

newtype SavedTemplate = SavedTemplate (TemplateId, Template)
  deriving (Generic)

instance ToJSON SavedTemplate where
  toEncoding (SavedTemplate (tId, t)) =
    pairs ("templateId" .= tId <> "template" .= t)

data Instance = Instance { _instanceDescription :: Text
                         , _instanceAmount      :: Currency
                         , _instanceDate        :: Day }
  deriving (Eq, Show, Generic)

instance ToJSON Instance where
  toEncoding =
    genericToEncoding (defaultOptions { fieldLabelModifier =
                                          convertToCamelCase . drop 9 })

instance FromJSON Instance where
  parseJSON (Object v) =
    Instance <$> v .: "description"
             <*> v .: "amount"
             <*> v .: "date"
  parseJSON _ = mzero

instance Arbitrary Instance where
  arbitrary = Instance <$> arbitrary
                       <*> arbitrary
                       <*> arbitrary

data DbTracingOptions = NoTracing | ConsoleTracing

data DbConfiguration = DbConfiguration { _dbConfigurationPath :: FilePath
                                       , _dbConfigurationTracingOptions :: DbTracingOptions }

data Config = Config { _configDb :: DbConfiguration }

newtype AppT m a
   = AppT
   { unAppT :: ReaderT Config m a
   } deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO)

type App = AppT Handler

class TemplateQuery m where
  getTemplates :: m [SavedTemplate]

class TemplateCommand m where
  insert :: Template -> m TemplateId
  update :: TemplateId -> Template -> m ()
  delete :: TemplateId -> m ()

class InstanceQuery m where
  getInstances :: Day -> Day -> m [Instance]

class InstanceCommand m where
  updateInstance :: InstanceType -> Instance -> m ()
