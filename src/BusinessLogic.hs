{-# LANGUAGE RecordWildCards #-}
module BusinessLogic where

import qualified Data.List                     as L
import           Data.Time.Calendar
import           Types
import Capability

-- Application logic
getInstances :: InstanceQuery m 
             => TemplateQuery m 
             => Day 
             -> m [Instance]
getInstances e =
  extractInstances e <$> Capability.getInstances <*> Capability.getTemplates

-- Helpers
extractInstances :: Day -> [Instance] -> [SavedTemplate] -> [Instance]
extractInstances e dbInstances =
    L.sortBy (\i1 i2 -> compare (_instanceDate i1) (_instanceDate i2))
    . filter ((e >=) . _instanceDate)
    . (++) dbInstances
    . filter (`notElem` dbInstances)
    . concatMap (uncurry (extractInstances' e) . extractSavedTemplate)

extractInstances' :: Day -> TemplateId -> Template -> [Instance]
extractInstances' e tId Template {..} =
  Instance tId _templateDescription _templateAmount NotActioned
    <$> takeWhile (e >=) (getDates _templateFrequency _templateStartDate)

getDates :: Frequency -> Day -> [Day]
getDates OneTime  d = pure d
getDates BiWeekly d = d : getDates BiWeekly (addDays 14 d)
getDates Monthly d =
  uncurry addGregorianMonthsClip <$> zip [0, 1 ..] (repeat d)
