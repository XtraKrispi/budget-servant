{-# LANGUAGE RecordWildCards #-}
module BusinessLogic where

import           Types
import           Data.Time.Calendar
import qualified Data.List                     as L

getInstances :: Day -> [Instance] -> [SavedTemplate] -> [Instance]
getInstances e = convert  
  where
    convert dbInstances =
          filter (`notElem` dbInstances)
        . L.sortBy (\i1 i2 -> compare (_instanceDate i1) (_instanceDate i2))
        . concatMap (uncurry (getInstances' e) . extractSavedTemplate)


getInstances' :: Day -> TemplateId -> Template -> [Instance]
getInstances' e tId Template {..} =
  Instance tId _templateDescription _templateAmount NotActioned
    <$> takeWhile (e >=) (getDates _templateFrequency _templateStartDate)

getDates :: Frequency -> Day -> [Day]
getDates OneTime  d = pure d
getDates BiWeekly d = d : getDates BiWeekly (addDays 14 d)
getDates Monthly d =
  uncurry addGregorianMonthsClip <$> zip [0, 1 ..] (repeat d)
