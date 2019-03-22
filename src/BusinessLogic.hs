{-# LANGUAGE RecordWildCards #-}
module BusinessLogic where

import qualified Data.List                     as L
import           Data.Time.Calendar
import           Types

getInstances :: Day -> [Instance] -> [SavedTemplate] -> [Instance]
getInstances e dbInstances =
    L.sortBy (\i1 i2 -> compare (_instanceDate i1) (_instanceDate i2))
    . (++) dbInstances
    . filter (`notElem` dbInstances)
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
