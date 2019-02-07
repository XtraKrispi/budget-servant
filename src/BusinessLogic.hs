{-# LANGUAGE RecordWildCards #-}
module BusinessLogic where

import           Types
import           Data.Time.Calendar

getTemplates :: TemplateQuery m => m [SavedTemplate]
getTemplates = Types.getTemplates

insertTemplate :: TemplateCommand m => Template -> m TemplateId
insertTemplate = insert

updateTemplate :: TemplateCommand m => TemplateId -> Template -> m ()
updateTemplate = update

deleteTemplate :: TemplateCommand m => TemplateId -> m ()
deleteTemplate = delete

getInstances :: Template -> [Instance]
getInstances Template {..} =
  Instance _templateDescription _templateAmount
    <$> getDates _templateFrequency _templateStartDate

getDates :: Frequency -> Day -> [Day]
getDates OneTime  d = pure d
getDates BiWeekly d = d : getDates BiWeekly (addDays 14 d)
getDates Monthly  d = 
  uncurry addGregorianMonthsClip <$> zip [0,1..] (repeat d)
