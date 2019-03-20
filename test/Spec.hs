
import Types
import Test.QuickCheck
import Data.Aeson
import qualified BusinessLogic as BL
import Data.Time.Calendar

prop_conversion :: (Eq a, ToJSON a, FromJSON a) => a -> Bool
prop_conversion a = decode (encode a) == Just a

prop_json_template_conversion :: Template -> Bool
prop_json_template_conversion = prop_conversion

prop_json_instance_conversion :: Instance -> Bool
prop_json_instance_conversion = prop_conversion

prop_instances_not_exceed_endDate :: Day -> [Instance] -> [SavedTemplate] -> Bool
prop_instances_not_exceed_endDate e ins s = 
  all (\i -> _instanceDate i <= e) $  BL.getInstances e ins s

main :: IO ()
main = do
  quickCheck prop_json_template_conversion
  quickCheck prop_json_instance_conversion
  quickCheck prop_instances_not_exceed_endDate