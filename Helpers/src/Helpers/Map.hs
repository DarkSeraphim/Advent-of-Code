module Helpers.Map ((!?), showMap) where
import Data.Map (Map, findWithDefault, toList)
import Data.List (intercalate)
import Text.Printf (printf)

(!?) :: Ord k => Map k a -> (k, a) -> a
(!?) m (k, a) = findWithDefault a k m

infixl 9 !?

showMap :: (Show k, Show a) => Map k a -> String
showMap m = "fromList: \n" ++ intercalate "\n" (map (("\t" ++) . mapKV) kvs)
 where mapKV (k, v) = printf "%s => %s" (show k) (show v)
       kvs = toList m
