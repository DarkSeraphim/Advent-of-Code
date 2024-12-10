module Helpers.Map ((!?)) where
import Data.Map (Map, findWithDefault)
(!?) :: Ord k => Map k a -> (k, a) -> a
(!?) m (k, a) = findWithDefault a k m

infixl 9 !?
