module DigitSpec (spec) where

import Data.Digit
import Control.Lens
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()


spec :: Spec
spec = describe "Digit" $ do
  specify "_Digit" $ property (prop_Prism _Digit)
  specify "_Digits" $ property (prop_Prism (_Digits @[]))
  specify "_DigitsText" $ property (prop_Prism _DigitsText)
  specify "_DigitsInt" $ property (prop_Prism (_DigitsInt @Integer))
  specify "_DigitsNat" $ property (prop_Iso _DigitsNat)

prop_Prism :: (Eq a, Eq s) => Prism' s a -> s -> a -> Bool
prop_Prism p s a = prop_PrismTo p s && prop_PrismRe p a

prop_PrismRe :: Eq a => Prism' s a -> a -> Bool
prop_PrismRe p a = preview (re p . p) a == Just a

prop_PrismTo :: Eq s => Prism' s a -> s -> Bool
prop_PrismTo p s = maybe True (== s) (s ^? p . re p)

prop_Iso :: (Eq a, Eq s) => Iso' s a -> s -> a -> Bool
prop_Iso i s a = prop_IsoTo i s && prop_IsoRe i a

prop_IsoRe :: Eq a => Iso' s a -> a -> Bool
prop_IsoRe i a = view (re i . i) a == a

prop_IsoTo :: Eq s => Iso' s a -> s -> Bool
prop_IsoTo i s = view (i . re i) s == s
