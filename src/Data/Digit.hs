{- HLINT ignore unsafeDigits -}
{-# LANGUAGE TemplateHaskell #-}

module Data.Digit
  ( Digit(..)
  , NormalDigits(..)
  , _NormalDigits
  , digitToChar
  , charToDigit
  , digitToNum
  , digitsToNum
  , poly10
  , digitFromNumMod10
  , natToDigits
  , _Digit
  , _Digits
  , _DigitsText
  , _DigitsInt
  , _DigitsNat
  , digitRegex
  , digitRegex'
  , unsafeDigits
  ) where

import Control.Lens
import Data.Foldable as F
import Data.List as L
import Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text as T
import Data.Text.Lens
import GHC.Generics (Generic)
import Numeric.Natural
import Test.QuickCheck
import Text.Regex.Applicative as R


-- | Decimal digit.
data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance Arbitrary Digit where
  arbitrary = arbitraryBoundedEnum

-- | Normal form of digits, that doesn't have leading zeroes
newtype NormalDigits = NormalDigits
  { unDigits :: NonEmpty Digit
  } deriving (Eq, Ord, Show)

makePrisms ''NormalDigits

instance Arbitrary NormalDigits where
  arbitrary = do
    norm <- L.dropWhile (== D0) <$> arbitrary
    normNe <- case norm of
      []   -> pure <$> arbitrary
      x:xs -> pure $ x :| xs
    return $ NormalDigits normNe

digitToChar :: Digit -> Char
digitToChar = \case
  D0 -> '0'
  D1 -> '1'
  D2 -> '2'
  D3 -> '3'
  D4 -> '4'
  D5 -> '5'
  D6 -> '6'
  D7 -> '7'
  D8 -> '8'
  D9 -> '9'

charToDigit :: Char -> Maybe Digit
charToDigit = \case
  '0' -> Just D0
  '1' -> Just D1
  '2' -> Just D2
  '3' -> Just D3
  '4' -> Just D4
  '5' -> Just D5
  '6' -> Just D6
  '7' -> Just D7
  '8' -> Just D8
  '9' -> Just D9
  _   -> Nothing

digitToNum :: Num n => Digit -> n
digitToNum = fromIntegral . fromEnum

digitsToNum :: (Functor f, Foldable f, Num n) => f Digit -> n
digitsToNum = poly10 . fmap digitToNum

poly10 :: (Num n, Foldable f) => f n -> n
poly10 = F.foldl' go 0
  where
    go acc n = (acc * 10) + n

digitFromNumMod10 :: Integral n => n -> Digit
digitFromNumMod10 = toEnum . fromIntegral . (`mod` 10)

-- | Convert natural number to non empty list of digits.
natToDigits :: Natural -> NormalDigits
natToDigits = NormalDigits . NE.reverse . natToDigitsRev

natToDigitsRev :: Natural -> NE.NonEmpty Digit
natToDigitsRev n = if d==0 then pure digit else digit `NE.cons` natToDigitsRev d
  where
    (d, m) = n `divMod` 10
    digit  = toEnum (fromIntegral m)

_Digit :: Prism' Char Digit
_Digit = prism' digitToChar charToDigit

_Digits :: Traversable t => Prism' (t Char) (t Digit)
_Digits = below _Digit

_DigitsText :: Prism' Text (NE.NonEmpty Digit)
_DigitsText = from packed . _Digits . _NonEmptyList
  where _NonEmptyList = prism' NE.toList NE.nonEmpty

_DigitsInt :: Integral n => Prism' n NormalDigits
_DigitsInt = prism' (digitsToNum . unDigits) positiveIntToDigits
  where
    positiveIntToDigits n = if n < 0
      then Nothing
      else Just $ natToDigits (fromInteger $ fromIntegral n)

_DigitsNat :: Iso' Natural NormalDigits
_DigitsNat = iso natToDigits (digitsToNum . unDigits)

digitRegex :: RE Char Digit
digitRegex = R.msym charToDigit

digitRegex' :: RE Char Char
digitRegex' = digitToChar <$> digitRegex

unsafeDigits :: String -> [Digit]
unsafeDigits = fmap (fromJust . charToDigit)
