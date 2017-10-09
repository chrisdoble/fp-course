{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion ::
  List Chars
illion =
  let preillion ::
        List (Chars -> Chars)
      preillion =
        listh [
          const ""
        , const "un"
        , const "do"
        , const "tre"
        , const "quattuor"
        , const "quin"
        , const "sex"
        , const "septen"
        , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion ::
        List Chars
      postillion =
        listh [
          "vigintillion"
        , "trigintillion"
        , "quadragintillion"
        , "quinquagintillion"
        , "sexagintillion"
        , "septuagintillion"
        , "octogintillion"
        , "nonagintillion"
        , "centillion"
        , "decicentillion"
        , "viginticentillion"
        , "trigintacentillion"
        , "quadragintacentillion"
        , "quinquagintacentillion"
        , "sexagintacentillion"
        , "septuagintacentillion"
        , "octogintacentillion"
        , "nonagintacentillion"
        , "ducentillion"
        , "deciducentillion"
        , "vigintiducentillion"
        , "trigintaducentillion"
        , "quadragintaducentillion"
        , "quinquagintaducentillion"
        , "sexagintaducentillion"
        , "septuagintaducentillion"
        , "octogintaducentillion"
        , "nonagintaducentillion"
        , "trecentillion"
        , "decitrecentillion"
        , "vigintitrecentillion"
        , "trigintatrecentillion"
        , "quadragintatrecentillion"
        , "quinquagintatrecentillion"
        , "sexagintatrecentillion"
        , "septuagintatrecentillion"
        , "octogintatrecentillion"
        , "nonagintatrecentillion"
        , "quadringentillion"
        , "deciquadringentillion"
        , "vigintiquadringentillion"
        , "trigintaquadringentillion"
        , "quadragintaquadringentillion"
        , "quinquagintaquadringentillion"
        , "sexagintaquadringentillion"
        , "septuagintaquadringentillion"
        , "octogintaquadringentillion"
        , "nonagintaquadringentillion"
        , "quingentillion"
        , "deciquingentillion"
        , "vigintiquingentillion"
        , "trigintaquingentillion"
        , "quadragintaquingentillion"
        , "quinquagintaquingentillion"
        , "sexagintaquingentillion"
        , "septuagintaquingentillion"
        , "octogintaquingentillion"
        , "nonagintaquingentillion"
        , "sescentillion"
        , "decisescentillion"
        , "vigintisescentillion"
        , "trigintasescentillion"
        , "quadragintasescentillion"
        , "quinquagintasescentillion"
        , "sexagintasescentillion"
        , "septuagintasescentillion"
        , "octogintasescentillion"
        , "nonagintasescentillion"
        , "septingentillion"
        , "deciseptingentillion"
        , "vigintiseptingentillion"
        , "trigintaseptingentillion"
        , "quadragintaseptingentillion"
        , "quinquagintaseptingentillion"
        , "sexagintaseptingentillion"
        , "septuagintaseptingentillion"
        , "octogintaseptingentillion"
        , "nonagintaseptingentillion"
        , "octingentillion"
        , "decioctingentillion"
        , "vigintioctingentillion"
        , "trigintaoctingentillion"
        , "quadragintaoctingentillion"
        , "quinquagintaoctingentillion"
        , "sexagintaoctingentillion"
        , "septuagintaoctingentillion"
        , "octogintaoctingentillion"
        , "nonagintaoctingentillion"
        , "nongentillion"
        , "decinongentillion"
        , "vigintinongentillion"
        , "trigintanongentillion"
        , "quadragintanongentillion"
        , "quinquagintanongentillion"
        , "sexagintanongentillion"
        , "septuagintanongentillion"
        , "octogintanongentillion"
        , "nonagintanongentillion"
        ]
  in listh [
       ""
     , "thousand"
     , "million"
     , "billion"
     , "trillion"
     , "quadrillion"
     , "quintillion"
     , "sextillion"
     , "septillion"
     , "octillion"
     , "nonillion"
     , "decillion"
     , "undecillion"
     , "duodecillion"
     , "tredecillion"
     , "quattuordecillion"
     , "quindecillion"
     , "sexdecillion"
     , "septendecillion"
     , "octodecillion"
     , "novemdecillion"
     ] ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit =
  Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Enum, Bounded, Show)

showDigit ::
  Digit
  -> Chars
showDigit Zero =
  "zero"
showDigit One =
  "one"
showDigit Two =
  "two"
showDigit Three =
  "three"
showDigit Four =
  "four"
showDigit Five =
  "five"
showDigit Six =
  "six"
showDigit Seven =
  "seven"
showDigit Eight =
  "eight"
showDigit Nine =
  "nine"

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3 =
  D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving (Eq, Show)

-- Possibly convert a character to a digit.
fromChar ::
  Char
  -> Optional Digit
fromChar '0' =
  Full Zero
fromChar '1' =
  Full One
fromChar '2' =
  Full Two
fromChar '3' =
  Full Three
fromChar '4' =
  Full Four
fromChar '5' =
  Full Five
fromChar '6' =
  Full Six
fromChar '7' =
  Full Seven
fromChar '8' =
  Full Eight
fromChar '9' =
  Full Nine
fromChar _ =
  Empty

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
dollars ::
  Chars
  -> Chars
dollars s =
  let (d, c) = parsePrice s
   in showDollars d ++ " and " ++ showCents c

type Price = (List Digit3, Digit3)

-- | Parses a string into a Price value
--
-- >>> parsePrice ""
-- ([D1 Zero],D2 Zero Zero)
--
-- >>> parsePrice "."
-- ([D1 Zero],D2 Zero Zero)
--
-- >>> parsePrice "0."
-- ([D1 Zero],D2 Zero Zero)
--
-- >>> parsePrice ".0"
-- ([D1 Zero],D2 Zero Zero)
--
-- >>> parsePrice "1."
-- ([D1 One],D2 Zero Zero)
--
-- >>> parsePrice ".1"
-- ([D1 Zero],D2 One Zero)
--
-- >>> parsePrice "12"
-- ([D2 One Two],D2 Zero Zero)
--
-- >>> parsePrice ".12"
-- ([D1 Zero],D2 One Two)
--
-- >>> parsePrice "12.34"
-- ([D2 One Two],D2 Three Four)
--
-- >>> parsePrice "123"
-- ([D3 One Two Three],D2 Zero Zero)
--
-- >>> parsePrice "1234"
-- ([D1 One,D3 Two Three Four],D2 Zero Zero)
--
-- >>> parsePrice "1234567.1234"
-- ([D1 One,D3 Two Three Four,D3 Five Six Seven],D2 One Two)
parsePrice :: Chars -> Price
parsePrice cs = let (d, c) = split cs '.' in (parseDollars d, parseCents c)

-- | Splits a string in two at a specific delimeter
--
-- >>> split "" ','
-- ("","")
--
-- >>> split "a," ','
-- ("a","")
--
-- >>> split ",a" ','
-- ("","a")
--
-- >>> split "a,b" ','
-- ("a","b")
--
-- >>> split "a,b,c" ','
-- ("a","b,c")
split :: Chars -> Char -> (Chars, Chars)
split Nil _ = (Nil, Nil)
split (c :. s) d
  | c == d = (Nil, s)
  | otherwise = let (l, r) = split s d in (c :. l, r)

parseDollars :: Chars -> List Digit3
parseDollars cs = case parseDigits cs of
  Nil -> D1 Zero :. Nil
  digits -> (toDigit3 . reverse <$>) . reverse . group 3 . reverse $ digits

-- | Parses digits
--
-- >>> parseDigits ""
-- []
--
-- >>> parseDigits "1"
-- [One]
--
-- >>> parseDigits "a"
-- []
--
-- >>> parseDigits "1a"
-- [One]
--
-- >>> parseDigits "12"
-- [One,Two]
--
-- >>> parseDigits "a1b2c"
-- [One,Two]
parseDigits :: Chars -> List Digit
parseDigits cs = cs >>= \c -> case fromChar c of
  Empty -> Nil
  Full d -> d :. Nil

-- | Groups elements of a list into groups of a particular size
--
-- >>> group 0 (1 :. 2 :. 3 :. Nil)
-- []
--
-- >>> group 1 Nil
-- []
--
-- >>> group 1 (1 :. 2 :. 3 :. Nil)
-- [[1],[2],[3]]
--
-- >>> group 2 (1 :. 2 :. 3 :. Nil)
-- [[1,2],[3]]
--
-- >>> group 3 (1 :. 2 :. 3 :. Nil)
-- [[1,2,3]]
--
-- >>> group 4 (1 :. 2 :. 3 :. Nil)
-- [[1,2,3]]
group :: Int -> List a -> List (List a)
group _ Nil = Nil
group n xs
  | n == 0 = Nil
  | n > length xs = xs :. Nil
  | otherwise = (take n xs) :. group n (drop n xs)

toDigit3 :: List Digit -> Digit3
toDigit3 (x :. Nil) = D1 x
toDigit3 (x :. y :. Nil) = D2 x y
toDigit3 (x :. y :. z :. Nil) = D3 x y z
toDigit3 _ = error "can't convert to Digit3"

parseCents :: Chars -> Digit3
parseCents cs = toDigit3 . take 2 . (++ Zero :. Zero :. Nil) . parseDigits $ cs

-- | Shows a dollar value
--
-- >>> showDollars $ D1 One :. Nil
-- "one dollar"
--
-- >>> showDollars $ D1 Two :. Nil
-- "two dollars"
--
-- >>> showDollars $ D2 One Zero :. Nil
-- "ten dollars"
--
-- >>> showDollars $ D2 Four Two :. Nil
-- "forty-two dollars"
--
-- >>> showDollars $ D3 One Zero Zero :. Nil
-- "one hundred dollars"
--
-- >>> showDollars $ D3 One Four Two :. Nil
-- "one hundred and forty-two dollars"
--
-- >>> showDollars $ D1 One :. D3 Zero Zero Zero :. Nil
-- "one thousand dollars"
--
-- >>> showDollars $ D1 One :. D3 Zero Zero One :. Nil
-- "one thousand one dollars"
--
-- >>> showDollars $ D1 One :. D3 One One One :. Nil
-- "one thousand one hundred and eleven dollars"
--
-- >>> showDollars $ D1 One :. D3 One One One :. D3 One One One :. Nil
-- "one million one hundred and eleven thousand one hundred and eleven dollars"
showDollars :: List Digit3 -> Chars
showDollars (D1 One :. Nil) = "one dollar"
showDollars s = foldRight f "dollars" . reverse . zip illion . reverse $ s where
  f (_, D3 Zero Zero Zero) d = d
  f ("", s) d = showDigit3 s ++ " " ++ d
  f (i, s) d = showDigit3 s ++ " " ++ i ++ " " ++ d

-- | Returns a human-readable representation of a Digit3
--
-- >>> showDigit3 $ D1 Zero
-- "zero"
--
-- >>> showDigit3 $ D1 One
-- "one"
--
-- >>> showDigit3 $ D2 Zero Zero
-- "zero"
--
-- >>> showDigit3 $ D2 Zero One
-- "one"
--
-- >>> showDigit3 $ D2 One Zero
-- "ten"
--
-- >>> showDigit3 $ D2 One Five
-- "fifteen"
--
-- >>> showDigit3 $ D2 Two Zero
-- "twenty"
--
-- >>> showDigit3 $ D2 Two Five
-- "twenty-five"
--
-- >>> showDigit3 $ D3 Zero Zero Zero
-- "zero"
--
-- >>> showDigit3 $ D3 Zero One Two
-- "twelve"
--
-- >>> showDigit3 $ D3 One Zero Zero
-- "one hundred"
--
-- >>> showDigit3 $ D3 One Zero One
-- "one hundred and one"
--
-- >>> showDigit3 $ D3 One Two Zero
-- "one hundred and twenty"
--
-- >>> showDigit3 $ D3 Nine Nine Nine
-- "nine hundred and ninety-nine"
showDigit3 :: Digit3 -> Chars
showDigit3 (D1 o) = showDigit o
showDigit3 d@(D2 t o) = case d of
  D2 Zero _ -> showDigit3 (D1 o)
  D2 One Zero -> "ten"
  D2 One One -> "eleven"
  D2 One Two -> "twelve"
  D2 One Three -> "thirteen"
  D2 One Four -> "fourteen"
  D2 One Five -> "fifteen"
  D2 One Six -> "sixteen"
  D2 One Seven -> "seventeen"
  D2 One Eight -> "eighteen"
  D2 One Nine -> "nineteen"
  D2 Two Zero -> "twenty"
  D2 Three Zero -> "thirty"
  D2 Four Zero -> "forty"
  D2 Five Zero -> "fifty"
  D2 Six Zero -> "sixty"
  D2 Seven Zero -> "seventy"
  D2 Eight Zero -> "eighty"
  D2 Nine Zero -> "ninety"
  _ -> showDigit3 (D2 t Zero) ++ "-" ++ showDigit3 (D1 o)
showDigit3 d@(D3 h t o) = case d of
  D3 Zero _ _ -> showDigit3 (D2 t o)
  D3 _ Zero Zero -> showDigit h ++ " hundred"
  _ -> showDigit3 (D3 h Zero Zero) ++ " and " ++ showDigit3 (D2 t o)

-- | Shows a cent value
--
-- >>> showCents $ D2 Zero One
-- "one cent"
--
-- >>> showCents $ D2 Zero Two
-- "two cents"
--
-- >>> showCents $ D2 One Zero
-- "ten cents"
--
-- >>> showCents $ D2 Four Two
-- "forty-two cents"
showCents :: Digit3 -> Chars
showCents (D2 Zero One) = "one cent"
showCents d@(D2 _ _) = showDigit3 d ++ " cents"
showCents d = error "unsupported Digit3"
