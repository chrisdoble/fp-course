{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- I think there's a problem with this exercise. The `Data.Set` import suggests
-- the solution should use sets, but `NoCaseString` doesn't have an `Ord`
-- instance which is required for most `Data.Set` functions. The solution below
-- is case sensitive, but so is the solution in github.com/tonymorris/fp-course.

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams name f =
  let findAnagrams w = filter (flip S.member w) $ permutations s
   in findAnagrams . S.fromList . hlist . lines <$> readFile f

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
