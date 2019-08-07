{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE DataKinds, OverloadedLabels #-}

import Text.Regex.Applicative
import Text.Regex.Applicative.Reference
import qualified Text.Regex.Applicative.Original as Orig
import Control.Applicative
import Control.Monad
import Data.Functor (($>))
import Data.Traversable
import Data.Maybe
import Text.Printf

import Test.SmallCheck
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.SmallCheck
import Test.Tasty.HUnit

-- Small alphabets as SmallCheck's series
newtype A = A { a :: Char } deriving Show
instance Monad m => Serial m A where
    series = cons0 $ A 'a'

newtype AB = AB { ab :: Char } deriving Show
instance Monad m => Serial m AB where
    series = cons0 (AB 'a') \/ cons0 (AB 'b')

newtype ABC = ABC { abc :: Char } deriving Show
instance Monad m => Serial m ABC where
    series = cons0 (ABC 'a') \/ cons0 (ABC 'b') \/ cons0 (ABC 'c')

re1 :: RE Char '[] '[] (Int, Int)
re1 =
    let one = 1 <$ sym 'a'
        two = 2 <$ sym 'a' <* sym 'a'
    in (,) <$> (one <|> two) <*> (two <|> one)

re2 :: RE Char '[] '[] [Int]
re2 = sequenceA
    [ 1 <$ sym 'a' <* sym 'a' <|>
      2 <$ sym 'a'
    , 3 <$ sym 'b'
    , 4 <$ sym 'b' <|>
      5 <$ sym 'a' ]

re3 :: RE Char '[] '[] [Int]
re3 = sequenceA
    [ pure 0 <|> pure 1
    ,  1 <$ sym 'a' <* sym 'a' <|>
       2 <$ sym 'a'
    ,  3 <$ sym 'b' <|> pure 6
    , fmap (+1) $
      4 <$ sym 'b' <|>
      pure 7 <|>
      5 <$ sym 'a' ]

re4 :: RE Char '[] '[] String
re4 = sym 'a' *> many (sym 'b') <* sym 'a'

re5 :: RE Char '[] '[] String
re5 = (sym 'a' <|> sym 'a' *> sym 'a') *> many (sym 'a')

re6 :: RE Char '[] '[] [Int]
re6 = many (3 <$ sym 'a' <* sym 'a' <* sym 'a' <|> 1 <$ sym 'a')

-- Regular expression from the weighted regexp paper.
re7 :: RE Char '[] '[] ([(String, Char, String, Char)], String)
re7 =
    let many_A_or_B = many (sym 'a' <|> sym 'b')
    in (,) <$>
        many ((,,,) <$> many_A_or_B <*> sym 'c' <*> many_A_or_B <*> sym 'c') <*>
        many_A_or_B

re8 :: RE Char '[] '[] (String, String)
re8 = (,) <$> many (sym 'a' <|> sym 'b') <*> many (sym 'b' <|> sym 'c')

-- NB: we don't test these against the reference impl, 'cause it will loop!
re9, re10 :: RE Char '[] '[] String
re9 = many (sym 'a' <|> empty) <* sym 'b'
re10 = few (sym 'a' <|> empty) <* sym 'b'

prop :: (Eq a, Eq s) => RE s '[] '[] a -> (b -> s) -> [b] -> Bool
prop re f s =
    let fs = map f s in
    reference re fs == (fs =~ re)

-- Because we have 2 slightly different algorithms for recognition and parsing,
-- we test that they agree
testRecognitionAgainstParsing :: Eq s => RE s '[] '[] a -> (b -> s) -> [b] -> Bool
testRecognitionAgainstParsing re f s =
    let fs = map f s in
    isJust (fs =~ re) == isJust (fs =~ (re $> ()))

tests :: TestTree
tests = testGroup "Tests"
    [ testGroup "Engine tests"
       [ t "re1" 10 $ prop re1 a
       , t "re2" 10 $ prop re2 ab
       , t "re3" 10 $ prop re3 ab
       , t "re4" 10 $ prop re4 ab
       , t "re5" 10 $ prop re5 a
       , t "re6" 10 $ prop re6 a
       , t "re7"  7 $ prop re7 abc
       , t "re8"  7 $ prop re8 abc
       ]
    , testGroup "Recognition vs parsing"
       [ t "re1" 10 $ testRecognitionAgainstParsing re1 a
       , t "re2" 10 $ testRecognitionAgainstParsing re2 ab
       , t "re3" 10 $ testRecognitionAgainstParsing re3 ab
       , t "re4" 10 $ testRecognitionAgainstParsing re4 ab
       , t "re5" 10 $ testRecognitionAgainstParsing re5 a
       , t "re6" 10 $ testRecognitionAgainstParsing re6 a
       , t "re7"  7 $ testRecognitionAgainstParsing re7 abc
       , t "re8"  7 $ testRecognitionAgainstParsing re8 abc
       , t "re8" 10 $ testRecognitionAgainstParsing re9 ab
       , t "re8" 10 $ testRecognitionAgainstParsing re10 ab
       ]
    , testGroup "Group capturing feature"
        [ testCase "fixed-length palindrome without group capturing" $ do
            let matchWithPalindrome8 s = do
                    let re = sequenceA $ replicate 4 Orig.anySym
                    (matched, left) <- Orig.findFirstPrefix re s
                    guard $ reverse matched == left

            assertEqual "Matches with a palindrome" (matchWithPalindrome8 "abcddcba") (Just ())
            assertEqual "Doesn't match with a non-palindrome" (matchWithPalindrome8 "abcddcab") Nothing

        , testCase "fixed-length palindrome with group capturing" $ do
            let re =
                    -- Never care about that this version is longer than the original...
                    -- Commenting out some of the `captureSym`s, the corresponding `refer` fails to type-check.
                        captureSym #firstHalf1 anySym
                    *>> captureSym #firstHalf2 anySym
                    *>> captureSym #firstHalf3 anySym
                    *>> captureSym #firstHalf4 anySym
                    *>> refer #firstHalf4
                    *>> refer #firstHalf3
                    *>> refer #firstHalf2
                    *>> refer #firstHalf1
                    *>> ireturn ()
            assertEqual "Matches with a palindrome" ("abcddcba" =~ re) (Just ())
            assertEqual "Doesn't match with a non-palindrome" ("abcddcab" =~ re) Nothing
        ]
    ]
    where
    t name n = localOption (SmallCheckDepth n) . testProperty name
    -- u name real ideal = testCase name (assertEqual "" real ideal)

main :: IO ()
main = defaultMain tests
