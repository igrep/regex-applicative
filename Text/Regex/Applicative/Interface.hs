{-# LANGUAGE TypeFamilies, GADTs, TupleSections #-}
{-# LANGUAGE DataKinds, FlexibleInstances, TypeOperators, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Regex.Applicative.Interface where

import Control.Applicative hiding (empty)
import qualified Control.Applicative
import Data.Extensible (type (>:), FieldName, Record)
import Data.Functor.Indexed
import Data.String
import Text.Regex.Applicative.Reference
import Text.Regex.Applicative.Types

instance Functor (RE s xs xs) where
    fmap f x = Fmap f x
    f <$ x = pure f <* x

instance Applicative (RE s xs xs) where
    pure x = const x <$> Eps
    a1 <*> a2 = App a1 a2
    a *> b = pure (const id) <*> Void a <*> b
    a <* b = pure const <*> a <*> Void b

instance Alternative (RE s xs xs) where
    a1 <|> a2 = Alt a1 a2
    empty = Fail
    many a = reverse <$> Rep Greedy (flip (:)) [] a
    some a = (:) <$> a <*> many a

instance IxFunctor (RE s) where
    imap = Fmap

instance IxPointed (RE s) where
    ireturn = pure

instance IxZero (RE s) where
    izero = Fail

instance IxPlus (RE s) where
    iplus = Alt

instance IxApplicative (RE s) where
    iap = App

instance (char ~ Char, string ~ String) => IsString (RE char xs xs string) where
    fromString = string

-- | 'RE' is a profunctor. This is its contravariant map.
--
-- (A dependency on the @profunctors@ package doesn't seem justified.)
comap :: (s2 -> s1) -> RE s1 xs ys a -> RE s2 xs ys a
comap f re =
  case re of
    Eps -> Eps
    Symbol t p    -> Symbol t (p . f)
    Alt r1 r2     -> Alt (comap f r1) (comap f r2)
    App r1 r2     -> App (comap f r1) (comap f r2)
    Capture k rx  -> Capture k (comap f rx)
    Fmap g r      -> Fmap g (comap f r)
    Fail          -> Fail
    Refer g       -> Refer g
    Rep gr fn a r -> Rep gr fn a (comap f r)
    Void r        -> Void (comap f r)

-- | Match and return a single symbol which satisfies the predicate
psym :: (s -> Bool) -> RE s xs xs s
psym p = msym (\s -> if p s then Just s else Nothing)

-- | Like 'psym', but allows to return a computed value instead of the
-- original symbol
msym :: (s -> Maybe a) -> RE s xs xs a
msym p = Symbol (error "Not numbered symbol") p

-- | Match and return the given symbol
sym :: Eq s => s -> RE s xs xs s
sym s = psym (s ==)

-- | Match and return any single symbol
anySym :: RE s xs xs s
anySym = msym Just

-- | Match and return the given sequence of symbols.
--
-- Note that there is an 'IsString' instance for regular expression, so
-- if you enable the @OverloadedStrings@ language extension, you can write
-- @string \"foo\"@ simply as @\"foo\"@.
--
-- Example:
--
-- >{-# LANGUAGE OverloadedStrings #-}
-- >import Text.Regex.Applicative
-- >
-- >number = "one" *> pure 1  <|>  "two" *> pure 2
-- >
-- >main = print $ "two" =~ number
string :: Eq a => [a] -> RE a xs xs [a]
string = traverse sym

-- | Match zero or more instances of the given expression, which are combined using
-- the given folding function.
--
-- 'Greediness' argument controls whether this regular expression should match
-- as many as possible ('Greedy') or as few as possible ('NonGreedy') instances
-- of the underlying expression.
reFoldl :: Greediness -> (b -> a -> b) -> b -> RE s xs xs a -> RE s xs xs b
reFoldl g f b a = Rep g f b a

-- | Match zero or more instances of the given expression, but as
-- few of them as possible (i.e. /non-greedily/). A greedy equivalent of 'few'
-- is 'many'.
--
-- Examples:
--
-- >Text.Regex.Applicative> findFirstPrefix (few anySym  <* "b") "ababab"
-- >Just ("a","abab")
-- >Text.Regex.Applicative> findFirstPrefix (many anySym  <* "b") "ababab"
-- >Just ("ababa","")
few :: RE s xs xs a -> RE s xs xs [a]
few a = reverse <$> Rep NonGreedy (flip (:)) [] a

capture :: FieldName k -> RE s xs xs v -> RE s xs ((k >: v) : xs) ()
capture = Capture

refer :: Getting v (Record xs) v -> RE s xs xs v
refer = Refer

-- | @s =~ a = match a s@
(=~) :: [s] -> RE s '[] ys a -> Maybe a
(=~) = flip match
infix 2 =~

-- | Attempt to match a string of symbols against the regular expression.
-- Note that the whole string (not just some part of it) should be matched.
--
-- Examples:
--
-- >Text.Regex.Applicative> match (sym 'a' <|> sym 'b') "a"
-- >Just 'a'
-- >Text.Regex.Applicative> match (sym 'a' <|> sym 'b') "ab"
-- >Nothing
--
match :: RE s '[] ys a -> [s] -> Maybe a
match re = fmap fst . referenceIx re
