{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-do-lambda-eta-expansion -fno-warn-unused-imports #-}
module Text.Regex.Applicative.Types where

import Control.Applicative
-- The above import is needed for haddock to properly generate links to
-- Applicative methods. But it's not actually used in the code, hence
-- -fno-warn-unused-imports.

import Data.Extensible
import Data.Kind (Type)
import qualified GHC.TypeLits as TL


newtype ThreadId = ThreadId Int

-- | A thread either is a result or corresponds to a symbol in the regular
-- expression, which is expected by that thread.
data Thread s i r
    = Thread
        { threadId_ :: ThreadId
        , _threadCont :: s -> i -> [Thread s i r]
        }
    | Accept r i

-- | Returns thread identifier. This will be 'Just' for ordinary threads and
-- 'Nothing' for results.
threadId :: Thread s i r -> Maybe ThreadId
threadId Thread { threadId_ = i } = Just i
threadId _ = Nothing

data Greediness = Greedy | NonGreedy
    deriving (Show, Read, Eq, Ord, Enum)

-- | Type of regular expressions that recognize symbols of type @s@ and
-- produce a result of type @a@.
--
-- Regular expressions can be built using 'Functor', 'Applicative' and
-- 'Alternative' instances in the following natural way:
--
-- * @f@ '<$>' @ra@ matches iff @ra@ matches, and its return value is the result
-- of applying @f@ to the return value of @ra@.
--
-- * 'pure' @x@ matches the empty string (i.e. it does not consume any symbols),
-- and its return value is @x@
--
-- * @rf@ '<*>' @ra@ matches a string iff it is a concatenation of two
-- strings: one matched by @rf@ and the other matched by @ra@. The return value
-- is @f a@, where @f@ and @a@ are the return values of @rf@ and @ra@
-- respectively.
--
-- * @ra@ '<|>' @rb@ matches a string which is accepted by either @ra@ or @rb@.
-- It is left-biased, so if both can match, the result of @ra@ is used.
--
-- * 'empty' is a regular expression which does not match any string.
--
-- * 'many' @ra@ matches concatenation of zero or more strings matched by @ra@
-- and returns the list of @ra@'s return values on those strings.
--
-- * 'some' @ra@ matches concatenation of one or more strings matched by @ra@
-- and returns the list of @ra@'s return values on those strings.
data RE s (xs :: [Assoc TL.Symbol Type]) (ys :: [Assoc TL.Symbol Type]) a where
    Eps :: RE s xs xs ()
    Symbol :: ThreadId -> (s -> Maybe a) -> RE s xs xs a
    Alt :: RE s xs ys a -> RE s xs ys a -> RE s xs ys a
    App :: RE s xs ys (a -> b) -> RE s ys zs a -> RE s xs zs b
    Capture :: FieldName k -> RE s xs xs [s] -> RE s xs (k >: [s] ': xs) [s]
    Fmap :: (a -> b) -> RE s xs ys a -> RE s xs ys b
    Fail :: RE s xs ys a
    Refer :: Getting [s] (Record xs) [s] -> RE s xs xs [s]
    Rep :: Greediness    -- repetition may be greedy or not
        -> (b -> a -> b) -- folding function (like in foldl)
        -> b             -- the value for zero matches, and also the initial value
                         -- for the folding function
        -> RE s xs xs a
        -> RE s xs xs b
    Void :: RE s xs ys a -> RE s xs ys ()

-- | Copied from the lens package
type Getting r s a = (a -> Const r a) -> s -> Const r s

-- | Copied from the lens package
(^.) :: s -> Getting a s a -> a
s ^. l = getConst (l Const s)
{-# INLINE (^.) #-}

-- TODO: Create a package or a PR to indexed and indexed-extras
class IxZero m where
    izero :: m i j a

class IxZero m => IxPlus m where
    iplus :: m i j a -> m i j a -> m i j a


infixl 3 <<|>>
{-# INLINE (<<|>>) #-}
(<<|>>) :: IxPlus m => m i j a -> m i j a -> m i j a
(<<|>>) = iplus
