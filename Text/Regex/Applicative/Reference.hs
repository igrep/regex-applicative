--------------------------------------------------------------------
-- |
-- Module    : Text.Regex.Applicative.Reference
-- Copyright : (c) Roman Cheplyaka
-- License   : MIT
--
-- Maintainer: Roman Cheplyaka <roma@ro-che.info>
-- Stability : experimental
--
-- Reference implementation (using backtracking).
--
-- This is exposed for testing purposes only!
--------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
module Text.Regex.Applicative.Reference (reference) where

import Prelude hiding (getChar)
import Text.Regex.Applicative.Types
import Data.Extensible
import Data.Functor.Const
import Control.Applicative
import Control.Monad
import Data.Functor.Indexed
import Control.Monad.Indexed
import Control.Monad.Indexed.State
import Control.Monad.Indexed.Trans


-- A simple parsing monad
newtype P s a = P { unP :: [s] -> [(a, [s])] }

instance Monad (P s) where
    return x = P $ \s -> [(x, s)]
    (P a) >>= k = P $ a >=> \(x, s') -> unP (k x) s'

instance Functor (P s) where
    fmap = liftM

instance Applicative (P s) where
    (<*>) = ap
    pure = return

instance Alternative (P s) where
    empty = P $ const []
    P a1 <|> P a2 = P $ \s ->
        a1 s ++ a2 s

instance MonadPlus (P s)

-- A simple parsing applicative with capturing feature.
newtype PC s i j a = PC { unPC :: IxStateT (P s) i j a }
    deriving (IxFunctor, IxPointed, IxApplicative, IxMonad, IxMonadZero, IxMonadPlus, IxMonadState)

instance Functor (PC s i j) where
    fmap f = PC . fmap f . unPC

instance Applicative (PC s i i) where
    (PC fa) <*> (PC fb) = PC $ fa <*> fb
    pure = PC . pure

instance Monad (PC s i i) where
    return = pure
    ma >>= k = PC (unPC . k =<< unPC ma)

instance IxZero (PC s) where
    izero = imzero

instance IxPlus (PC s) where
    iplus = implus

liftPC :: P s a -> PC s i i a
liftPC = PC . ilift

getChar :: P s s
getChar = P $ \case
    [] -> []
    c : cs -> [(c, cs)]

re2monad :: RE s (Record xs) (Record ys) a -> PC s (Record xs) (Record ys) a
re2monad r =
    case r of
        Eps -> return $ error "eps"
        Symbol _ p ->
            liftPC getChar >>>= \c ->
                case p c of
                  Just r' -> ireturn r'
                  Nothing -> liftPC empty
        Alt a1 a2 -> re2monad a1 `iplus` re2monad a2
        App a1 a2 -> re2monad a1 `iap` re2monad a2
        Capture k rx ->
            re2monad rx >>>= \x -> imodify (k @== x <:)
        Fmap f a -> f <$> re2monad a
        Refer k -> igets (^. k)
        Rep g f rb ra -> rep rb
          where
            am = re2monad ra
            rep b = combine (rep . f b =<<< am) (ireturn b)
            combine a b = case g of Greedy -> a `iplus` b; NonGreedy -> b `iplus` a
        Void a -> void $ re2monad a
        Fail -> izero


-- | Copied from the lens package
(^.) :: s -> Getting a s a -> a
s ^. l = getConst (l Const s)
{-# INLINE (^.) #-}

runP :: P s a -> [s] -> Maybe a
runP m s = case filter (null . snd) $ unP m s of
    (r, _) : _ -> Just r
    _ -> Nothing

runPC :: PC s (Record xs) (Record ys) a -> Record xs -> [s] -> Maybe (a, Record ys)
runPC (PC m) xs s = (`runP` s) $ runIxStateT m xs

-- | 'reference' @r@ @s@ should give the same results as @s@ '=~' @r@.
--
-- However, this is not very efficient implementation and is supposed to be
-- used for testing only.
reference :: RE s (Record '[]) (Record ys) a -> [s] -> Maybe (a, Record ys)
reference r = runPC (re2monad r) nil
