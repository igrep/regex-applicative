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
module Text.Regex.Applicative.Reference (referenceIx, reference) where

import Prelude hiding (getChar)
import Text.Regex.Applicative.Types
import Data.Extensible
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

re2IxMonad :: Eq s => RE s xs ys a -> PC s (Record xs) (Record ys) a
re2IxMonad r =
    case r of
        Eps -> return $ error "eps"
        Symbol _ p ->
            liftPC getChar >>>= \c ->
                case p c of
                  Just r' -> ireturn r'
                  Nothing -> liftPC empty
        Alt a1 a2 -> re2IxMonad a1 `iplus` re2IxMonad a2
        App a1 a2 -> re2IxMonad a1 `iap` re2IxMonad a2
        Capture k rx ->
            re2IxMonad rx >>>= \x -> imodify (k @== x <:) *>> ireturn x
        Fmap f a -> f <$> re2IxMonad a
        Refer k -> igets (^. k) >>>=
            traverse (re2IxMonad . Symbol (error "Not numbered symbol") . (\c1 c2 -> if c1 == c2 then Just c1 else Nothing))
        Rep g f rb ra -> rep rb
          where
            am = re2IxMonad ra
            rep b = combine (rep . f b =<<< am) (ireturn b)
            combine a b = case g of Greedy -> a `iplus` b; NonGreedy -> b `iplus` a
        Void a -> void $ re2IxMonad a
        Fail -> izero

-- | For original regex-applicative's 'reference' implementation.
re2Monad :: RE s xs ys a -> P s a
re2Monad r =
    case r of
        Eps -> return $ error "eps"
        Symbol _ p -> do
            c <- getChar
            case p c of
              Just r' -> return r'
              Nothing -> empty
        Alt a1 a2 -> re2Monad a1 <|> re2Monad a2
        App a1 a2 -> re2Monad a1 <*> re2Monad a2
        Capture {} -> error "Capture is not supported by re2Monad"
        Fmap f a -> fmap f $ re2Monad a
        Refer {} -> error "Refer is not supported by re2Monad"
        Rep g f rb ra -> rep rb
            where
            am = re2Monad ra
            rep b = combine (do a <- am; rep $ f b a) (return b)
            combine a b = case g of Greedy -> a <|> b; NonGreedy -> b <|> a
        Void a -> re2Monad a >> return ()
        Fail -> empty

runP :: Eq s => P s a -> [s] -> Maybe a
runP m s = case filter (null . snd) $ unP m s of
    (r, _) : _ -> Just r
    _ -> Nothing

runPC :: Eq s => PC s (Record xs) (Record ys) a -> Record xs -> [s] -> Maybe (a, Record ys)
runPC (PC m) xs s = (`runP` s) $ runIxStateT m xs

referenceIx :: Eq s => RE s '[] ys a -> [s] -> Maybe (a, Record ys)
referenceIx r = runPC (re2IxMonad r) nil

-- | 'reference' @r@ @s@ should give the same results as @s@ '=~' @r@.
--
-- However, this is not very efficient implementation and is supposed to be
-- used for testing only.
reference :: Eq s => RE s '[] '[] a -> [s] -> Maybe a
reference r = runP (re2Monad r)
