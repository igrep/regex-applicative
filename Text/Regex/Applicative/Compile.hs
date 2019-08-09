{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fno-do-lambda-eta-expansion #-}
module Text.Regex.Applicative.Compile (compile) where

import Control.Applicative
import Control.Monad.Trans.State
import Data.Extensible hiding (State)
import qualified Data.IntMap as IntMap
import Data.Maybe
import Text.Regex.Applicative.Types

compile :: RE s xs ys a -> Record xs -> (Record ys -> a -> [Thread s (Record xs) r]) -> [Thread s (Record xs) r]
compile e cs k = compile2 e cs (SingleCont k)

data Cont a = SingleCont !a | EmptyNonEmpty !a !a

instance Functor Cont where
    fmap f k =
        case k of
            SingleCont a -> SingleCont (f a)
            EmptyNonEmpty a b -> EmptyNonEmpty (f a) (f b)

emptyCont :: Cont a -> a
emptyCont k =
    case k of
        SingleCont a -> a
        EmptyNonEmpty a _ -> a
nonEmptyCont :: Cont a -> a
nonEmptyCont k =
    case k of
        SingleCont a -> a
        EmptyNonEmpty _ a -> a

-- The whole point of this module is this function, compile2, which needs to be
-- compiled with -fno-do-lambda-eta-expansion for efficiency.
--
-- Since this option would make other code perform worse, we place this
-- function in a separate module and make sure it's not inlined.
--
-- The point of "-fno-do-lambda-eta-expansion" is to make sure the tree is
-- "compiled" only once.
--
-- compile2 function takes two continuations: one when the match is empty and
-- one when the match is non-empty. See the "Rep" case for the reason.
compile2 :: forall s xs ys a r. RE s xs ys a -> Record xs -> Cont (Record ys -> a -> [Thread s (Record xs) r]) -> [Thread s (Record xs) r]
compile2 e =
    case e of
        Eps -> \cs k -> emptyCont k cs ()
        Symbol i p -> \cs k -> [t cs $ nonEmptyCont k] where
          t :: Record xs -> (Record xs -> a -> [Thread s (Record ys) r]) -> Thread s (Record ys) r
          t cs k = Thread i $ \s ->
            case p s of
              Just r -> k cs r
              Nothing -> []
        App n1 n2 ->
            let a1 = compile2 n1
                a2 = compile2 n2
            in \cs k -> case k of
                SingleCont k' -> a1 cs . SingleCont $ \cs1 a1_value -> a2 cs1 $ SingleCont (\cs0 -> map (mapThreadIx (const cs1)) . k' cs0 . a1_value)
                EmptyNonEmpty ke kn ->
                    let empty cs1 a1_value =
                            a2 cs1 $ EmptyNonEmpty (\cs0 -> map (mapThreadIx (const cs1)) . ke cs0 . a1_value) (\cs0 -> map (mapThreadIx (const cs1)) . kn cs0 . a1_value)
                        nonEmpty cs1 a1_value =
                            a2 cs1 $ EmptyNonEmpty (\cs0 -> map (mapThreadIx (const cs1)) . kn cs0 . a1_value) (\cs0 -> map (mapThreadIx (const cs1)) . kn cs0 . a1_value)
                    in a1 cs $ EmptyNonEmpty empty nonEmpty
        Alt n1 n2 ->
            let a1 = compile2 n1
                a2 = compile2 n2
            in \cs k -> a1 cs k ++ a2 cs k
        Capture key n ->
            let a = compile2 n
                t k' cs0' v = k' (key @== v <: cs0') v
            in \cs0 k -> a cs0 $ t <$> k
        Fail -> const $ const []
        Fmap f n -> let a = compile2 n in \cs k -> a cs $ fmap (\f' cs' a_value -> f' cs' (f a_value) ) k
        -- This is actually the point where we use the difference between
        -- continuations. For the inner RE the empty continuation is a
        -- "failing" one in order to avoid non-termination.
        Rep g f b n ->
            let a = compile2 n
                threads :: a -> Record xs -> Cont (Record xs -> a -> [Thread s (Record ys) r]) -> [Thread s (Record ys) r]
                threads b' cs k =
                    combine g
                        (a cs $ EmptyNonEmpty (\_ _ -> []) (\cs' v -> let b'' = f b' v in threads b'' cs' (SingleCont $ nonEmptyCont k)))
                        (emptyCont k cs b')
            in threads b

combine :: Greediness -> [a] -> [a] -> [a]
combine g continue stop =
    case g of
        Greedy -> continue ++ stop
        NonGreedy -> stop ++ continue
