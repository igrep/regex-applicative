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

compile :: RE s xs ys a -> (a -> [Thread s (Record xs -> Record ys) r]) -> [Thread s (Record xs -> Record ys) r]
compile e k = compile2 e (SingleCont k)

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
compile2 :: forall s xs ys a r. RE s xs ys a -> Cont (a -> [Thread s (Record xs -> Record ys) r]) -> [Thread s (Record xs -> Record ys) r]
compile2 e =
    case e of
        Eps -> \k -> emptyCont k ()
        Symbol i p -> \k -> [t $ nonEmptyCont k] where
          -- t :: (a -> [Thread s (Record xs) (Record ys) r]) -> Thread s (Record xs) (Record ys) r
          t k = Thread i $ \s xs ->
            case p s of
              Just r -> k r
              Nothing -> []
        App n1 n2 ->
            let a1 = compile2 n1
                a2 = compile2 n2
            in \k -> case k of
                SingleCont k' -> a1 $ SingleCont $ \a1_value -> a2 $ SingleCont $ k' . a1_value
                EmptyNonEmpty ke kn ->
                    a1 $ EmptyNonEmpty
                        -- empty
                        (\a1_value -> a2 $ EmptyNonEmpty (ke . a1_value) (kn . a1_value))
                        -- non-empty
                        (\a1_value -> a2 $ EmptyNonEmpty (kn . a1_value) (kn . a1_value))
        Alt n1 n2 ->
            let a1 = compile2 n1
                a2 = compile2 n2
            in \k -> a1 k ++ a2 k
        Fail -> const []
        Fmap f n -> let a = compile2 n in \k -> a $ fmap (. f) k
        -- This is actually the point where we use the difference between
        -- continuations. For the inner RE the empty continuation is a
        -- "failing" one in order to avoid non-termination.
        Rep g f b n ->
            let a = compile2 n
                threads b' k =
                    combine g
                        (a $ EmptyNonEmpty (\_ -> []) (\v -> let b'' = f b' v in threads b'' (SingleCont $ nonEmptyCont k)))
                        (emptyCont k b')
            in threads b

combine :: Greediness -> [a] -> [a] -> [a]
combine g continue stop =
    case g of
        Greedy -> continue ++ stop
        NonGreedy -> stop ++ continue
