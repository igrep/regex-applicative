{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators, DataKinds, PolyKinds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fno-do-lambda-eta-expansion #-}
module Text.Regex.Applicative.Compile (compile) where

import Data.Extensible
import Data.Functor.Indexed
import Text.Regex.Applicative.Types
import Control.Applicative

compile :: RE s xs ys a -> (a -> [Thread s (Record ys) r]) -> [Thread s (Record xs) r]
compile e k = runChooseCont (compile2 e) (Single k)

data Choose a = Single !a | EmptyNonEmpty !a !a

{-
ContT r m a = (a -> m r) -> m r
IxState i j a = i -> (a, j)

ContT r (IxState i j) a = (a -> (IxState i j) r) -> (IxState i j) r
ContT r (IxState i j) a = (a -> (i -> (r, j)) -> (i -> (r, j))
ContT r (IxState i j) a = (a -> i -> (r, j)) -> i -> (r, j)

|
v

ContT r (IxState i j) a = Choose (a -> i -> (r, j)) -> i -> (r, j)
-}

instance Functor Choose where
    fmap f k =
        case k of
            Single a -> Single (f a)
            EmptyNonEmpty a b -> EmptyNonEmpty (f a) (f b)

newtype ChooseCont r o a = ChooseCont { runChooseCont :: Choose (a -> o) -> r }

instance IxFunctor ChooseCont where
    imap f mx = ChooseCont $ \ck -> runChooseCont mx $ (. f) <$> ck

instance IxPointed ChooseCont where
    ireturn = error "No IxPointed instance for ChooseCont is not defined"

instance IxApplicative ChooseCont where
    iap f v = ChooseCont $ \k ->
        case k of
            Single sk -> runChooseCont f . Single $ \g -> runChooseCont v $ Single (sk . g)
            EmptyNonEmpty ke kn ->
                runChooseCont f $ EmptyNonEmpty
                    (\g -> runChooseCont v $ EmptyNonEmpty (ke . g) (kn . g))
                    (\g -> runChooseCont v $ EmptyNonEmpty (kn . g) (kn . g))

chooseEmpty :: Choose a -> a
chooseEmpty k =
    case k of
        Single a -> a
        EmptyNonEmpty a _ -> a

chooseNonEmpty :: Choose a -> a
chooseNonEmpty k =
    case k of
        Single a -> a
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
compile2 :: RE s xs ys a -> ChooseCont [Thread s (Record xs) r] [Thread s (Record ys) r] a
compile2 e =
    case e of
        Eps -> ChooseCont $ \k -> chooseEmpty k ()
        Symbol i p -> ChooseCont $ \k -> [t $ chooseNonEmpty k] where
          -- t :: (a -> [Thread s r]) -> Thread s r
          t k = Thread i $ \s ->
            case p s of
              Just r -> k r
              Nothing -> []
        App n1 n2 -> compile2 n1 <<*>> compile2 n2
        Alt n1 n2 ->
            let a1 = runChooseCont $ compile2 n1
                a2 = runChooseCont $ compile2 n2
            in ChooseCont $ \k -> a1 k ++ a2 k
        Capture key e' ->
            let k = runChooseCont $ compile2 e'
                f :: [Thread s (Record xs) r] -> [Thread s (Record (_ ': xs)) r]
                f = undefined
            in ChooseCont (f . k)
        Fail -> ChooseCont $ const []
        Fmap f n -> f <<$>> compile2 n
        -- This is actually the point where we use the difference between
        -- continuations. For the inner RE the empty continuation is a
        -- "failing" one in order to avoid non-termination.
        Rep g f b n ->
            let a = runChooseCont $ compile2 n
                threads b' k =
                    combine g
                        (a $ EmptyNonEmpty (\_ -> []) (\v -> let b'' = f b' v in threads b'' (Single $ chooseNonEmpty k)))
                        (chooseEmpty k b')
            in ChooseCont $ threads b

combine :: Greediness -> [a] -> [a] -> [a]
combine g continue stop =
    case g of
        Greedy -> continue ++ stop
        NonGreedy -> stop ++ continue
