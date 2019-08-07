--------------------------------------------------------------------
-- |
-- Module    : Text.Regex.Applicative
-- Copyright : (c) Roman Cheplyaka
-- License   : MIT
--
-- Maintainer: Roman Cheplyaka <roma@ro-che.info>
-- Stability : experimental
--
-- To get started, see some examples on the wiki:
-- <https://github.com/feuerbach/regex-applicative/wiki/Examples>
--------------------------------------------------------------------

module Text.Regex.Applicative
    ( RE
    , sym
    , psym
    , msym
    , anySym
    , string
    , reFoldl
    , Greediness(..)
    , few
    , match
    , (=~)
    , refer
    , capture
    , captureChar
    , module Control.Applicative
    , module Data.Functor.Indexed
    )
    where
import Text.Regex.Applicative.Types
import Text.Regex.Applicative.Interface
import Control.Applicative
import Data.Functor.Indexed
