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

module Text.Regex.Applicative.Original
    ( RE
    , sym
    , psym
    , msym
    , anySym
    , string
    , reFoldl
    , Greediness(..)
    , few
    , comap
    , withMatched
    , match
    , (=~)
    , replace
    , findFirstPrefix
    , findLongestPrefix
    , findShortestPrefix
    , findFirstInfix
    , findLongestInfix
    , findShortestInfix
    , module Control.Applicative
    )
    where
import Text.Regex.Applicative.Original.Types
import Text.Regex.Applicative.Original.Interface
import Control.Applicative