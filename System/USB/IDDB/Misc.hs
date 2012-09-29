{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

module System.USB.IDDB.Misc
    ( eitherMaybe
    , swap
    , restOfLine
    ) where

import "base" Data.Either   ( Either, either )
import "base" Data.Function ( ($), const, flip )
import "base" Data.Maybe    ( Maybe(..) )
import "base" Data.Tuple    ( uncurry )
import "base" Prelude       ( String )
import "parsimony" Parsimony      (Parser, manyTill)
import "parsimony" Parsimony.Char (anyChar, newline)


eitherMaybe ∷ Either r α → Maybe α
eitherMaybe = either (const Nothing) Just

swap ∷ (α, β) → (β, α)
swap = uncurry $ flip (,)

restOfLine ∷ Parser String String
restOfLine = manyTill anyChar newline

