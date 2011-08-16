{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.USB.IDDB.Misc
    ( eitherMaybe
    , swap
    , restOfLine
    ) where

-- base
import Data.Either   ( Either, either )
import Data.Function ( ($), const, flip )
import Data.Maybe    ( Maybe(..) )
import Data.Tuple    ( uncurry )
import Prelude       ( String )

-- parsimony
import Parsimony        (Parser, manyTill)
import Parsimony.Char   (anyChar, newline)


eitherMaybe ∷ Either r α → Maybe α
eitherMaybe = either (const Nothing) Just

swap ∷ (α, β) → (β, α)
swap = uncurry $ flip (,)

restOfLine ∷ Parser String String
restOfLine = manyTill anyChar newline

