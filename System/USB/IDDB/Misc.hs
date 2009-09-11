module System.USB.IDDB.Misc
    ( eitherMaybe
    , swap
    , restOfLine
    ) where

import Parsimony        (Parser, manyTill)
import Parsimony.Char   (anyChar, newline)


eitherMaybe :: Either e a -> Maybe a
eitherMaybe = either (const Nothing) Just

swap :: (a, b) -> (b, a)
swap = uncurry $ flip (,)

restOfLine :: Parser String String
restOfLine = manyTill anyChar newline

