module System.USB.IDDB.Misc
    ( BSParser
    , eitherMaybe
    , restOfLine
    ) where

import Data.ByteString  (ByteString)
import Data.String.UTF8 (UTF8)
import Parsimony        (Parser, manyTill)
import Parsimony.Char   (anyChar, newline)


type BSParser = Parser (UTF8 ByteString)


eitherMaybe :: Either e a -> Maybe a
eitherMaybe = either (const Nothing) Just

restOfLine :: BSParser String
restOfLine = manyTill anyChar newline
