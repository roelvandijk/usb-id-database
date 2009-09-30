module System.USB.IDDB.UsbDotOrg
    ( parseDb
    , staticDb
    , fromFile
    , dbURL
    ) where

import Control.Monad        ( fmap )
import Data.Maybe           ( fromJust )
import Parsimony
import Parsimony.Char       ( char, digit )
import System.IO            ( FilePath, readFile )
import System.USB.IDDB.Base ( IDDB(..)
                            , getDataFileName
                            )
import System.USB.IDDB.Misc ( eitherMaybe, swap, restOfLine )

import qualified Data.IntMap as IM ( fromList, empty )
import qualified Data.Map    as MP ( fromList )


-- |Construct a database from a string in the format used by usb.org.
parseDb :: String -> Maybe IDDB
parseDb = eitherMaybe . parse staticDbParser

staticDbParser :: Parser String IDDB
staticDbParser = do
  vendors <- many vendorParser
  return IDDB { dbVendorNameId = MP.fromList $ fmap swap vendors
              , dbVendorIdName = IM.fromList vendors
              , dbProducts  = IM.empty
              , dbClasses   = IM.empty
              , dbACT       = IM.empty
              , dbLanguages = IM.empty
              }
    where
      vendorParser :: Parser String (Int, String)
      vendorParser = do vid  <- many1 digit
                        char '|'
                        name <- restOfLine
                        return (read vid, name)

-- |Load a vendor database from file. If the file can not be read for
--  some reason an error will be thrown.
fromFile :: FilePath -> IO (Maybe IDDB)
fromFile = fmap parseDb . readFile

staticDbPath :: FilePath
staticDbPath = "usb_dot_org_db.txt"

dbURL :: String
dbURL = "http://www.usb.org/developers/tools/comp_dump"

-- |Load a database from a snapshot of the usb.org database which is
--  supplied with the package.
staticDb :: IO IDDB
staticDb = getDataFileName staticDbPath >>= fmap fromJust . fromFile
