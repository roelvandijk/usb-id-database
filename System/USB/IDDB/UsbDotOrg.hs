module System.USB.IDDB.UsbDotOrg
    ( parseDb
    , staticDb
    , fromFile
    , fromWeb
    ) where

import Control.Monad        ( fmap )
import Data.Maybe           ( fromJust )
import Network.Download     ( openURIString )
import Parsimony
import Parsimony.Char       ( char, digit )
import System.IO            ( FilePath, readFile )
import System.USB.IDDB.Base ( IDDB(..), VendorID, VendorName
                            , getDataFileName
                            )
import System.USB.IDDB.Misc ( eitherMaybe, swap, restOfLine )

import qualified Data.IntMap as IM ( fromList, empty )
import qualified Data.Map    as MP ( fromList )


-- |Construct a database from a string in the format used by usb.org.
parseDb :: String -> Maybe IDDB
parseDb = eitherMaybe . parse staticDbParser

staticDbParser :: Parser String IDDB
staticDbParser = do vendors <- many vendorParser
                    return IDDB { dbVendorNameId = MP.fromList $ fmap swap vendors
                                , dbVendorIdName = IM.fromList vendors
                                , dbProducts = IM.empty
                                , dbClasses  = IM.empty
                                }
    where
      vendorParser :: Parser String (VendorID, VendorName)
      vendorParser = do vid  <- many1 digit
                        char '|'
                        name <- restOfLine
                        return (read vid, name)

-- |Load a vendor database from file. If the file can not be read for
--  some reason an error will be thrown.
fromFile :: FilePath -> IO (Maybe IDDB)
fromFile = fmap parseDb . readFile

-- |Construct a database from the list of companies available at
--  <http://www.usb.org/developers/tools/comp_dump>. The website
--  informs us that: /"Remember this list changes almost daily, be/
--  /sure to get a fresh copy when you use the tools"/. However, the
--  list seems to be quite stable. Using this function more than once
--  a day is probably overkill.
fromWeb :: IO (Maybe IDDB)
fromWeb = fmap ( either (const Nothing)
                        parseDb
               ) $ openURIString dbURL

staticDbPath :: FilePath
staticDbPath = "usb_dot_org_db.txt"

dbURL :: String
dbURL = "http://www.usb.org/developers/tools/comp_dump"

-- |Load a database from a snapshot of the usb.org database which is
--  supplied with the package.
staticDb :: IO IDDB
staticDb = getDataFileName staticDbPath >>= fmap fromJust . fromFile
