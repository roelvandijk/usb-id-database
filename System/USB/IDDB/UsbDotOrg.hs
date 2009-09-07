module System.USB.IDDB.UsbDotOrg
    ( parseDb
    , staticDb
    , fromFile
    , fromWeb
    ) where

import Control.Monad        (liftM)
import Data.Maybe           (fromJust)
import Data.String.UTF8     (UTF8, fromRep)
import Network.Download     (openURI)
import Parsimony
import Parsimony.Char       (char, digit)
import System.IO            (FilePath)
import System.USB.IDDB.Base ( IDDB(..)
                            , VendorID, VendorName
                            , getDataFileName
                            )
import System.USB.IDDB.Misc (BSParser, eitherMaybe, restOfLine)

import qualified Codec.Binary.UTF8.String as UTF8 (encode)
import qualified Data.Bimap               as BM   (fromList)
import qualified Data.ByteString          as BS   (ByteString, pack, readFile)
import qualified Data.Map                 as MP   (empty)


-- |Construct a database from a string in the format used by usb.org.
parseDb :: UTF8 BS.ByteString -> Maybe IDDB
parseDb = eitherMaybe . parse staticDbParser

staticDbParser :: BSParser IDDB
staticDbParser = do vendors <- many vendorParser
                    return IDDB { dbVendors  = BM.fromList vendors
                                , dbProducts = MP.empty
                                }
    where
      vendorParser :: BSParser (VendorID, VendorName)
      vendorParser = do vid  <- many1 digit
                        char '|'
                        name <- restOfLine
                        return (read vid, BS.pack $ UTF8.encode name)

-- |Load a vendor database from file. If the file can not be read for
--  some reason an error will be thrown.
fromFile :: FilePath -> IO (Maybe IDDB)
fromFile = liftM (parseDb . fromRep) . BS.readFile

-- |Construct a database from the list of companies available at
--  <http://www.usb.org/developers/tools/comp_dump>. The website
--  informs us that: /"Remember this list changes almost daily, be/
--  /sure to get a fresh copy when you use the tools"/. However, the
--  list seems to be quite stable. Using this function more than once
--  a day is probably overkill.
fromWeb :: IO (Maybe IDDB)
fromWeb = liftM ( either (const Nothing)
                                     (parseDb . fromRep)
                            ) $ openURI dbURL

staticDbPath :: FilePath
staticDbPath = "usb_dot_org_db.txt"

dbURL :: String
dbURL = "http://www.usb.org/developers/tools/comp_dump"

-- |Load a database from a snapshot of the usb.org database which is
--  supplied with the package.
staticDb :: IO IDDB
staticDb = getDataFileName staticDbPath >>= liftM fromJust . fromFile
