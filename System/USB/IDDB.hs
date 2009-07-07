{-# LANGUAGE CPP #-}

{-| A database of USB identifiers.

Databases with vendor names and identifiers can be loaded from
string, file or directly from <http://www.usb.org>.

Example usage:

@
module Main where

import System.USB.IDDB
import Data.ByteString.Char8 (pack, unpack)

main :: IO ()
main = do -- Acquire the default database
          db <- 'vdbDefault'
          -- Print the name of vendor 0x1D6B
          'putStrLn' $ 'maybe \"unknown ID!\" 'unpack'
                   $ 'vendorName' db 0x1D6B
          -- Print the ID of \"The Linux Foundation\"
          'putStrLn' $ 'maybe' \"unknown name!\" 'show'
                   $ 'vendorID' db ('pack' \"The Linux Foundation\")
@

EBNF grammar of the textual representation of a vendor database:

>  vendor database = {row};
>  row             = vendor id, "|", vendor name;
>  vendor id       = natural number;
>  vendor name     = ASCII string;
>  natural number  = positive digit, {digit}
>  positive digit  = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9";
>  digit           = "0" | positive digit;
-}
module System.USB.IDDB
    ( -- *Types
      VendorID
    , VendorName
    , VendorDB

      -- *Acquire database
    , vdbFromString
    , vdbFromFile
    , vdbFromUsbDotOrg
    , vdbDefault

      -- *Export database
    , vdbToString
    , vdbToFile

      -- *Query database
    , vendorName
    , vendorID
    )
    where

import Control.Arrow    ((>>>))
import Control.Monad    (liftM)
import Data.Char        (isSpace)
import Data.Maybe       (fromJust)
import Network.Download (openURI)
import System.IO        (FilePath)
import Text.Read        (reads)

import qualified Data.Bimap            as BM
import qualified Data.ByteString.Char8 as BS

#ifdef BUILD_WITH_CABAL
import Paths_usb_id_database (getDataFileName)
#else
getDataFileName :: FilePath -> IO FilePath
getDataFileName = return
#endif

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- |A numerical identifier for a vendor.
type VendorID   = Int
-- |The name of a company/entity which has acquired an official ID
--  from the USB Implementors Forum.
type VendorName = BS.ByteString

-- |A database of USB vendors. Associates numerical vendor ID's with
--  vendor names and vice versa.
type VendorDB = BM.Bimap VendorID VendorName

-------------------------------------------------------------------------------
-- Acquire database
-------------------------------------------------------------------------------

-- |Construct a vendor database from a string.
vdbFromString :: BS.ByteString -> Maybe VendorDB
vdbFromString = strip >>> BS.lines >>> map parseLine >>> sequence
                >>> maybe Nothing (Just . BM.fromList)
    where parseLine :: BS.ByteString -> Maybe (VendorID, VendorName)
          parseLine line = let (vid, rest) = BS.break (== '|') line
                           in if BS.null rest
                              then Nothing
                              else let name = BS.tail rest
                                   in case reads (BS.unpack vid) of
                                        []           -> Nothing
                                        ((vid',_):_) -> Just (vid', name)

-- |Load a vendor database from file. If the file can not be read for
--  some reason an error will be thrown.
vdbFromFile :: FilePath -> IO (Maybe VendorDB)
vdbFromFile = liftM vdbFromString . BS.readFile

vdbUsbDotOrgUrl :: String
vdbUsbDotOrgUrl = "http://www.usb.org/developers/tools/comp_dump"

-- |Construct a vendor database from the list of companies available
--  at <http://www.usb.org/developers/tools/comp_dump>. The website
--  informs us that: /"Remember this list changes almost daily, be/
--  /sure to get a fresh copy when you use the tools"/. However, the
--  list seems to be quite stable. Using this function more than once
--  a day is probably overkill.
vdbFromUsbDotOrg :: IO (Maybe VendorDB)
vdbFromUsbDotOrg = liftM (either (const Nothing) vdbFromString)
                   $ openURI vdbUsbDotOrgUrl

vdbDataFile :: FilePath
vdbDataFile = "usb_vendor_list.txt"

-- |Load a vendor database from a static file which is supplied with
--  the package.
vdbDefault :: IO VendorDB
vdbDefault = getDataFileName vdbDataFile >>= liftM fromJust . vdbFromFile

-------------------------------------------------------------------------------
-- Export database
-------------------------------------------------------------------------------

-- |Convert a vendor database to its textual representation.
vdbToString :: VendorDB -> BS.ByteString
vdbToString = BS.unlines . map row . BM.toAscList
    where row (vid, name) = BS.pack (show vid)
                            `BS.append` BS.singleton '|'
                            `BS.append` name

-- |Write a database to a file. If this file is not accessible an
--  error will be thrown.
vdbToFile :: FilePath -> VendorDB -> IO ()
vdbToFile fp = BS.writeFile fp . vdbToString

-------------------------------------------------------------------------------
-- Query database
-------------------------------------------------------------------------------

-- |Retrieve the name of a vendor given its ID.
vendorName :: VendorDB -> VendorID -> Maybe VendorName
vendorName db i = BM.lookup i db

-- |Retrieve the ID of a vendor given its name.
vendorID :: VendorDB -> VendorName -> Maybe VendorID
vendorID db name = BM.lookupR name db

-------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------

stripL :: BS.ByteString -> BS.ByteString
stripL = snd . BS.span isSpace

stripR :: BS.ByteString -> BS.ByteString
stripR = fst . BS.spanEnd isSpace

strip :: BS.ByteString -> BS.ByteString
strip = stripR . stripL

-------------------------------------------------------------------------------
-- Properties
-------------------------------------------------------------------------------

{--

-- |Converting a vendor DB to a string and back should yield the same DB.
prop_toAndFromString :: VendorDB -> Bool
prop_toAndFromString db = maybe False (== db) (vdbFromString $ vdbToString db)

prop_fromAndToString :: String -> Bool
prop_fromAndToString = maybe True prop_toAndFromString . vdbFromString

--}
