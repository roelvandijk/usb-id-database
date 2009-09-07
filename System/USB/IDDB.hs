{-# LANGUAGE CPP #-}

{-| A database of USB identifiers.

Databases with vendor names and identifiers can be loaded from string,
file or directly from <http://www.usb.org> or
<http://linux-usb.sourceforge.net>.

Example usage:

@
import System.USB.IDDB
import Data.ByteString.Char8 (pack, unpack)

main :: IO ()
main = do demo =<< 'linuxUsbIdDb'
          demo =<< 'usbDotOrgDb'

demo :: 'IDDB' -> IO ()
demo db = do -- Print the name of vendor 0x1D6B
             putStrLn $ maybe \"unknown ID!\" unpack
                      $ 'vendorName' db 0x1D6B
             -- Print the ID of \"The Linux Foundation\"
             putStrLn $ maybe \"unknown name!\" show
                      $ 'vendorId' db (pack \"The Linux Foundation\")
@
-}
module System.USB.IDDB
    ( -- *Types
      IDDB
    , VendorID
    , VendorName
    , ProductID
    , ProductName

      -- *Acquire database
    , emptyDb

      -- **usb.org
    , parseUsbDotOrgDb
    , usbDotOrgDb
    , usbDotOrgDbFromWeb

      -- **linux-usb.sourceforge.net
    , parseUsbIdRepoDb
    , usbIdRepoDb
    , usbIdRepoDbFromWeb

      -- *Query database
    , vendorName
    , vendorId
    , productName
    , productId
    )
    where

import Control.Monad            (liftM)
import Data.Encoding            ( decodeStrictByteString
                                , encodeStrictByteString
                                )
import Data.Maybe               (fromJust)
import Data.String.UTF8         (UTF8, fromRep)
import Network.Download         (openURI)
import Numeric                  (readHex)
import Parsimony
import Parsimony.Char
import System.IO                (FilePath)

import qualified Codec.Binary.UTF8.String as UTF8 (encode)
import qualified Data.Bimap               as BM
import qualified Data.ByteString          as BS ( ByteString
                                                , pack
                                                , readFile
                                                )
import qualified Data.Encoding.ISO88591   as Enc (ISO88591(..))
import qualified Data.Encoding.UTF8       as Enc (UTF8(..))
import qualified Data.Map                 as MP

#ifdef BUILD_WITH_CABAL
import Paths_usb_id_database (getDataFileName)
#else
getDataFileName :: FilePath -> IO FilePath
getDataFileName = return
#endif

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type VendorID  = Int
type ProductID = Int

type VendorName  = BS.ByteString
type ProductName = BS.ByteString

-- |A database of USB identifiers. Contains both vendor identifiers
-- and product identifiers.
data IDDB = IDDB { dbVendors  :: BM.Bimap VendorID VendorName
                 , dbProducts :: MP.Map   VendorID ProductDB
                 }

type ProductDB = BM.Bimap ProductID ProductName

type BSParser a = Parser (UTF8 BS.ByteString) a

-------------------------------------------------------------------------------
-- Empty
-------------------------------------------------------------------------------

-- |An empty database.
emptyDb :: IDDB
emptyDb = IDDB { dbVendors  = BM.empty
               , dbProducts = MP.empty
               }

-------------------------------------------------------------------------------
-- www.usb.org
-------------------------------------------------------------------------------

-- |Construct a database from a string in the format used by usb.org.
parseUsbDotOrgDb :: UTF8 BS.ByteString -> Maybe IDDB
parseUsbDotOrgDb = eitherMaybe . parse usbDotOrgDbParser

usbDotOrgDbParser :: BSParser IDDB
usbDotOrgDbParser = do vendors <- many vendorParser
                       return $ IDDB { dbVendors  = BM.fromList vendors
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
usbDotOrgDbFromFile :: FilePath -> IO (Maybe IDDB)
usbDotOrgDbFromFile = liftM (parseUsbDotOrgDb . fromRep) . BS.readFile

-- |Construct a database from the list of companies available at
--  <http://www.usb.org/developers/tools/comp_dump>. The website
--  informs us that: /"Remember this list changes almost daily, be/
--  /sure to get a fresh copy when you use the tools"/. However, the
--  list seems to be quite stable. Using this function more than once
--  a day is probably overkill.
usbDotOrgDbFromWeb :: IO (Maybe IDDB)
usbDotOrgDbFromWeb = liftM ( either (const Nothing)
                                     (parseUsbDotOrgDb . fromRep)
                            ) $ openURI usbDotOrgUrl

usbDotOrgDataFile :: FilePath
usbDotOrgDataFile = "usb_dot_org_db.txt"

usbDotOrgUrl :: String
usbDotOrgUrl = "http://www.usb.org/developers/tools/comp_dump"

-- |Load a database from a snapshot of the usb.org database which is
--  supplied with the package.
usbDotOrgDb :: IO IDDB
usbDotOrgDb = getDataFileName usbDotOrgDataFile >>= liftM fromJust . usbDotOrgDbFromFile

-------------------------------------------------------------------------------
-- linux-usb.sourceforge.net
-------------------------------------------------------------------------------

-- |Construct a database from a string in the format used by linux-usb.sourceforge.net.
parseUsbIdRepoDb :: UTF8 BS.ByteString -> Maybe IDDB
parseUsbIdRepoDb = eitherMaybe . parse usbIdRepoDbParser

usbIdRepoDbParser :: BSParser IDDB
usbIdRepoDbParser = do
  spaces >> many (lexeme comment)
  xs <- many vendorParser
  return $ IDDB { dbVendors  = BM.fromList [(vid, name) | (vid, name, _)   <- xs]
                , dbProducts = MP.fromList [(vid, pdb)  | (vid, _,    pdb) <- xs]
                }
    where
      lexeme :: BSParser a -> BSParser a
      lexeme p = do x <- p
                    spaces
                    return x

      comment :: BSParser String
      comment = char '#' >> restOfLine

      hexId :: Num n => BSParser n
      hexId = do ds <- count 4 hexDigit
                 case readHex ds of
                   [(n, _)]  -> return n
                   _         -> error "impossible"

      vendorParser :: BSParser (VendorID, VendorName, ProductDB)
      vendorParser = do vid <- hexId
                        spaces
                        name <- restOfLine
                        products <- many (tab >> productParser)
                        return ( vid
                               , BS.pack $ UTF8.encode name
                               , BM.fromList products
                               )

      productParser :: BSParser (ProductID, ProductName)
      productParser = do pid <- hexId
                         spaces
                         name <- restOfLine
                         return (pid, BS.pack $ UTF8.encode name)

-- |Construct a database from the data available at
-- <http://linux-usb.sourceforge.net/usb.ids>.
usbIdRepoDbFromWeb :: IO (Maybe IDDB)
usbIdRepoDbFromWeb = liftM ( either (const Nothing)
                                    (parseUsbIdRepoDb . fromRep . iso88591_to_utf8)
                           ) $ openURI usbIdRepoURL

usbIdRepoDbFromFile :: FilePath -> IO (Maybe IDDB)
usbIdRepoDbFromFile = liftM (parseUsbIdRepoDb . fromRep . iso88591_to_utf8) . BS.readFile

iso88591_to_utf8 :: BS.ByteString -> BS.ByteString
iso88591_to_utf8 = encodeStrictByteString Enc.UTF8
                 . decodeStrictByteString Enc.ISO88591

usbIdRepoDb :: IO IDDB
usbIdRepoDb = getDataFileName usbIdRepoDataFile >>= liftM fromJust . usbIdRepoDbFromFile

usbIdRepoDataFile :: FilePath
usbIdRepoDataFile = "usb_id_repo_db.txt"

usbIdRepoURL :: String
usbIdRepoURL = "http://linux-usb.sourceforge.net/usb.ids"

-------------------------------------------------------------------------------
-- Query database
-------------------------------------------------------------------------------

-- |Retrieve the name of a vendor given its ID.
vendorName :: IDDB -> VendorID -> Maybe VendorName
vendorName db vid = BM.lookup vid (dbVendors db)

-- |Retrieve the ID of a vendor given its name.
vendorId :: IDDB -> VendorName -> Maybe VendorID
vendorId db name = BM.lookupR name (dbVendors db)

productName :: IDDB -> VendorID -> ProductID -> Maybe ProductName
productName db vid pid = BM.lookup pid =<< MP.lookup vid (dbProducts db)

productId :: IDDB -> VendorID -> ProductName -> Maybe ProductID
productId db vid name = BM.lookupR name =<< MP.lookup vid (dbProducts db)

-------------------------------------------------------------------------------
-- Utility
-------------------------------------------------------------------------------

eitherMaybe :: Either e a -> Maybe a
eitherMaybe = either (const Nothing) Just

restOfLine :: BSParser String
restOfLine = manyTill anyChar newline
