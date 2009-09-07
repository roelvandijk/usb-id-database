module System.USB.IDDB.LinuxUsbIdRepo
    ( parseDb
    , staticDb
    , fromFile
    , fromWeb
    ) where

import Control.Monad        (liftM)
import Data.Encoding        ( decodeStrictByteString
                            , encodeStrictByteString
                            )
import Data.Maybe           (fromJust)
import Data.String.UTF8     (UTF8, fromRep)
import Network.Download     (openURI)
import Numeric              (readHex)
import Parsimony
import Parsimony.Char       (char, hexDigit, spaces, tab)
import System.IO            (FilePath)
import System.USB.IDDB.Base ( IDDB(..)
                            , VendorID, VendorName
                            , ProductID, ProductName
                            , getDataFileName
                            )
import System.USB.IDDB.Misc (BSParser, eitherMaybe, restOfLine)

import qualified Codec.Binary.UTF8.String as UTF8 (encode)
import qualified Data.Bimap               as BM   (Bimap, fromList)
import qualified Data.ByteString          as BS   (ByteString, pack, readFile)
import qualified Data.Encoding.ISO88591   as Enc  (ISO88591(..))
import qualified Data.Encoding.UTF8       as Enc  (UTF8(..))
import qualified Data.Map                 as MP   (fromList)


-- |Construct a database from a string in the format used by
-- <http://linux-usb.org>.
parseDb :: UTF8 BS.ByteString -> Maybe IDDB
parseDb = eitherMaybe . parse dbParser

dbParser :: BSParser IDDB
dbParser = do
  spaces >> many (lexeme comment)
  xs <- many vendorParser
  return IDDB { dbVendors  = BM.fromList [(vid, name) | (vid, name, _)   <- xs]
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

      vendorParser :: BSParser (VendorID, VendorName, BM.Bimap ProductID ProductName)
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
-- <http://linux-usb.org/usb.ids>.
fromWeb :: IO (Maybe IDDB)
fromWeb = liftM ( either (const Nothing)
                                    (parseDb . fromRep . iso88591_to_utf8)
                           ) $ openURI dbURL

fromFile :: FilePath -> IO (Maybe IDDB)
fromFile = liftM (parseDb . fromRep . iso88591_to_utf8) . BS.readFile

iso88591_to_utf8 :: BS.ByteString -> BS.ByteString
iso88591_to_utf8 = encodeStrictByteString Enc.UTF8
                 . decodeStrictByteString Enc.ISO88591

staticDb :: IO IDDB
staticDb = getDataFileName staticDbPath >>= liftM fromJust . fromFile

staticDbPath :: FilePath
staticDbPath = "usb_id_repo_db.txt"

dbURL :: String
dbURL = "http://linux-usb.org/usb.ids"
