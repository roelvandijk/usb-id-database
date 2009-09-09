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
import System.USB.IDDB.Base
import System.USB.IDDB.Misc (BSParser, eitherMaybe, restOfLine)

import qualified Codec.Binary.UTF8.String as UTF8 (encode)
import qualified Data.Bimap               as BM   (Bimap, fromList)
import qualified Data.ByteString          as BS   (ByteString, pack, readFile)
import qualified Data.Encoding.ISO88591   as Enc  (ISO88591(..))
import qualified Data.Encoding.UTF8       as Enc  (UTF8(..))
import qualified Data.Map                 as MP   (Map, fromList)


-- |Construct a database from a string in the format used by
-- <http://linux-usb.org>.
parseDb :: UTF8 BS.ByteString -> Maybe IDDB
parseDb = eitherMaybe . parse dbParser

dbParser :: BSParser IDDB
dbParser = do spaces
              comments
              (vendorDB, productDB) <- lexeme vendorSection
              comments
              classDB <- classSection

              return IDDB { dbVendors  = vendorDB
                          , dbProducts = productDB
                          , dbClasses  = classDB
                          }
    where
      utf8BS :: String -> BS.ByteString
      utf8BS = BS.pack . UTF8.encode

      lexeme :: BSParser a -> BSParser a
      lexeme p = do x <- p
                    spaces
                    return x

      comment :: BSParser String
      comment = char '#' >> restOfLine

      comments :: BSParser [String]
      comments = many $ lexeme comment

      hexId :: Num n => Int -> BSParser n
      hexId d = do ds <- count d hexDigit
                   case readHex ds of
                     [(n, _)]  -> return n
                     _         -> error "impossible"

      vendorSection :: BSParser (VendorDB, MP.Map VendorID ProductDB)
      vendorSection = do xs <- lexeme $ many vendorParser
                         return ( BM.fromList [(vid, name) | (vid, name, _)   <- xs]
                                , MP.fromList [(vid, pdb)  | (vid, _,    pdb) <- xs]
                                )

      vendorParser :: BSParser (VendorID, VendorName, BM.Bimap ProductID ProductName)
      vendorParser = do vid  <- hexId 4
                        count 2 $ char ' '
                        name <- restOfLine
                        products <- many productParser
                        return ( vid
                               , utf8BS name
                               , BM.fromList products
                               )

      productParser :: BSParser (ProductID, ProductName)
      productParser = do tab
                         pid  <- hexId 4
                         count 2 $ char ' '
                         name <- restOfLine
                         return (pid, utf8BS name)

      classSection :: BSParser ClassDB
      classSection = do xs <- lexeme $ many classParser
                        return $ MP.fromList xs

      classParser :: BSParser (ClassID, (ClassName, SubClassDB))
      classParser = do char 'C'
                       char ' '
                       cid  <- hexId 2
                       count 2 $ char ' '
                       name <- restOfLine
                       subClasses <- many subClassParser
                       return ( cid
                              , (utf8BS name, MP.fromList subClasses)
                              )

      subClassParser :: BSParser (SubClassID, (SubClassName, ProtocolDB))
      subClassParser = do tab
                          scid <- hexId 2
                          count 2 $ char ' '
                          name <- restOfLine
                          protocols <- many (try protocolParser)
                          return ( scid
                                 , (utf8BS name, MP.fromList protocols)
                                 )

      protocolParser :: BSParser (ProtocolID, ProtocolName)
      protocolParser = do count 2 tab
                          protId <- hexId 2
                          count 2 $ char ' '
                          name <- restOfLine
                          return (protId, utf8BS name)

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
