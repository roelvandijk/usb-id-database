module System.USB.IDDB.LinuxUsbIdRepo
    ( parseDb
    , staticDb
    , fromFile
    , fromWeb
    ) where

import Control.Monad        (fmap)
import Data.Maybe           (fromJust)
import Network.Download     (openURIString)
import Numeric              (readHex)
import Parsimony
import Parsimony.Char       (char, hexDigit, spaces, tab)
import System.IO            (FilePath, readFile)
import System.USB.IDDB.Base
import System.USB.IDDB.Misc (eitherMaybe, swap, restOfLine)

import qualified Data.IntMap as IM (IntMap, fromList)
import qualified Data.Map    as MP (Map,    fromList)


-- |Construct a database from a string in the format used by
-- <http://linux-usb.org>.
parseDb :: String -> Maybe IDDB
parseDb = eitherMaybe . parse dbParser

dbParser :: Parser String IDDB
dbParser = do spaces
              comments
              (vendorNameId, vendorIdName, productDB) <- lexeme vendorSection
              comments
              classDB <- classSection

              return IDDB { dbVendorNameId = vendorNameId
                          , dbVendorIdName = vendorIdName
                          , dbProducts     = productDB
                          , dbClasses      = classDB
                          }
    where
      lexeme :: Parser String a -> Parser String a
      lexeme p = do x <- p
                    spaces
                    return x

      comment :: Parser String String
      comment = char '#' >> restOfLine

      comments :: Parser String [String]
      comments = many $ lexeme comment

      hexId :: Num n => Int -> Parser String n
      hexId d = do ds <- count d hexDigit
                   case readHex ds of
                     [(n, _)]  -> return n
                     _         -> error "impossible"

      vendorSection :: Parser String ( MP.Map VendorName VendorID
                                     , IM.IntMap VendorName
                                     , IM.IntMap ProductDB
                                     )
      vendorSection = do xs <- lexeme $ many vendorParser
                         return ( MP.fromList [(name, vid) | (vid, name, _)   <- xs]
                                , IM.fromList [(vid, name) | (vid, name, _)   <- xs]
                                , IM.fromList [(vid, pdb)  | (vid, _,    pdb) <- xs]
                                )

      vendorParser :: Parser String (VendorID, VendorName, ProductDB)
      vendorParser = do vid  <- hexId 4
                        count 2 $ char ' '
                        name <- restOfLine
                        products <- many productParser
                        return ( vid
                               , name
                               , ( MP.fromList $ fmap swap products
                                 , IM.fromList products
                                 )
                               )

      productParser :: Parser String (ProductID, ProductName)
      productParser = do tab
                         pid  <- hexId 4
                         count 2 $ char ' '
                         name <- restOfLine
                         return (pid, name)

      classSection :: Parser String ClassDB
      classSection = do xs <- lexeme $ many classParser
                        return $ IM.fromList xs

      classParser :: Parser String (ClassID, (ClassName, SubClassDB))
      classParser = do char 'C'
                       char ' '
                       cid  <- hexId 2
                       count 2 $ char ' '
                       name <- restOfLine
                       subClasses <- many subClassParser
                       return ( cid
                              , (name, IM.fromList subClasses)
                              )

      subClassParser :: Parser String (SubClassID, (SubClassName, ProtocolDB))
      subClassParser = do tab
                          scid <- hexId 2
                          count 2 $ char ' '
                          name <- restOfLine
                          protocols <- many (try protocolParser)
                          return ( scid
                                 , (name, IM.fromList protocols)
                                 )

      protocolParser :: Parser String (ProtocolID, ProtocolName)
      protocolParser = do count 2 tab
                          protId <- hexId 2
                          count 2 $ char ' '
                          name <- restOfLine
                          return (protId, name)

-- |Construct a database from the data available at
-- <http://linux-usb.org/usb.ids>.
fromWeb :: IO (Maybe IDDB)
fromWeb = fmap ( either (const Nothing)
                        parseDb
               ) $ openURIString dbURL

-- |Load a vendor database from file. If the file can not be read for
-- some reason an error will be thrown.
fromFile :: FilePath -> IO (Maybe IDDB)
fromFile = fmap parseDb . readFile

-- |Load a database from a snapshot of the linux-usb.org database which is
--  supplied with the package.
staticDb :: IO IDDB
staticDb = getDataFileName staticDbPath >>= fmap fromJust . fromFile

staticDbPath :: FilePath
staticDbPath = "usb_id_repo_db.txt"

dbURL :: String
dbURL = "http://linux-usb.org/usb.ids"
