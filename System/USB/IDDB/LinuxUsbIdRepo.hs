module System.USB.IDDB.LinuxUsbIdRepo
    ( parseDb
    , staticDb
    , fromFile
    , dbURL
    ) where

import Control.Arrow        ( second )
import Control.Monad        ( fmap )
import Data.Char            ( isSpace )
import Data.List            ( lines, unlines, isPrefixOf )
import Data.Maybe           ( fromJust )
import Numeric              ( readHex )
import Parsimony
import Parsimony.Char       ( char, string, hexDigit, tab )
import System.IO            ( FilePath, readFile )
import System.USB.IDDB.Base
import System.USB.IDDB.Misc ( eitherMaybe, swap, restOfLine )

import qualified Data.IntMap as IM
import qualified Data.Map    as MP

-- |Construct a database from a string in the format used by
-- <http://linux-usb.org>.
parseDb :: String -> Maybe IDDB
parseDb = eitherMaybe . parse dbParser . stripBoring

-- |Remove comments and empty lines.
stripBoring :: String -> String
stripBoring = unlines
            . filter (\xs -> not (isComment xs) && not (isEmpty xs))
            . lines
    where
      isComment :: String -> Bool
      isComment = isPrefixOf "#"

      isEmpty :: String -> Bool
      isEmpty = all isSpace

dbParser :: Parser String IDDB
dbParser = do (vendorNameId, vendorIdName, productDB) <- vendorSection
              classDB <- genericSection (label "C") 2 id
                         . genericSection tab 2 id
                           . genericSection (count 2 tab) 2 fst
                             $ return ()
              actDB   <- simpleSection "AT" 4
              _       <- simpleSection "HID" 2
              _       <- simpleSection "R" 2
              _       <- simpleSection "BIAS" 1
              _       <- simpleSection "PHY" 2
              _       <- genericSection (label "HUT") 2 id
                         . genericSection tab 3 fst
                           $ return ()
              langDB  <- genericSection (label "L") 4 id
                         . genericSection tab 2 fst
                           $ return ()
              _       <- simpleSection "HCC" 2
              _       <- simpleSection "VT" 4

              return IDDB { dbVendorNameId = vendorNameId
                          , dbVendorIdName = vendorIdName
                          , dbProducts     = productDB
                          , dbClasses      = classDB
                          , dbACT          = actDB
                          , dbLanguages    = langDB
                          }
    where
      hexId :: Num n => Int -> Parser String n
      hexId d = do ds <- count d hexDigit
                   case readHex ds of
                     [(n, _)]  -> return n
                     _         -> error "impossible"

      label :: String -> Parser String ()
      label n = string n >> char ' ' >> return ()

      -- Top level section without subsections.
      simpleSection :: String -> Int -> Parser String (IM.IntMap String)
      simpleSection sym idSize = genericSection (string sym >> char ' ')
                                                idSize fst $ return ()

      genericSection :: (Parser String p)
                     -> Int
                     -> ((String, s) -> r)
                     -> Parser String s
                     -> Parser String (IM.IntMap r)
      genericSection prefix idSize convert =
          fmap (IM.fromList . map (second convert))
          . many . try . genericItem prefix idSize

      genericItem :: (Parser String p)
                  -> Int
                  -> Parser String s
                  -> Parser String (Int, (String, s))
      genericItem prefix idSize sub = do
          _        <- prefix
          itemId   <- hexId idSize
          _        <- count 2 $ char ' '
          itemName <- restOfLine
          s        <- sub
          return (itemId, (itemName, s))

      vendorSection :: Parser String ( MP.Map String Int
                                     , IM.IntMap String
                                     , IM.IntMap ProductDB
                                     )
      vendorSection = do
        xs <- many (try (vendorParser <?> "vendor"))
        return ( MP.fromList [(name, vid) | (vid, name, _)   <- xs]
               , IM.fromList [(vid, name) | (vid, name, _)   <- xs]
               , IM.fromList [(vid, pdb)  | (vid, _,    pdb) <- xs]
               )

      vendorParser :: Parser String (Int, String, ProductDB)
      vendorParser = do
        vid      <- hexId 4
        _        <- count 2 $ char ' '
        name     <- restOfLine
        products <- many (productParser <?> "product")
        return ( vid
               , name
               , ( MP.fromList $ fmap swap products
                 , IM.fromList products
                 )
               )

      productParser :: Parser String (Int, String)
      productParser = do _    <- tab
                         pid  <- hexId 4
                         _    <- count 2 $ char ' '
                         name <- restOfLine
                         return (pid, name)

-- |Load a vendor database from file. If the file can not be read for some
-- reason an error will be thrown.
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
