{-# LANGUAGE CPP
           , NoImplicitPrelude
           , UnicodeSyntax
  #-}

{-| Functions to acquire a database from <http://linux-usb.org>. -}

module System.USB.IDDB.LinuxUsbIdRepo
    ( -- * Parsing
      parseDb
      -- * Acquiring a database
    , staticDb
    , fromFile
    , dbURL
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- base
import Control.Arrow         ( second )
import Control.Monad         ( (>>=), (>>), fmap, return )
import Data.Bool             ( Bool, not )
import Data.Char             ( isSpace )
import Data.Eq               ( Eq )
import Data.Function         ( ($), id )
import Data.Int              ( Int )
import Data.List             ( all, filter, length, map
                             , isPrefixOf, lines, unlines
                             )
import Data.Maybe            ( Maybe, fromJust )
import Data.Tuple            ( fst )
import Numeric               ( readHex )
import Prelude               ( String, Num, error, seq )
import System.IO             ( IO, FilePath )
#if MIN_VERSION_base(4,2,0)
import System.IO             ( IOMode(ReadMode)
                             , withFile, hSetEncoding, latin1, hGetContents
                             )
#else
import System.IO             ( readFile )
#endif

#if __GLASGOW_HASKELL__ < 700
import Control.Monad ( fail )
import Prelude       ( fromInteger )
#endif

-- base-unicode-symbols
import Data.Bool.Unicode     ( (∧) )
import Data.Function.Unicode ( (∘) )

-- containers
import qualified Data.IntMap as IM
import qualified Data.Map    as MP

-- parsimony
import Parsimony
import Parsimony.Char        ( char, string, hexDigit, tab )

-- usb-id-database
import System.USB.IDDB.Base
import System.USB.IDDB.Misc  ( eitherMaybe, swap, restOfLine )


-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

-- |Construct a database from a string in the format used by
-- <http://linux-usb.org>.
parseDb ∷ String → Maybe IDDB
parseDb = eitherMaybe ∘ parse dbParser ∘ stripBoring

-- |Remove comments and empty lines.
stripBoring ∷ String → String
stripBoring = unlines
            ∘ filter (\xs → not (isComment xs) ∧ not (isEmpty xs))
            ∘ lines

isComment ∷ String → Bool
isComment = isPrefixOf "#"

isEmpty ∷ String → Bool
isEmpty = all isSpace

dbParser ∷ Parser String IDDB
dbParser = do (vendorNameId, vendorIdName, productDB) ← vendorSection
              classDB ← genericSection (label "C") 2 id
                         ∘ genericSection tab 2 id
                           ∘ genericSection (count 2 tab) 2 fst
                             $ return ()
              at      ← simpleSection "AT" 4
              hid     ← simpleSection "HID" 2
              r       ← simpleSection "R" 2
              bias    ← simpleSection "BIAS" 1
              phy     ← simpleSection "PHY" 2
              hut     ← genericSection (label "HUT") 2 id
                         ∘ genericSection tab 3 fst
                           $ return ()
              l       ← genericSection (label "L") 4 id
                         ∘ genericSection tab 2 fst
                           $ return ()
              hcc     ← simpleSection "HCC" 2
              vt      ← simpleSection "VT" 4

              return IDDB { dbVendorNameId = vendorNameId
                          , dbVendorIdName = vendorIdName
                          , dbProducts     = productDB
                          , dbClasses      = classDB
                          , dbAudioCTType  = at
                          , dbVideoCTType  = vt
                          , dbHIDDescType  = hid
                          , dbHIDDescItem  = r
                          , dbHIDDescCCode = hcc
                          , dbHIDUsage     = hut
                          , dbPhysDescBias = bias
                          , dbPhysDescItem = phy
                          , dbLanguages    = l
                          }
    where
      hexId ∷ (Eq n, Num n) ⇒ Int → Parser String n
      hexId d = do ds ← count d hexDigit
                   case readHex ds of
                     [(n, _)]  → return n
                     _         → error "impossible"

      label ∷ String → Parser String ()
      label n = string n >> char ' ' >> return ()

      -- Top level section without subsections.
      simpleSection ∷ String → Int → Parser String (IM.IntMap String)
      simpleSection sym idSize = genericSection (string sym >> char ' ')
                                                idSize fst $ return ()

      genericSection ∷ (Parser String p)
                     → Int
                     → ((String, s) → r)
                     → Parser String s
                     → Parser String (IM.IntMap r)
      genericSection prefix idSize convert =
          fmap (IM.fromList ∘ map (second convert))
          ∘ many ∘ try ∘ genericItem prefix idSize

      genericItem ∷ (Parser String p)
                  → Int
                  → Parser String s
                  → Parser String (Int, (String, s))
      genericItem prefix idSize sub = do
          _        ← prefix
          itemId   ← hexId idSize
          _        ← count 2 $ char ' '
          itemName ← restOfLine
          s        ← sub
          return (itemId, (itemName, s))

      vendorSection ∷ Parser String ( MP.Map String Int
                                     , IM.IntMap String
                                     , IM.IntMap ProductDB
                                     )
      vendorSection = do
        xs ← many (try (vendorParser <?> "vendor"))
        return ( MP.fromList [(name, vid) | (vid, name, _)   ← xs]
               , IM.fromList [(vid, name) | (vid, name, _)   ← xs]
               , IM.fromList [(vid, pdb)  | (vid, _,    pdb) ← xs]
               )

      vendorParser ∷ Parser String (Int, String, ProductDB)
      vendorParser = do
        vid      ← hexId 4
        _        ← count 2 $ char ' '
        name     ← restOfLine
        products ← many (productParser <?> "product")
        return ( vid
               , name
               , ( MP.fromList $ fmap swap products
                 , IM.fromList products
                 )
               )

      productParser ∷ Parser String (Int, String)
      productParser = do _    ← tab
                         pid  ← hexId 4
                         _    ← count 2 $ char ' '
                         name ← restOfLine
                         return (pid, name)


-------------------------------------------------------------------------------
-- Acquiring a database
-------------------------------------------------------------------------------

-- |Load a database from file. If the file can not be read for some reason an
-- error will be thrown.
fromFile ∷ FilePath → IO (Maybe IDDB)
#if MIN_VERSION_base(4,2,0)
fromFile fp = withFile fp ReadMode
              $ \h → do hSetEncoding h latin1
                        contents ← hGetContents h
                        -- Bit ugly, but necessary to force the
                        -- evaluation of contents before it is parsed
                        -- as a database. Otherwise you'll get an
                        -- empty database.
                        length contents `seq` (return $ parseDb contents)
#else
fromFile = fmap parseDb ∘ readFile
#endif

-- |Load a database from a snapshot of the linux-usb.org database which is
-- supplied with the package.
staticDb ∷ IO IDDB
staticDb = getDataFileName staticDbPath >>= fmap fromJust ∘ fromFile
    where
      staticDbPath ∷ FilePath
      staticDbPath = "usb_id_repo_db.txt"

-- |<http://linux-usb.org/usb.ids>
--
-- The source of the database. Download this file for the most up-to-date
-- version.
dbURL ∷ String
dbURL = "http://linux-usb.org/usb.ids"
