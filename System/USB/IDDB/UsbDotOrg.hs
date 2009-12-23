{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax #-}

{-| Functions to acquire a database from <http://www.usb.org>. -}

module System.USB.IDDB.UsbDotOrg
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
import Control.Monad        ( (>>=), fail, fmap, return )
import Data.Char            ( String )
import Data.Function        ( ($) )
import Data.Int             ( Int )
import Data.Maybe           ( Maybe, fromJust )
import System.IO            ( IO, FilePath, readFile )
import Text.Read            ( read )

-- base-unicode-symbols
import Prelude.Unicode      ( (∘) )

-- containers
import qualified Data.IntMap as IM ( fromList )
import qualified Data.Map    as MP ( fromList )

-- containers-unicode-symbols
import qualified Data.IntMap.Unicode as IM ( (∅) )

-- parsimony
import Parsimony
import Parsimony.Char       ( char, digit )

-- usb-id-database
import System.USB.IDDB.Base ( IDDB(..), getDataFileName )
import System.USB.IDDB.Misc ( eitherMaybe, swap, restOfLine )


-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

-- |Construct a database from a string in the format used by usb.org.
parseDb ∷ String → Maybe IDDB
parseDb = eitherMaybe ∘ parse staticDbParser

staticDbParser ∷ Parser String IDDB
staticDbParser = do
  vendors ← many vendorParser
  return IDDB { dbVendorNameId = MP.fromList $ fmap swap vendors
              , dbVendorIdName = IM.fromList vendors
              , dbProducts     = (IM.∅)
              , dbClasses      = (IM.∅)
              , dbAudioCTType  = (IM.∅)
              , dbVideoCTType  = (IM.∅)
              , dbHIDDescType  = (IM.∅)
              , dbHIDDescItem  = (IM.∅)
              , dbHIDDescCCode = (IM.∅)
              , dbHIDUsage     = (IM.∅)
              , dbPhysDescBias = (IM.∅)
              , dbPhysDescItem = (IM.∅)
              , dbLanguages    = (IM.∅)
              }
    where
      vendorParser ∷ Parser String (Int, String)
      vendorParser = do vid  ← many1 digit
                        _    ← char '|'
                        name ← restOfLine
                        return (read vid, name)


-------------------------------------------------------------------------------
-- Acquiring a database
-------------------------------------------------------------------------------

-- |Load a vendor database from file. If the file can not be read for
--  some reason an error will be thrown.
fromFile ∷ FilePath → IO (Maybe IDDB)
fromFile = fmap parseDb ∘ readFile

staticDbPath ∷ FilePath
staticDbPath = "usb_dot_org_db.txt"

-- |Load a database from a snapshot of the usb.org database which is
--  supplied with the package.
staticDb ∷ IO IDDB
staticDb = getDataFileName staticDbPath >>= fmap fromJust ∘ fromFile

-- |<http://www.usb.org/developers/tools/comp_dump>
--
-- The source of the database. Download this file for the most up-to-date
-- version.
dbURL ∷ String
dbURL = "http://www.usb.org/developers/tools/comp_dump"

