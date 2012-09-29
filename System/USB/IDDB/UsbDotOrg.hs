{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

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

import "base" Control.Monad ( (>>=), fmap, return )
import "base" Data.Function ( ($) )
import "base" Data.Int      ( Int )
import "base" Data.Maybe    ( Maybe, fromJust )
import "base" Prelude       ( String )
import "base" System.IO     ( IO, FilePath, readFile )
import "base" Text.Read     ( read )
#if __GLASGOW_HASKELL__ < 700
import "base" Control.Monad ( fail )
#endif
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import qualified "containers" Data.IntMap as IM ( fromList )
import qualified "containers" Data.Map    as MP ( fromList )
import qualified "containers-unicode-symbols" Data.IntMap.Unicode as IM ( (∅) )
import "parsimony" Parsimony
import "parsimony" Parsimony.Char ( char, digit )
import "this" System.USB.IDDB.Base ( IDDB(..), getDataFileName )
import "this" System.USB.IDDB.Misc ( eitherMaybe, swap, restOfLine )


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

