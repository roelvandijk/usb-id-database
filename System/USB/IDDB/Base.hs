{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE UnicodeSyntax     #-}

module System.USB.IDDB.Base
    ( IDDB(..)

    , ProductDB
    , ClassDB, SubClassDB, ProtocolDB

    , emptyDb

    , vendorName
    , vendorId
    , productName
    , productId
    , className
    , subClassName
    , protocolName
    , audioClassTerminalTypeName
    , videoClassTerminalTypeName
    , hidDescTypeName
    , hidDescItemName
    , hidDescCountryCodeName
    , hidUsagePageName
    , hidUsageName
    , physicalDescBiasName
    , physicalDescItemName
    , langName
    , subLangName

    , getDataFileName
    )
    where

import "base" Control.Monad ( (=<<), fmap )
import "base" Data.Function ( ($) )
import "base" Data.Int      ( Int )
import "base" Data.Maybe    ( Maybe )
import "base" Data.Tuple    ( fst, snd )
import "base" Prelude       ( String )
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import qualified "containers" Data.IntMap as IM
import qualified "containers" Data.Map    as MP
import qualified "containers-unicode-symbols" Data.IntMap.Unicode as IM ( (∅) )
import qualified "containers-unicode-symbols" Data.Map.Unicode    as MP ( (∅) )

#ifdef BUILD_WITH_CABAL
import "this" Paths_usb_id_database ( getDataFileName )
#else
import "base" Control.Monad ( return )
import "base" System.IO     ( IO, FilePath )
getDataFileName ∷ FilePath → IO FilePath
getDataFileName = return
#endif

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type ProductDB  = ( MP.Map String Int
                  , IM.IntMap String
                  )
type ClassDB    = IM.IntMap (String, SubClassDB)
type SubClassDB = IM.IntMap (String, ProtocolDB)
type ProtocolDB = IM.IntMap String

-- |A database of USB identifiers. Contains both vendor identifiers and product
-- identifiers.
data IDDB = IDDB { dbVendorNameId ∷ MP.Map String Int
                 , dbVendorIdName ∷ IM.IntMap String
                 , dbProducts     ∷ IM.IntMap ProductDB
                 , dbClasses      ∷ ClassDB
                 , dbAudioCTType  ∷ IM.IntMap String
                 , dbVideoCTType  ∷ IM.IntMap String
                 , dbHIDDescType  ∷ IM.IntMap String
                 , dbHIDDescItem  ∷ IM.IntMap String
                 , dbHIDDescCCode ∷ IM.IntMap String
                 , dbHIDUsage     ∷ IM.IntMap (String, IM.IntMap String)
                 , dbPhysDescBias ∷ IM.IntMap String
                 , dbPhysDescItem ∷ IM.IntMap String
                 , dbLanguages    ∷ IM.IntMap (String, IM.IntMap String)
                 }

-- |An empty database.
emptyDb ∷ IDDB
emptyDb = IDDB { dbVendorNameId = (MP.∅)
               , dbVendorIdName = (IM.∅)
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

-------------------------------------------------------------------------------
-- Query database
-------------------------------------------------------------------------------

vendorName ∷ IDDB -- ^Database
           → Int  -- ^Vendor identifier
           → Maybe String
vendorName db vid = IM.lookup vid $ dbVendorIdName db

vendorId ∷ IDDB   -- ^Database
         → String -- ^Vendor name
         → Maybe Int
vendorId db name = MP.lookup name $ dbVendorNameId db

productName ∷ IDDB -- ^Database
            → Int  -- ^Vendor identifier
            → Int  -- ^Product identifier
            → Maybe String
productName db vid pid = IM.lookup pid ∘ snd =<< IM.lookup vid (dbProducts db)

productId ∷ IDDB   -- ^Database
          → Int    -- ^Vendor identifier
          → String -- ^Product name
          → Maybe Int
productId db vid name = MP.lookup name ∘ fst =<< IM.lookup vid (dbProducts db)

className ∷ IDDB -- ^Database
          → Int  -- ^Class identifier
          → Maybe String
className db cid = fmap fst ∘ IM.lookup cid $ dbClasses db

subClassName ∷ IDDB -- ^Database
             → Int  -- ^Class identifier
             → Int  -- ^Sub class identifier
             → Maybe String
subClassName db cid scid = fmap fst $   IM.lookup scid ∘ snd
                                    =<< IM.lookup cid (dbClasses db)

protocolName ∷ IDDB -- ^Database
             → Int  -- ^Class identifier
             → Int  -- ^Sub class identifier
             → Int  -- ^Protocol identifier
             → Maybe String
protocolName db cid scid protId =   IM.lookup protId ∘ snd
                                =<< IM.lookup scid   ∘ snd
                                =<< IM.lookup cid (dbClasses db)

audioClassTerminalTypeName ∷ IDDB -- ^Database
                           → Int  -- ^Audio class terminal type identifier
                           → Maybe String
audioClassTerminalTypeName db actid = IM.lookup actid (dbAudioCTType db)

videoClassTerminalTypeName ∷ IDDB -- ^Database
                           → Int  -- ^Video class terminal type identifier
                           → Maybe String
videoClassTerminalTypeName db actid = IM.lookup actid (dbVideoCTType db)

hidDescTypeName ∷ IDDB -- ^Database
                → Int  -- ^HID descriptor type identifier
                → Maybe String
hidDescTypeName db hidid = IM.lookup hidid (dbHIDDescType db)

hidDescItemName ∷ IDDB -- ^Database
                → Int  -- ^HID descriptor item identifier
                → Maybe String
hidDescItemName db hidid = IM.lookup hidid (dbHIDDescItem db)

hidDescCountryCodeName ∷ IDDB -- ^Database
                       → Int  -- ^HID descriptor country code identifier
                       → Maybe String
hidDescCountryCodeName db hidid = IM.lookup hidid (dbHIDDescCCode db)

hidUsagePageName ∷ IDDB -- ^Database
                 → Int  -- ^HID usage page identifier
                 → Maybe String
hidUsagePageName db upid = fmap fst $ IM.lookup upid (dbHIDUsage db)

hidUsageName ∷ IDDB -- ^Database
             → Int  -- ^HID usage page identifier
             → Int  -- ^HID usage identifier
             → Maybe String
hidUsageName db upid uid =   IM.lookup uid ∘ snd
                         =<< IM.lookup upid (dbHIDUsage db)

physicalDescBiasName ∷ IDDB -- ^Database
                     → Int  -- ^Physical descriptor bias identifier
                     → Maybe String
physicalDescBiasName db phyid = IM.lookup phyid (dbPhysDescBias db)

physicalDescItemName ∷ IDDB -- ^Database
                     → Int  -- ^Physical descriptor item identifier
                     → Maybe String
physicalDescItemName db phyid = IM.lookup phyid (dbPhysDescItem db)

langName ∷ IDDB -- ^Database
         → Int  -- ^Primary language identifier
         → Maybe String
langName db lid = fmap fst ∘ IM.lookup lid $ dbLanguages db

subLangName ∷ IDDB -- ^Database
            → Int  -- ^Primary language identifier
            → Int  -- ^Sub language identifier
            → Maybe String
subLangName db lid slid =   IM.lookup slid ∘ snd
                        =<< IM.lookup lid (dbLanguages db)
