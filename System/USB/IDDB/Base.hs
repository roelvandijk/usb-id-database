{-# LANGUAGE CPP #-}

module System.USB.IDDB.Base
    ( IDDB(..)

    , VendorID,   VendorName,   VendorDB
    , ProductID,  ProductName,  ProductDB
    , ClassID,    ClassName,    ClassDB
    , SubClassID, SubClassName, SubClassDB
    , ProtocolID, ProtocolName, ProtocolDB

    , emptyDb

    , vendorName
    , vendorId
    , productName
    , productId
    , className
    , subClassName
    , protocolName

    , getDataFileName
    )
    where

import Data.ByteString (ByteString)

import qualified Data.Bimap as BM
import qualified Data.Map   as MP

#ifdef BUILD_WITH_CABAL
import Paths_usb_id_database (getDataFileName)
#else
getDataFileName :: FilePath -> IO FilePath
getDataFileName = return
#endif

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type ID          = Int
type Name        = ByteString

type VendorID    = ID
type ProductID   = ID
type ClassID     = ID
type SubClassID  = ID
type ProtocolID  = ID

type VendorName   = Name
type ProductName  = Name
type ClassName    = Name
type SubClassName = Name
type ProtocolName = Name

type VendorDB   = BM.Bimap VendorID VendorName
type ProductDB  = BM.Bimap ProductID ProductName
type ClassDB    = MP.Map ClassID (ClassName, SubClassDB)
type SubClassDB = MP.Map SubClassID (SubClassName, ProtocolDB)
type ProtocolDB = MP.Map ProtocolID ProtocolName

-- |A database of USB identifiers. Contains both vendor identifiers
-- and product identifiers.
data IDDB = IDDB { dbVendors  :: VendorDB
                 , dbProducts :: MP.Map VendorID ProductDB
                 , dbClasses  :: ClassDB
                 }

-- |An empty database./
emptyDb :: IDDB
emptyDb = IDDB { dbVendors  = BM.empty
               , dbProducts = MP.empty
               , dbClasses  = MP.empty
               }

-------------------------------------------------------------------------------
-- Query database
-------------------------------------------------------------------------------

vendorName :: IDDB -> VendorID -> Maybe VendorName
vendorName db vid = BM.lookup vid $ dbVendors db

vendorId :: IDDB -> VendorName -> Maybe VendorID
vendorId db name = BM.lookupR name $ dbVendors db

productName :: IDDB -> VendorID -> ProductID -> Maybe ProductName
productName db vid pid = BM.lookup pid =<< MP.lookup vid (dbProducts db)

productId :: IDDB -> VendorID -> ProductName -> Maybe ProductID
productId db vid name = BM.lookupR name =<< MP.lookup vid (dbProducts db)

className :: IDDB -> ClassID -> Maybe ClassName
className db cid = fmap fst $ MP.lookup cid $ dbClasses db

subClassName :: IDDB -> ClassID -> SubClassID -> Maybe SubClassName
subClassName db cid scid = fmap fst $   MP.lookup scid . snd
                                    =<< MP.lookup cid (dbClasses db)

protocolName :: IDDB -> ClassID -> SubClassID -> ProtocolID -> Maybe ProtocolName
protocolName db cid scid protId =   MP.lookup protId . snd
                                =<< MP.lookup scid   . snd
                                =<< MP.lookup cid (dbClasses db)


