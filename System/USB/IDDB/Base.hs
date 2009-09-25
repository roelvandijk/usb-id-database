{-# LANGUAGE CPP #-}

module System.USB.IDDB.Base
    ( IDDB(..)

    , ID,         Name
    , VendorID,   VendorName
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

import Data.Binary (Binary(..), Get)

import qualified Data.IntMap as IM
import qualified Data.Map    as MP

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
type Name        = String

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

type ProductDB  = ( MP.Map ProductName ProductID
                  , IM.IntMap ProductName
                  )
type ClassDB    = IM.IntMap (ClassName, SubClassDB)
type SubClassDB = IM.IntMap (SubClassName, ProtocolDB)
type ProtocolDB = IM.IntMap ProtocolName

-- |A database of USB identifiers. Contains both vendor identifiers
-- and product identifiers.
data IDDB = IDDB { dbVendorNameId :: MP.Map VendorName VendorID
                 , dbVendorIdName :: IM.IntMap VendorName
                 , dbProducts     :: IM.IntMap ProductDB
                 , dbClasses      :: ClassDB
                 }

-- |An empty database.
emptyDb :: IDDB
emptyDb = IDDB { dbVendorNameId = MP.empty
               , dbVendorIdName = IM.empty
               , dbProducts     = IM.empty
               , dbClasses      = IM.empty
               }

-------------------------------------------------------------------------------
-- Binary serialisation
-------------------------------------------------------------------------------

instance Binary IDDB where
    put db = put ( dbVendorNameId db
                 , dbVendorIdName db
                 , dbProducts     db
                 , dbClasses      db
                 )

    get = do (a, b, c, d) <- get :: Get ( MP.Map VendorName VendorID
                                        , IM.IntMap VendorName
                                        , IM.IntMap ProductDB
                                        , ClassDB
                                        )
             return IDDB { dbVendorNameId = a
                         , dbVendorIdName = b
                         , dbProducts     = c
                         , dbClasses      = d
                         }

-------------------------------------------------------------------------------
-- Query database
-------------------------------------------------------------------------------

vendorName :: IDDB -> VendorID -> Maybe VendorName
vendorName db vid = IM.lookup vid $ dbVendorIdName db

vendorId :: IDDB -> VendorName -> Maybe VendorID
vendorId db name = MP.lookup name $ dbVendorNameId db

productName :: IDDB -> VendorID -> ProductID -> Maybe ProductName
productName db vid pid = IM.lookup pid . snd =<< IM.lookup vid (dbProducts db)

productId :: IDDB -> VendorID -> ProductName -> Maybe ProductID
productId db vid name = MP.lookup name . fst =<< IM.lookup vid (dbProducts db)

className :: IDDB -> ClassID -> Maybe ClassName
className db cid = fmap fst . IM.lookup cid $ dbClasses db

subClassName :: IDDB -> ClassID -> SubClassID -> Maybe SubClassName
subClassName db cid scid = fmap fst $   IM.lookup scid . snd
                                    =<< IM.lookup cid (dbClasses db)

protocolName :: IDDB -> ClassID -> SubClassID -> ProtocolID -> Maybe ProtocolName
protocolName db cid scid protId =   IM.lookup protId . snd
                                =<< IM.lookup scid   . snd
                                =<< IM.lookup cid (dbClasses db)


