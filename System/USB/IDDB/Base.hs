{-# LANGUAGE CPP #-}

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
    , langName
    , subLangName

    , getDataFileName
    )
    where

import Data.Binary ( Binary(..), Get )

import qualified Data.IntMap as IM
import qualified Data.Map    as MP

#ifdef BUILD_WITH_CABAL
import Paths_usb_id_database ( getDataFileName )
#else
getDataFileName :: FilePath -> IO FilePath
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
type LanguageDB = IM.IntMap (String, IM.IntMap String)

-- |A database of USB identifiers. Contains both vendor identifiers
-- and product identifiers.
data IDDB = IDDB { dbVendorNameId :: MP.Map String Int
                 , dbVendorIdName :: IM.IntMap String
                 , dbProducts     :: IM.IntMap ProductDB
                 , dbClasses      :: ClassDB
                 , dbACT          :: IM.IntMap String
                 , dbLanguages    :: LanguageDB
                 }

-- |An empty database.
emptyDb :: IDDB
emptyDb = IDDB { dbVendorNameId = MP.empty
               , dbVendorIdName = IM.empty
               , dbProducts     = IM.empty
               , dbClasses      = IM.empty
               , dbACT          = IM.empty
               , dbLanguages    = IM.empty
               }

-------------------------------------------------------------------------------
-- Binary serialisation
-------------------------------------------------------------------------------

instance Binary IDDB where
    put db = put ( dbVendorNameId db
                 , dbVendorIdName db
                 , dbProducts     db
                 , dbClasses      db
                 , dbACT          db
                 , dbLanguages    db
                 )

    get = do (a, b, c, d, e, f) <- get'
             return IDDB { dbVendorNameId = a
                         , dbVendorIdName = b
                         , dbProducts     = c
                         , dbClasses      = d
                         , dbACT          = e
                         , dbLanguages    = f
                         }
        where get' :: Get ( MP.Map String Int
                          , IM.IntMap String
                          , IM.IntMap ProductDB
                          , ClassDB
                          , IM.IntMap String
                          , LanguageDB
                          )
              get' = get

-------------------------------------------------------------------------------
-- Query database
-------------------------------------------------------------------------------

vendorName :: IDDB
           -> Int -- ^Vendor Id
           -> Maybe String
vendorName db vid = IM.lookup vid $ dbVendorIdName db

vendorId :: IDDB
         -> String -- ^Vendor name
         -> Maybe Int
vendorId db name = MP.lookup name $ dbVendorNameId db

productName :: IDDB
            -> Int -- ^Vendor Id
            -> Int -- ^Product Id
            -> Maybe String
productName db vid pid = IM.lookup pid . snd =<< IM.lookup vid (dbProducts db)

productId :: IDDB
          -> Int    -- ^Vendor Id
          -> String -- ^Product name
          -> Maybe Int
productId db vid name = MP.lookup name . fst =<< IM.lookup vid (dbProducts db)

className :: IDDB
          -> Int -- ^Class Id
          -> Maybe String
className db cid = fmap fst . IM.lookup cid $ dbClasses db

subClassName :: IDDB
             -> Int -- ^Class Id
             -> Int -- ^Sub class Id
             -> Maybe String
subClassName db cid scid = fmap fst $   IM.lookup scid . snd
                                    =<< IM.lookup cid (dbClasses db)

protocolName :: IDDB
             -> Int -- ^Class Id
             -> Int -- ^Sub class Id
             -> Int -- ^Protocol Id
             -> Maybe String
protocolName db cid scid protId =   IM.lookup protId . snd
                                =<< IM.lookup scid   . snd
                                =<< IM.lookup cid (dbClasses db)

audioClassTerminalTypeName :: IDDB
                           -> Int -- ^Audio class terminal type Id
                           -> Maybe String
audioClassTerminalTypeName db actid = IM.lookup actid (dbACT db)

langName :: IDDB
         -> Int -- ^Primary language Id
         -> Maybe String
langName db lid = fmap fst . IM.lookup lid $ dbLanguages db

subLangName :: IDDB
            -> Int -- ^Primary language Id
            -> Int -- ^Sub language Id
            -> Maybe String
subLangName db lid slid =   IM.lookup slid . snd
                        =<< IM.lookup lid (dbLanguages db)
