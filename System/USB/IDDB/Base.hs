module System.USB.IDDB.Base
    ( IDDB(..)
    , VendorID, VendorName
    , ProductID, ProductName

    , emptyDb

    , vendorName
    , vendorId
    , productName
    , productId

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

type VendorID  = Int
type ProductID = Int

type VendorName  = ByteString
type ProductName = ByteString


-- |A database of USB identifiers. Contains both vendor identifiers
-- and product identifiers.
data IDDB = IDDB { dbVendors  :: BM.Bimap VendorID VendorName
                 , dbProducts :: MP.Map   VendorID (BM.Bimap ProductID ProductName)
                 }

-- |An empty database.
emptyDb :: IDDB
emptyDb = IDDB { dbVendors  = BM.empty
               , dbProducts = MP.empty
               }

-------------------------------------------------------------------------------
-- Query database
-------------------------------------------------------------------------------

vendorName :: IDDB -> VendorID -> Maybe VendorName
vendorName db vid = BM.lookup vid (dbVendors db)

vendorId :: IDDB -> VendorName -> Maybe VendorID
vendorId db name = BM.lookupR name (dbVendors db)

productName :: IDDB -> VendorID -> ProductID -> Maybe ProductName
productName db vid pid = BM.lookup pid =<< MP.lookup vid (dbProducts db)

productId :: IDDB -> VendorID -> ProductName -> Maybe ProductID
productId db vid name = BM.lookupR name =<< MP.lookup vid (dbProducts db)
