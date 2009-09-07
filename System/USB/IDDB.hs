{-# LANGUAGE CPP #-}

{-| A database of USB identifiers.

Databases with vendor names and identifiers can be loaded from string,
file or directly from <http://www.usb.org> or
<http://linux-usb.sourceforge.net>.

Example usage:

@
import Data.ByteString.Char8 ('pack', 'unpack')
import System.USB.IDDB
import System.USB.IDDB.LinuxUsbIdRepo
import Text.Printf

main :: IO ()
main = do -- Load a snapshot from the linux-usb.sourceforget.net database.
          db <- 'staticDb'
          -- Print the name of vendor 0x1d6b
          'putStrLn' $ 'maybe' \"unknown VID!\" 'unpack'
                   $ 'vendorName' db 0x1d6b
@

@
          -- Print the ID of \"Linux Foundation\"
          'putStrLn' $ 'maybe' \"unknown vendor name!\" ('printf' \"0x%04x\")
                   $ 'vendorId' db ('pack' "Linux Foundation")
@

@
          -- Print the name of the product with ID 0x0101 from the
          -- vendor with ID 0x1d6b.
          'putStrLn' $ 'maybe' \"unknown PID!\" 'unpack'
                   $ 'productName' db 0x1d6b 0x0101
@

@
          -- Print the ID of the product with the name \"Audio Gadget\"
          -- from the vendor with ID 0x1d6b.
          'putStrLn' $ 'maybe' \"unknown product name!\" ('printf' \"0x%04x\")
                   $ 'productId' db 0x1d6b ('pack' \"Audio Gadget\")
@
-}
module System.USB.IDDB
    ( -- *Types
      IDDB
    , VendorID
    , VendorName
    , ProductID
    , ProductName

    , emptyDb

      -- *Query database
    , vendorName
    , vendorId
    , productName
    , productId
    )
    where

import System.USB.IDDB.Base
