module Main where

import System.USB.IDDB
import System.USB.IDDB.LinuxUsbIdRepo (staticDb)
import Text.Printf (printf)


main :: IO ()
main = do -- Load a snapshot from the linux-usb.sourceforget.net database.
          db <- staticDb

          -- Print the name of vendor 0x1d6b
          putStrLn $ maybe "unknown VID!" id
                   $ vendorName db 0x1d6b

          -- Print the ID of "Linux Foundation"
          putStrLn $ maybe "unknown vendor name!" (printf "0x%04x")
                   $ vendorId db "Linux Foundation"

          -- Print the name of the product with ID 0x0101 from the
          -- vendor with ID 0x1d6b.
          putStrLn $ maybe "unknown PID!" id
                   $ productName db 0x1d6b 0x0101

          -- Print the ID of the product with the name "Audio Gadget"
          -- from the vendor with ID 0x1d6b.
          putStrLn $ maybe "unknown product name!" (printf "0x%04x")
                   $ productId db 0x1d6b "Audio Gadget"
