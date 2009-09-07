module Main where

import System.USB.IDDB
import Data.ByteString.Char8 (pack, unpack)

main :: IO ()
main = do putStrLn "linux-usb.sourceforge.net - from file"
          demo =<< usbIdRepoDb
          putStrLn ""

          putStrLn "usb.org - from file"
          demo =<< usbDotOrgDb
          putStrLn ""

          putStrLn "linux-usb.sourceforge.net - from web"
          db <- usbIdRepoDbFromWeb
          maybe (putStrLn "can't load from linux-usb.sourceforge.net")
                demo
                db
          putStrLn ""

          putStrLn "usb.org - from web"
          db <- usbDotOrgDbFromWeb
          maybe (putStrLn "can't load from usb.org")
                demo
                db

demo :: IDDB -> IO ()
demo db = do -- Print the name of vendor 0x1D6B
             putStrLn $ maybe "unknown ID!" unpack
                      $ vendorName db 0x1D6B
             -- Print the ID of "The Linux Foundation"
             putStrLn $ maybe "unknown name!" show
                      $ vendorId db (pack "The Linux Foundation")
