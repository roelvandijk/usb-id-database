module Main where

import System.USB.IDDB
import Data.ByteString.Char8 (pack, unpack)

main :: IO ()
main = do -- Acquire the default database
          db <- vdbDefault
          -- Print the name of vendor 0x1D6B
          putStrLn $ maybe "unknown ID!" unpack
                   $ vendorName db 0x1D6B
          -- Print the ID of "The Linux Foundation"
          putStrLn $ maybe "unknown name!" show
                   $ vendorID db (pack "The Linux Foundation")
