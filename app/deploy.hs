module Main where

import           Control.Exception (throwIO)
import           Data.List  
import           System.Environment (getArgs)
import qualified Control.Monad.IO.Class                                                     as MonadIOClass (MonadIO (..))
import qualified Control.Monad                       as Monad (void)
import qualified MarketNFTV2.Deploy as Deploy

--Modulo: 

-- cabal run deploy "/home/manuelpadilla/source/copyRepos/ALDEA-DAO/nft-marketplace-plutus-v2/export/files" "" "abfff883edcf7a2e38628015cebb72952e361b2c8a2262f7daf9c16e" "0d74ec8142e06007b3969313e391cf30ad8b2a96efa3293ba161b91f59564d8c#1"

#1

main :: IO ()
main = do
    putStrLn "Creating Smart Contracts..."

    [ pathOutputFiles, protocolName, adminsStr, uTxOutRefStr ] <- getArgs

    let 
        maybePathOutputFiles = 
            if pathOutputFiles == "" then
                Nothing
            else
                Just pathOutputFiles

        maybeProtocolName = 
            if protocolName == "" then
                Nothing
            else
                Just protocolName
    Monad.void $ Deploy.exportarScriptsFromStringsParams maybePathOutputFiles maybeProtocolName adminsStr uTxOutRefStr   

