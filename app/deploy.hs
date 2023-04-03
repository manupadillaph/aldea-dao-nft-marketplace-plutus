module Main where

import           Control.Exception (throwIO)
import           Data.List  
import           System.Environment (getArgs)

import qualified Deploy

--Modulo: 

main :: IO ()
main = do

  putStrLn "Deploy Smart Contracts:"

  [ pathOutputFiles, uTxOutRefStr, adminsStr ] <- getArgs

  let 
    maybePathOutputFiles = 
      if pathOutputFiles == "" then
        Nothing
      else
        Just pathOutputFiles
        
  Deploy.exportarScripts maybePathOutputFiles adminsStr uTxOutRefStr 

