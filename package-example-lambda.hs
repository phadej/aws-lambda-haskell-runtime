module Main (main) where

import AWS.Lambda.RuntimeAPI.Package (defaultConf, packageAwsLambda)
import Data.Char                     (isSpace)

import qualified Data.ByteString.Lazy as LBS
import qualified System.Process       as P

main :: IO ()
main = do
    -- Build the executable
    let exe = "example-lambda"
    P.callProcess "cabal" ["new-build", exe]

    -- Find the build artifact
    exePath <- trim <$> P.readProcess "cabal-plan" [ "list-bin", exe ] ""
    putStrLn $ "Packaging executable: " ++ exePath

    -- Package it
    zipFile <- packageAwsLambda defaultConf exePath
    LBS.writeFile "example-lambda.zip" zipFile
  where
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
