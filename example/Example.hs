module Main where

import AWS.Lambda.RuntimeAPI
import Control.Lens          (deep, (%~), (&))
import Data.Aeson            (Value (..), decode) 
import Data.String (fromString)
import Data.Aeson.Lens       (_String)
import System.Environment (getArgs)

import qualified Data.Text as T

main :: IO ()
main = autoMockJsonMain mock $ \req -> do
    -- get remaining time (in millisecons) and log it
    t <- getTimeRemaining req
    print t

    let res :: Value
        res = requestPayload req
            & deep _String %~ T.toUpper

    return res
  where
    mock = Mock
        { mockResponse = print
        , mockRequest = do
            args <- getArgs
            case args of
                -- no arguments: run with @null@
                [] -> mockRequest' Null
                (a:_)
                    -- single argument: if looks like valid JSON: use it
                    | Just a' <- decode (fromString a) -> mockRequest' a'
                    -- otherwise pass as a literal string:
                    | otherwise -> mockRequest' (String (fromString a))
        }

    mockRequest' :: Value -> IO (GenRequest Value)
    mockRequest' v = makeMockRequest v 10000 -- 10 seconds
