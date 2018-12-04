{-# LANGUAGE MultiWayIf #-}
-- | Utility to package lambda
--
-- Example usage, you need to run it on AWS Linux
-- (e.g. Docker, see [Dockerfile](https://github.com/phadej/aws-lambda-runtime-api/blob/master/docker-image/Dockerfile))
-- with @GHC@, @cabal@ and @cabal-plan@ available.
--
-- @
-- module Main (main) where
--
-- import AWS.Lambda.RuntimeAPI.Package ('defaultConf', 'packageAwsLambda')
-- import Data.Char                     (isSpace)
--
-- import qualified Data.ByteString.Lazy as LBS
-- import qualified System.Process       as P
--
-- main :: IO ()
-- main = do
--     -- Build the executable
--     let exe = "example-lambda"
--     P.callProcess "cabal" ["new-build", exe]
--
--     -- Find the build artifact
--     exePath \<- trim \<$> P.readProcess "cabal-plan" [ "list-bin", exe ] ""
--     putStrLn $ "Packaging executable: " ++ exePath
--
--     -- Package it
--     zipFile \<- 'packageAwsLambda' 'defaultConf' exePath
--     LBS.writeFile "example-lambda.zip" zipFile
--   where
--     trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
-- @
--
-- Given such environment, we can build an example lambda function:
--
-- @
-- % docker run -ti --rm -v $(pwd):/work -w /work amazonlinux-with-ghc
--
-- % cabal new-update
-- % cabal new-build
-- % runghc package-example-lambda.hs
-- @
--
-- The resulting @example-lambda.zip@ is ready to be uploaded!
--
module AWS.Lambda.RuntimeAPI.Package (
    packageAwsLambda,
    Conf (..),
    defaultConf,
    confAdditionalLibs,
    confReadFile,
    -- * Utilities
    findExtraLibs,
    ) where

import Prelude ()
import Prelude.Compat

import Control.Applicative   (Alternative (..), optional)
import Control.DeepSeq       (force)
import Control.Exception     (evaluate)
import Data.Bits             (shiftL)
import Data.Char             (isAlphaNum, isHexDigit)
import Data.List             (isPrefixOf)
import Data.List.NonEmpty    (NonEmpty (..))
import Data.Maybe            (catMaybes)
import Data.Traversable      (for)
import System.FilePath.Posix (takeFileName)
import System.Process        (proc, readCreateProcess)

import qualified Codec.Archive.Zip    as Zip
import qualified Data.ByteString.Lazy as LBS
import qualified Text.Parsec          as P

-- | Package AWS Lambda executable.
packageAwsLambda
    :: Conf
    -> FilePath
    -> IO LBS.ByteString  -- ^ Result is ZIP package
packageAwsLambda conf exePath = do
    -- executable, we package it as "bootstrap"
    exeContents <- _confReadFile conf exePath

    let exeEntry :: Zip.Entry
        exeEntry = (Zip.toEntry "bootstrap" 0 exeContents)
            { Zip.eExternalFileAttributes = shiftL 0755 16
            , Zip.eVersionMadeBy = 0x0300 -- UNIX file attributes
            }

    -- .so dependencies
    libs <- findExtraLibs (_confAdditionalLibs conf) exePath
    libEntries <- for libs $ \lib -> do
        libContents <- _confReadFile conf lib
        return $ Zip.toEntry (takeFileName lib) 0 libContents

    -- all together
    let entries = exeEntry : libEntries

    evaluate $ force $ Zip.fromArchive Zip.Archive
        { Zip.zEntries   = entries
        , Zip.zSignature = Nothing
        , Zip.zComment   = mempty
        }

data Conf = Conf
    { _confAdditionalLibs :: ![String]                         -- ^ library names we know surely need to be packaged
    , _confReadFile       :: !(FilePath -> IO LBS.ByteString)  -- ^ function to read file from filesystem
    }

defaultConf :: Conf
defaultConf = Conf
    { _confAdditionalLibs = []
    , _confReadFile       = LBS.readFile
    }

confAdditionalLibs :: Functor f => ([String] -> f [String]) -> Conf -> f Conf
confAdditionalLibs f conf = (\x -> conf { _confAdditionalLibs = x}) <$> f (_confAdditionalLibs conf)

confReadFile :: Functor f => ((FilePath -> IO LBS.ByteString) -> f (FilePath -> IO LBS.ByteString)) -> Conf -> f Conf
confReadFile f conf = (\x -> conf { _confReadFile = x}) <$> f (_confReadFile conf)

-------------------------------------------------------------------------------
-- LDD magic - find dependencies
-------------------------------------------------------------------------------

-- | Run @ldd@ on given @filepath@, return @.so@ files.
findExtraLibs
    :: [String]        -- ^ additional libs
    -> FilePath        -- ^ ELF file
    -> IO [FilePath]
findExtraLibs additionalCopyLibs fp = do
    output <- readCreateProcess (proc "ldd" [fp]) ""
    either (fail . show) (return . catMaybes) $
        P.parse (many lddLine <* P.eof) "<ldd output>" output
  where
    lddLine = do
        P.spaces
        lib <|> ldLinux

    lib = do
        l :| _ <- sepByNonEmpty
            (some $ P.satisfy $ \c -> isAlphaNum c || c == '-' || c == '+' || c == '_')
            (P.char '.')

        P.spaces
        md <- optional $ do
            _ <- P.string "=>"
            P.spaces
            many $ P.satisfy $ \c ->
                isAlphaNum c || c == '.' || c == '-' || c == '+' || c == '_' || c == '/'
        _ <- address

        if | "libHS" `isPrefixOf` l -> return md
           | l `elem` skipLibs      -> return Nothing
           | l `elem` copyLibs      -> return md
           | otherwise              -> fail $ "Unknown lib " ++ l

    ldLinux = P.string "/lib64/ld-linux-x86-64.so.2" *> address *> return Nothing

    sepByNonEmpty p sep = do
        x <- p
        xs <- many (sep *> p)
        return (x :| xs)

    -- Libraries which exist in Linux AMI
    skipLibs =
        [ "linux-vdso"

        , "libBrokenLocale"
        , "libacl"
        , "libanl"
        , "libasound"
        , "libattr"
        , "libaudit"
        , "libauparse"
        , "libblkid"
        , "libbz2"
        , "libc"
        , "libcap-ng"
        , "libcap"
        , "libcidn"
        , "libcrypt"
        -- , "libcrypto"
        , "libdbus-1"
        , "libdl"
        , "libexpat"
        , "libgcc_s-4.8.3-20140911"
        , "libgcc_s"
        , "libgpg-error"
        , "libidn"
        , "libip4tc"
        , "libip6tc"
        , "libiptc"
        , "libkeyutils"
        , "liblber-2.4"
        , "libldap-2.4"
        , "libldap_r-2.4"
        , "libldif-2.4"
        , "libm"
        , "libmount"
        , "libncurses"
        , "libncursesw"
        , "libnih-dbus"
        , "libnih"
        , "libnsl"
        , "libnss_compat"
        , "libnss_db"
        , "libnss_dns"
        , "libnss_files"
        , "libnss_hesiod"
        , "libnss_nis"
        , "libnss_nisplus"
        , "libpam"
        , "libpam_misc"
        , "libpamc"
        , "libpcre"
        , "libpopt"
        , "libpthread"
        , "libpwquality"
        , "libreadline"
        , "libresolv"
        , "librt"
        , "libsepol"
        , "libthread_db"
        , "libtinfo"
        , "libudev"
        , "libutil"
        , "libuuid"
        , "libxtables"
        , "libz"

        ]

    -- Libraries which we know for sure aren't in Amazon Linux AMI
    copyLibs =
        [ "libgmp"
        , "libffi"
        ] ++ additionalCopyLibs

    address = P.spaces
        *> P.char '('
        *> P.string "0x"
        *> P.skipMany (P.satisfy isHexDigit)
        *> P.char ')'
        *> P.char '\n'
