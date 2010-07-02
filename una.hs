#!/usr/bin/env runhaskell

module Unarchiver where

-- una.hs, version 1.0 (2010-07-02)
--
-- by John Wiegley <johnw@newartisans.com>
--
-- A simple universal unarchiving utility.  Just point it at any archive
-- or compressed file, and it spits out a single file or directory in
-- the current directory with its contents.  Use -d to delete the
-- original archive on success.  Use -f to overwrite any existing file
-- or directory that might be in the way.

import System.Environment
import System.Console.GetOpt
import System.FilePath
import System.Process
import System.Exit
import Control.Monad

-- This script takes a series of pathnames to compressed files and/or
-- archives, and uncompresses/unarchives them.
--
-- What is useful about this script is that it guarantees that the
-- result of the unarchiving is a single new entry in the current
-- directory.  That is:
--
--   1. If it's simply a compressed file, it uncompresses it in the
--      current directory.
--   2. If it's an archive containing a single file or directory, the
--      result is much the same as if it had been compressed: the file
--      or directory ends up in the current directory.
--   3. If the archive contains multiple items, they are unarchived in a
--      directory named after the original.
--
-- If the -d flag is given, the original file is removed after a
--   successful unarchiving.

-- If the -f flag is given, any existing files in the current directory
--   will be overwritten.  Otherwise, an error is signaled.

main :: IO ()
main = do
  cmdArgs <- getArgs
  forM_ cmdArgs $ \path ->
    let t = archiveType path in do 
      cmd <- unaCmd t path
      case cmd of
        Right f  -> do putStrLn $ "Path: " ++ path
                       putStrLn $ "Type: " ++ show t
                       putStrLn $ "Dest: " ++ f
        Left err -> do putStrLn $ "Path: " ++ path
                       putStrLn $ "Type: " ++ show t
                       putStrLn $ "Err:  " ++ err

-- Every archive type known to this script is listed here.  They are
-- each handled by `unaCmd', below, which returns the command needed to
-- unarchive that type.

data ArchiveType =
  Compress                      -- UNIX-style compress
  | Gzip
  | Bzip2
  | P7zip
  | Xzip
  | Tarball
  | TarCompress
  | TarGzip
  | TarBzip2
  | TarP7zip
  | TarXzip
  | Unknown
  deriving Show

-- Determine which "type" an archive is by examining its extension.  It
-- may not even be an archive at all, but just a compressed file.  It
-- could even be a compressed archive containing just a single file!
-- There are too many possible combinations to know at this point.

archiveType :: FilePath -> ArchiveType
archiveType f = 
  case takeExtension f of
    ".z"   -> if isTarball then TarCompress else Compress
    ".gz"  -> if isTarball then TarGzip     else Gzip
    ".bz2" -> if isTarball then TarBzip2    else Bzip2
    ".7z"  -> if isTarball then TarP7zip    else P7zip
    ".xz"  -> if isTarball then TarXzip     else Xzip
    ".taz" -> TarCompress
    ".tgz" -> TarGzip
    ".tbz" -> TarBzip2
    ".tz2" -> TarBzip2
    ".txz" -> TarXzip
    ".tar" -> Tarball
    _      -> Unknown

  where isTarball = innerExt f == ".tar"
        innerExt  = takeExtension . dropExtension

-- Given an archive type, return the command(s) needed to extract its
-- contents into the current directory.  The command must leave the
-- original archive untouched.

unaCmd :: ArchiveType -> FilePath -> IO (Either String FilePath)
unaCmd Compress    = uncompress "gzip"
unaCmd Gzip        = unaCmd Compress
unaCmd Bzip2       = uncompress "bzip2"
unaCmd Xzip        = uncompress "xz"
unaCmd P7zip       = un7zip
unaCmd Tarball     = untar "xf"
unaCmd TarCompress = untar "xzf"
unaCmd TarGzip     = unaCmd TarCompress
unaCmd TarBzip2    = untar "xjf"
unaCmd TarXzip     = untar "xJf"
unaCmd TarP7zip    = untar7zip

-- A simple uncompression leaves us with a file whose name is based on
-- the original file.

uncompress :: FilePath -> FilePath -> IO (Either String FilePath)
uncompress cmd f = do
  (exit, out, err) <- readProcessWithExitCode cmd ["-dc", f] []
  if exit == ExitSuccess
    then do writeFile (fileRoot f) out
            return $ Right (takeDirectory f </> fileRoot f)
    else return $ Left err

-- Extracting from an archive results in two possible results:
--
-- 1. The tarball contains a single file or directory; extract and
--    return that pathname.  This is very similar to the `uncompress'
--    case.
-- 2. The tarball contains multiple files; create a new directory for
--    them based on the tarball's name, and return that new directory.
-- 3. The tarball contains a single directory, which contains a single
--    file or directory.  This inner item is extracted and lifted into
--    the current directory.

untar :: String -> FilePath -> IO (Either String FilePath)
untar args f = do
  (exit, _, err) <- readProcessWithExitCode "tar" [args, f] []
  if exit == ExitSuccess
    then return $ Right (takeDirectory f </> fileRoot f)
    else return $ Left err

un7zip :: FilePath -> IO (Either String FilePath)
un7zip _ = return $ Left "nyi"

untar7zip :: FilePath -> IO (Either String FilePath)
untar7zip _ = return $ Left "nyi"

-- Assorted helper functions

fileRoot = takeBaseName . dropExtension

-- una.hs ends here
