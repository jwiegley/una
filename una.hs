#!/usr/bin/env runhaskell

module Main where

-- una.hs, version 1.0 (2010-07-02)
--
-- by John Wiegley <johnw@newartisans.com>
--
-- A simple universal unarchiving utility.  Just point it at any archive or
-- compressed file, and it spits out a single file or directory in the current
-- directory with its contents.  Use -d to delete the original archive on
-- success.  Use -f to overwrite any existing file or directory which might be
-- in the way.

import Data.List
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as Char8
import System.Environment
import System.Console.GetOpt
import System.FilePath
import System.Process
import System.Directory
import System.IO
import System.Exit
import Control.Monad
import Control.Concurrent.MonadIO
import qualified Control.Exception as C

-- This script takes a series of pathnames to compressed files and/or
-- archives, and uncompresses/unarchives them.
--
-- What is useful about this script is that it guarantees that the result of
-- the unarchiving is a single new entry in the current directory.  That is:
--
--   1. If it's simply a compressed file, it uncompresses it in the current
--      directory.
--   2. If it's an archive containing a single file or directory, the result
--      is much the same as if it had been compressed: the file or directory
--      ends up in the current directory.
--   3. If the archive contains multiple items, they are unarchived in a
--      directory named after the original.
--
-- If the -d flag is given, the original file is removed after a successful
--   unarchiving.
-- If the -f flag is given, any existing files in the current directory will
--   be overwritten.  Otherwise, an error is signaled.

main :: IO ()
main = do
  cmdArgs <- getArgs
  forM_ cmdArgs $ \path -> do
    cpath  <- canonicalizePath path
    result <- unarchive cpath
    case result of
      ArchiveError err -> error err
      FileName f       -> do rf <- makeRelativeToCurrentDirectory f
                             putStrLn $ "Extracted single file: " ++ rf
      DirectoryName f  -> do rf <- makeRelativeToCurrentDirectory f
                             putStrLn $ "Extracted directory: " ++  rf
      _                -> return ()

-- The variations of unarchive' receive a list of archive types yet to be
-- "unwrapped" from the previous extraction, plus a cleanup action which must
-- be executed before the final result is returned.  The end result is that
-- only the final extraction remains, indicated by the return value, with all
-- temporaries having been properly cleaned up.

unarchive :: FilePath -> IO Extraction
unarchive path = do
  fexists <- doesFileExist basename
  dexists <- doesDirectoryExist basename
  if fexists || dexists
    then return $ ArchiveError $ "Destination already exists: " ++ basename
    else unarchive' typs (FileName path,return ())

  where (basename, typs) = archiveTypes path

        -- If an error occurs at any stage of the process, short-circuit and
        -- return, performing any cleanup along the way.
        unarchive' _  (x@(ArchiveError _),m) = do m; return x

        -- If we reach the final step of the unarchiving process, and the
        -- result is a data stream, write it to disk at the desired basename.
        unarchive' [] (x@(DataStream d),m) = do
          B.writeFile basename d
          m; return (FileName basename)

        unarchive' [] (x@(FileName f),m) = do
          when (f /= basename) $ renameFile f basename
          m; return (FileName basename)

        unarchive' [] (x@(DirectoryName f),m) = do
          when (f /= basename) $ renameDirectory f basename
          m; return (DirectoryName basename)

        unarchive' (t:ts) (x,m) = do y <- extract t x
                                     result <- unarchive' ts y
                                     m; return result

-- Every archive type known to this script is listed here.  They are each
-- handled by `unaCmd', below, which returns the command needed to unarchive
-- that type.

data ArchiveType = Compress     -- UNIX-style compress
                 | Gzip
                 | Bzip2
                 | P7zip
                 | Xzip
                 | Zip
                 | Tarball
                   -- jww (2010-07-08): Types to add:
                   --  .UU         UUencode
                   --  .ARJ        Arj
                   --  .BIN        MacBinary
                   --  .BXY        some old Binary II format
                   --  .BNY        some old Binary II format
                   --  .BQY        some old Binary II format
                   --  .CAB        Windows CABinet files
                   --  .cpio       CPIO
                   --  .dmg        Apple Disk Image
                   --  .HQX        BinHex?
                   --  .ISO        CD-ROM ISO9660
                   --  .LHA        LHarc
                   --  .LZH        LHarc again
                   --  .RAR        WinRAR
                   --  .SDK        ShrinkIt disk
                   --  .SEA        StuffIt Expander archive
                   --  .SHK        ShrinkIt archive
                   --  .SIT        StuffIt archive
                   -- jww (2010-07-08): Make these case-insensitive
                 deriving Show


-- Determine which "type" an archive is by examining its extension.  It may
-- not even be an archive at all, but just a compressed file.  It could even
-- be a compressed archive containing just a single file!  There are too many
-- possible combinations to know at this point.

exts = [ (".tar", [Tarball])
       , (".taz", [Compress, Tarball])
       , (".tgz", [Gzip, Tarball])
       , (".tbz", [Bzip2, Tarball])
       , (".tz2", [Bzip2, Tarball])
       , (".txz", [Xzip, Tarball])
       , (".z",   [Compress])
       , (".gz",  [Gzip])
       , (".bz2", [Bzip2])
       , (".xz",  [Xzip])
       , (".7z",  [P7zip])
       , (".zip", [Zip])
       , (".jar", [Zip])
       ]

archiveTypes' :: [ArchiveType] -> FilePath -> (FilePath, [ArchiveType])
archiveTypes' acc f = apply $ lookup (takeExtension f) exts
  where apply (Just types) = archiveTypes' (acc ++ types) (dropExtension f)
        apply Nothing      = (f, acc)

archiveTypes :: FilePath -> (FilePath, [ArchiveType])
archiveTypes = archiveTypes' []

-- Given an archive path and its type, extract its contents.  Depending on the
-- type of archive, and what command-line options the unarchiver command
-- supports, the result may be one of several types, described by the type
-- Extraction.

noCleanup :: IO ()
noCleanup = return ()


data Extraction = DataStream B.ByteString
                | FileName FilePath
                | DirectoryName FilePath
                | ArchiveError String
                deriving (Show)


extractArgs :: Extraction -> [String] -> ([String], B.ByteString)
extractArgs (DataStream d) args = (args, d)
extractArgs (FileName f) args   = (args ++ [f], B.empty)
extractArgs _ _                 = error "Cannot extract a directory"


extractStream :: String -> [String] -> Extraction -> IO (Extraction, IO ())
extractStream cmd args x = do
  (exit, out, err) <- uncurry (bReadProcessWithExitCode cmd)
                              (extractArgs x args)
  if exit == ExitSuccess
    then return (DataStream out, noCleanup)
    else return (ArchiveError (Char8.unpack err), noCleanup)


extract :: ArchiveType -> Extraction -> IO (Extraction, IO ())
extract Compress = extractStream "gzip" ["-dc"]
extract Gzip     = extract Compress
extract Bzip2    = extractStream "bzip2" ["-dc"]
extract Xzip     = extractStream "xz" ["-dc"]
extract Tarball  = extractTarball
extract P7zip    = extractP7zip
extract Zip      = extractZip
extract _        = \x -> return (ArchiveError "Unrecognized archive type", noCleanup)


extractTarball :: Extraction -> IO (Extraction, IO ())
extractTarball (DataStream d) = extractArchive "tar" ["xCf", "$DIR", "-"] d
extractTarball (FileName f)   = extractArchive "tar" ["xCf", "$DIR", f] B.empty

extractP7zip :: Extraction -> IO (Extraction, IO ())
extractP7zip (DataStream d) = extractArchive "7za" ["x", "-o$DIR", "-si"] d
extractP7zip (FileName f)   = extractArchive "7za" ["x", f, "-o$DIR"] B.empty

extractZip :: Extraction -> IO (Extraction, IO ())
extractZip (DataStream d) = do
  (path, handle) <- openBinaryTempFile "." ".file.zip"
  B.hPut handle d
  hFlush handle
  -- unzip does not support reading from standard input, so we must write the
  -- data out to a temporary file in order to continue.
  (x,m) <- extractZip (FileName path)
  hClose handle
  return (x, do removeFile path; m)
extractZip (FileName f) = extractArchive "unzip" ["-d", "$DIR", f] B.empty


replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace old new xs@(y:ys) =
  case stripPrefix old xs of
    Nothing -> y : replace old new ys
    Just ys' -> new ++ replace old new ys'

extractArchive :: String -> [String] -> B.ByteString -> IO (Extraction, IO ())
extractArchive cmd args inp = do
  -- jww (2010-07-08): Create a real temporary name here
  tmpDir <- return "tmpdir"
  -- Make a temporary directory within the current dir
  createDirectory tmpDir
  -- Extract the contents of the archive into that directory
  (exit, out, err) <- let args' = map (replace "$DIR" tmpDir) args
                      in bReadProcessWithExitCode cmd args' inp
  if exit == ExitSuccess
    then examineContents tmpDir
    else return (ArchiveError (Char8.unpack err), noCleanup)


-- Examine the contents of the now-populated temp directory
--   if it's a single file, return the name and a cleanup action to
--     remove temp
--   if it's a single directory, recurse this step
--   if it's many files and/or directories, return the name of the temp
--     directory, and no cleanup action

examineContents :: FilePath ->  IO (Extraction, IO ())
examineContents dir = do
  contents <- getDirectoryContents dir
  case delete "." $ delete ".." contents of
    []     -> return (ArchiveError "Archive was empty", 
                      removeDirectoryRecursive dir)
    (x:[]) -> do let path = dir </> x
                 isDir <- doesDirectoryExist path
                 if isDir
                   then do (x,m) <- examineContents path
                           return (x, do removeDirectoryRecursive dir; m)
                   else return (FileName path, removeDirectoryRecursive dir)
    xs     -> return (DirectoryName dir, noCleanup)


-- The following function was copied from System.Process, because I needed a
-- variant which operates on lazy ByteStrings.  The regular version attempts
-- to decode Unicode in the binary output from the decompressor.

bReadProcessWithExitCode
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> B.ByteString               -- ^ standard input
    -> IO (ExitCode,B.ByteString,B.ByteString) -- ^ exitcode, stdout, stderr
bReadProcessWithExitCode cmd args input = do
    -- putStrLn $ "Executing: " ++ cmd ++ show args

    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }

    outMVar <- newEmptyMVar

    -- fork off a thread to start consuming stdout
    out  <- B.hGetContents outh

    _ <- forkIO $ C.evaluate (B.length out) >> putMVar outMVar ()

    -- fork off a thread to start consuming stderr
    err  <- B.hGetContents errh
    _ <- forkIO $ C.evaluate (B.length err) >> putMVar outMVar ()

    -- now write and flush any input
    unless (B.null input) $ do B.hPutStr inh input; hFlush inh
    hClose inh -- done with stdin

    -- wait on the output
    takeMVar outMVar
    takeMVar outMVar
    hClose outh
    hClose errh

    -- wait on the process
    ex <- waitForProcess pid

    return (ex, out, err)

-- una.hs ends here
