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
--
-- To handle all the supported formats on Mac OS X, you must first install:
--   sudo port install cabextract unarj unrar lha p7zip

import Data.Char
import Data.List
import Data.Function
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
-- archives, and uncompresses/decodes/unarchives them.
--
-- What's especially useful about this script is that it guarantees that the
-- result of this process is a single new entry in the current directory,
-- whether that be a file or a directory.  That is:
--
--   1. If it's simply a compressed file, it uncompresses in the current
--      directory.
--   2. If it's an archive containing a single file or directory, the result
--      is the same as if it had been compressed: the file or directory
--      ends up in the current directory.
--   3. If the archive contains multiple items, they are unarchived in a
--      directory named after the original file.

main :: IO ()
main = do
  cmdArgs <- getArgs
  forM_ cmdArgs $ \path -> do
    cpath  <- canonicalizePath path
    result <- extract cpath False
    case result of
      ArchiveError err -> error err
      _ -> return ()

      -- DEBUG:
      -- FileName f       -> do rf <- makeRelativeToCurrentDirectory f
      --                        putStrLn $ "-> file: " ++ rf
      -- DirectoryName f  -> do rf <- makeRelativeToCurrentDirectory f
      --                        putStrLn $ "-> dir: " ++  rf

-- Determine which "type" an archive is by examining its extension.  It may
-- not be an archive at all, but just a compressed file.  It could even be a
-- compressed archive containing just a single file!  There are too many
-- possible combinations to know at this point.

exts = [ (".tar",          [tarballExtractor])
       , (".taz",          [compressExtractor, tarballExtractor])
       , (".tgz",          [gzipExtractor, tarballExtractor])
       , (".tbz",          [bzip2Extractor, tarballExtractor])
       , (".tz2",          [bzip2Extractor, tarballExtractor])
       , (".txz",          [xzipExtractor, tarballExtractor])
       , (".z",            [compressExtractor])
       , (".gz",           [gzipExtractor])
       , (".bz2",          [bzip2Extractor])
       , (".xz",           [xzipExtractor])
       , (".7z",           [p7zipExtractor])
       , (".zip",          [zipExtractor])
       , (".jar",          [zipExtractor])
       , (".arj",          [arjExtractor])
       , (".lha",          [lhaExtractor])
       , (".lzh",          [lhaExtractor])
       , (".rar",          [rarExtractor])
       , (".uu",           [uuExtractor])
       , (".ar",           [arExtractor])
       , (".cab",          [cabExtractor])
       , (".cpio",         [cpioExtractor])

       -- , (".gpg",          [gpgExtractor])
       -- , (".asc",          [gpgExtractor])

       , (".dmg",          [diskImageExtractor])
       , (".iso",          [diskImageExtractor])
       , (".cdr",          [diskImageExtractor])
       , (".sparseimage",  [diskImageExtractor])
       , (".sparsebundle", [diskImageExtractor])

       , (".sit",          [stuffItExtractor True])
       , (".sea",          [stuffItExtractor True])
       , (".bin",          [stuffItExtractor False])
       , (".hqx",          [stuffItExtractor False])

       , (".sdk",          [shrinkItExtractor])
       , (".shk",          [shrinkItExtractor])
       , (".bxy",          [shrinkItExtractor])
       , (".bny",          [shrinkItExtractor])
       , (".bqy",          [shrinkItExtractor])
       ]

-- Gzip and family are very simple compressors that can handle streaming data
-- quite easily.

returnStream :: B.ByteString -> IO ExtractionResult
returnStream out = return (DataStream out, noCleanup)

simpleExtractor :: String -> [String] -> Extraction -> IO ExtractionResult
simpleExtractor cmd args item =
  case item of
    DataStream d -> performExtract cmd args d returnStream
    FileName f   -> performExtract cmd (args ++ [f]) B.empty returnStream
    
gzipExtractor     = Extractor $ simpleExtractor "gzip" ["-qdc"]
compressExtractor = gzipExtractor
bzip2Extractor    = Extractor $ simpleExtractor "bzip2" ["-qdc"]
xzipExtractor     = Extractor $ simpleExtractor "xz" ["-qdc"]

uuExtractor       = Extractor $ simpleExtractor "uudecode" []
-- gpgExtractor      = Extractor $ simpleExtractor "gpg" ["-d"]

-- Tarballs and 7-zip are both archive formats that can accept their input on
-- stdin.  It's not likely that someone will compress a 7zip archive, but it's
-- handled anyway.

returnContents :: FilePath -> B.ByteString -> IO ExtractionResult
returnContents dir _ = examineContents dir True

tarballExtractor = Extractor $ \item -> do
  dir <- createTempDirectory
  case item of
    DataStream d ->
      performExtract "tar" ["xCf", dir, "-"] d (returnContents dir)
    FileName f   ->
      performExtract "tar" ["xCf", dir, f] B.empty (returnContents dir)

p7zipExtractor = Extractor $ \item -> do
  dir <- createTempDirectory
  case item of
    DataStream d ->
      performExtract "7za" ["x", "-bd", "-o" ++ dir, "-si"] d
                     (returnContents dir)
    FileName f   ->
      performExtract "7za" ["x", f, "-bd", "-o" ++ dir] B.empty
                     (returnContents dir)
                     
-- Zip and CAB are not quite as flexible as tar and 7-zip, in that they cannot
-- accept the archive via stdin.  If there is a stream from an earlier
-- decoding, it must be written to a temp file and then extracted from that.

zipExtractor = Extractor $ fix $ \fn item ->
  case item of
    DataStream d -> extractByTemp d fn
    FileName f   -> do
      dir <- createTempDirectory
      performExtract "unzip" ["-q", "-d", dir, f] B.empty (returnContents dir)

cabExtractor = Extractor $ fix $ \fn item ->
  case item of
    DataStream d -> extractByTemp d fn
    FileName f   -> do
      dir <- createTempDirectory
      performExtract "cabextract" ["-q", "-d", dir, f] B.empty
                     (returnContents dir)
                     
-- cpio doesn't know how to extract its contents to a particular directory, so
-- a temporary must be created.  It can, however, read input from stream.

extractInTempDir :: String -> [String] -> B.ByteString -> IO ExtractionResult
extractInTempDir cmd args inp = do
  dir <- createTempDirectory
  cdir <- canonicalizePath dir
  cwd <- getCurrentDirectory
  ccwd <- canonicalizePath cwd
  C.bracket (setCurrentDirectory cdir)
            (\_ -> setCurrentDirectory ccwd)
            (\_ -> performExtract cmd args inp (returnContents cdir))

cpioExtractor = Extractor $ fix $ \fn item ->
  case item of
    DataStream d -> extractInTempDir "cpio" ["-id"] d
    FileName f   -> extractInTempDir "cpio" ["-idF", f] B.empty

-- The next set of formats can't handle streaming input, nor can they extract
-- to a specified directory.  This is why I call them the "dumb" extractors.
-- Everything must be setup for them in advance.

dumbExtractor :: String -> String -> Extraction -> IO ExtractionResult
dumbExtractor cmd arg item =
  case item of
    DataStream d -> extractByTemp d (dumbExtractor cmd arg)
    FileName f   -> extractInTempDir cmd [arg, f] B.empty

arjExtractor      = Extractor $ dumbExtractor "unarj" "x"
lhaExtractor      = Extractor $ dumbExtractor "lha" "x"
rarExtractor      = Extractor $ dumbExtractor "unrar" "x"
arExtractor       = Extractor $ dumbExtractor "ar" "x"
shrinkItExtractor = Extractor $ dumbExtractor "nulib2" "-x"

-- Disk images are mountable archives, which means the data must be copied out
-- in order to "extract" it.  jww (2010-07-09): We should handle Linux
-- loopbook mounts too.

diskImageExtractor = Extractor $ fix $ \fn item ->
  case item of
    DataStream d -> extractByTemp d fn
    FileName f   -> do
      let args = ["attach", "-readonly", "-mountrandom", "/tmp",
                  "-noverify", "-noautofsck", f]
      -- DEBUG:
      -- putStrLn $ "hdiutil " ++ unwords args
      (exit, out, _) <- readProcessWithExitCode "hdiutil" args []
      case exit of
        ExitFailure _ ->
          return (ArchiveError "Failed to attach disk image", noCleanup)

        ExitSuccess -> do
          let mountPoint =
                case find (isInfixOf "/tmp/dmg") (lines out) of
                  Just line -> (Just . last . words) line
                  Nothing   -> Nothing
          case mountPoint of
            Nothing -> return (ArchiveError "Failed to attach disk image",
                               noCleanup)
            Just dir -> do
              tmpDir <- createTempDirectory
              -- DEBUG:
              -- putStrLn $ "ditto " ++ unwords [dir, tmpDir]
              readProcessWithExitCode "ditto" [dir, tmpDir] []
              -- DEBUG:
              -- putStrLn $ "hdiutil " ++ unwords ["detach", dir, "-force"]
              readProcessWithExitCode "hdiutil" ["detach", dir, "-force"] []
              examineContents tmpDir True
              
-- StuffIt Expander is its own creature.  We talk to it via Applescript, as I
-- know of no better way.

stuffItExtractor archivep = Extractor $ fix $ \fn item ->
  case item of
    DataStream d -> extractByTemp d fn
    FileName f   -> do
      tmpDir <- createTempDirectory
      canonTmp <- canonicalizePath tmpDir

      let script =    "  tell application \"StuffIt Expander\"\n"
                   ++ "    run\n"
                   ++ "    expand {POSIX file \"" ++ f ++ "\"}"
                   -- ++ " with delete originals"
                   ++ " to POSIX file \"" ++ canonTmp ++ "\"\n"
                   ++ "  end tell"

      -- DEBUG:
      -- putStrLn "! invoking StuffIt Expander"

      (exit, _, err) <- readProcessWithExitCode "osascript" [] script
      case exit of
        ExitSuccess   ->
          if archivep
          then examineContents canonTmp True
          else do contents <- getDirectoryContents canonTmp
                  let elems = delete "." $ delete ".." contents
                      file  = C.assert (length elems == 1)
                                       (canonTmp </> head elems)
                  exists <- doesFileExist file
                  return (FileName $ C.assert exists file,
                          removeDirectoryRecursive canonTmp)
        ExitFailure _ -> return (ArchiveError $
                                 "Failed to invoke StuffIt Expander: " ++ err,
                                 removeDirectoryRecursive canonTmp)

-- Types used by this script.

data Extractor = Extractor { extractor :: Extraction -> IO ExtractionResult }

data Extraction = DataStream B.ByteString
                | FileName FilePath
                | DirectoryName FilePath
                | ArchiveError String
                deriving (Show)

type ExtractionResult = (Extraction, IO ())

noCleanup :: IO ()
noCleanup = return ()


-- Given a file path, determine its type and extract its contents.  Depending
-- on the type of the file, and what command-line options the extraction
-- command supports, the result may be one of several types, described by the
-- type Extraction.

extract :: FilePath -> Bool -> IO Extraction
extract path overwrite = do
  fexists <- doesFileExist basename
  dexists <- doesDirectoryExist basename
  when (fexists || dexists) $
    if overwrite
    then if fexists
         then removeFile basename
         else removeDirectoryRecursive basename
    else unless (null typs) $
         error $ "Destination already exists: " ++ basename

  extract' typs (FileName path,return ())

  where (basename, typs) = findExtractors [] path

        -- The variations of extract' receive a list of archive types yet to
        -- be "unwrapped" from the previous extraction, plus a cleanup action
        -- which must be executed before the final result is returned.  The
        -- end result is that only the final extraction remains, indicated by
        -- the return value, with all temporaries having been properly cleaned
        -- up.

        extract' _  (x@(ArchiveError _),m) = do m; return x

        extract' [] (x@(DataStream d),m) = do
          -- If we reach the final step of the unarchiving process, and
          -- the result is a data stream, write it to disk at the
          -- desired basename.
          B.writeFile basename d
          m; return (FileName basename)

        extract' [] (x@(FileName f),m) = do
          renameFile f basename
          m; return (FileName basename)

        extract' [] (x@(DirectoryName f),m) = do
          renameDirectory f basename
          m; return (DirectoryName basename)

        extract' (t:ts) (x,m) = do y <- extractor t x
                                   result <- extract' ts y
                                   m; return result


findExtractors :: [Extractor] -> FilePath -> (FilePath, [Extractor])
findExtractors acc f = apply $ lookup (map toLower (takeExtension f)) exts
  where apply (Just types) = findExtractors (acc ++ types) (dropExtension f)
        apply Nothing      = (f, acc)


performExtract :: String        -- command to execute
                  -> [String]   -- command arguments
                  -> B.ByteString -- standard input
                  -> (B.ByteString -> IO ExtractionResult)
                                -- function to process output
                  -> IO ExtractionResult
performExtract cmd args ds fn = do
  (exit, out, err) <- bReadProcessWithExitCode cmd args ds
  if exit == ExitSuccess
    then fn out
    else return (ArchiveError err, noCleanup)


examineContents :: FilePath -> Bool -> IO ExtractionResult
examineContents dir cleanup = do
  -- Examine the contents of a populated directory
  --  if it's a single file, return the name and a cleanup action to
  --    remove temp
  --  if it's a single directory, recurse this step
  --  if it's many files and/or directories, return the name of the temp
  --    directory, and no cleanup action
  canon <- canonicalizePath dir
  contents <- getDirectoryContents canon
  case delete "." $ delete ".." contents of
    [] -> return (ArchiveError "Empty archive", removeDirectoryRecursive canon)

    (x:[]) -> do
      let path = canon </> x
      isDir <- doesDirectoryExist path
      if isDir
        then do (x,m) <- examineContents path False
                return (x, do when cleanup $ removeDirectoryRecursive canon; m)
        else do x <- extract path False
                return (x, when cleanup $ removeDirectoryRecursive canon)

    xs     -> return (DirectoryName canon, noCleanup)


extractByTemp :: B.ByteString   -- output to write to temp
                 -> (Extraction -> IO ExtractionResult)
                                -- function to handle the new temp file
                 -> IO ExtractionResult
extractByTemp ds fn = do
  (path, handle) <- openBinaryTempFile "." "file.ar"
  -- DEBUG:
  -- putStrLn $ "> " ++ path
  B.hPut handle ds
  hFlush handle
  -- unzip does not support reading from standard input, so we must write the
  -- data out to a temporary file in order to continue.
  (x,m) <- fn (FileName path)
  hClose handle
  return (x, do removeFile path; m)

createTempDirectory :: IO FilePath
createTempDirectory = do
  (path, handle) <- openBinaryTempFile "." "dir.tmp"
  hClose handle
  removeFile path
  createDirectory path
  return path


-- The following function was copied from System.Process, because I needed a
-- variant which operates on lazy ByteStrings.  The regular version attempts
-- to decode Unicode in the binary output from the decompressor.

bReadProcessWithExitCode
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> B.ByteString               -- ^ standard input
    -> IO (ExitCode,B.ByteString,String) -- ^ exitcode, stdout, stderr
bReadProcessWithExitCode cmd args input = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }

    outMVar <- newEmptyMVar

    -- fork off a thread to start consuming stdout
    out  <- B.hGetContents outh
    _ <- forkIO $ C.evaluate (B.length out) >> putMVar outMVar ()

    -- fork off a thread to start consuming stderr
    err  <- hGetContents errh
    _ <- forkIO $ C.evaluate (length err) >> putMVar outMVar ()

    -- now write and flush any input
    unless (B.null input) $ do B.hPutStr inh input; hFlush inh
    -- DEBUG:
    -- if B.null input
    --   then putStrLn $ cmd ++ " " ++ unwords args
    --   else do putStrLn $ "| " ++ cmd ++ " " ++ unwords args
    --           B.hPutStr inh input; hFlush inh
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
