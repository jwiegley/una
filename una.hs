#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}

module Main where

-- una.hs, version 2.0 (2010-07-02)
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
import System.Console.CmdArgs
import System.FilePath
import System.Process
import System.Directory
import System.IO
import System.IO.Storage
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

version   = "2.0.0"
copyright = "2009-2010"

data Una = Una
    { delete_   :: Bool
    , force     :: Bool
    , temp      :: FilePath
    , output    :: FilePath
    , sys_temp  :: Bool
    , test      :: Bool
    , files     :: [FilePath]
    }
    deriving (Data, Typeable, Show, Eq)

una = mode $ Una
    { delete_   = def &= text "Delete the original archive if successful"
    , force     = def &= flag "f" & text "Overwrite any existing file/dir"
    , temp      = def &= typDir & 
                  text "Use DIR as a temp directory, instead of current"
    , output    = def &= typDir & flag "o" & 
                  text "Unarchive to DIR instead of the current directory"
    , sys_temp  = def &= flag "T" & 
                  text "Use the system's temp directory (typically /tmp)"
    , test      = def &= explicit & flag "test" &
                  text "Extract, throw away resulting file(s), set error code"
    , files     = def &= args & typ "FILE..."
    } &=
    prog "una" &
    text "Universal recursive unarchiver/decoder/decompressor tool"


main :: IO ()
main = do
  let progInfo = ("una v" ++ version ++ ", (C) John Wiegley " ++ copyright)
  opts <- cmdArgs progInfo [una]
  
  when (null (files opts)) $ cmdArgsHelp progInfo [una] Text >>= putStr

  -- Extract each archive given on the command-line.  If it's not recognizable
  -- as an archive, the resulting pathname will be identical to the input, in
  -- which case nothing has been done.  If an error occurs for a given
  -- archive, stop then.
  forM_ (files opts) $ \path -> do
    result <- withStore "main" $ do putValue "main" "opts" opts
                                    extract path (force opts)
    if test opts
      then case result of
      ArchiveError err -> exitWith $ ExitFailure 1
      FileName fp      -> if fp /= path
                          then removeFilePath fp
                          else exitWith $ ExitFailure 1
      DirectoryName dp -> removeFilePath dp

      else case result of
      ArchiveError err -> error err
      FileName fp      -> if fp /= path
                          then success path fp "file" (delete_ opts)
                          else putStrLn $ "Archive unrecognized: " ++ fp
      DirectoryName dp -> success path dp "directory" (delete_ opts)
                             
  -- In case of success, print the final product's path and delete the
  -- original archive if -d was used.
  where success path f kind delete = do 
          rf <- makeRelativeToCurrentDirectory f
          putStrLn $ "Extracted " ++ kind ++ ": " ++ rf
          when delete $ removeFile path


getOption :: Data a => (a -> b) -> IO b
getOption opt = do
  opts <- getValue "main" "opts"
  case opts of
    Just o -> return $ opt o

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

       , (".gpg",          [gpgExtractor])
       , (".asc",          [gpgExtractor])

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
gpgExtractor      = Extractor $ simpleExtractor "gpg" ["-d"]

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
    DataStream d -> extractByTemp d ".zip" fn
    FileName f   -> do
      dir <- createTempDirectory
      performExtract "unzip" ["-q", "-d", dir, f] B.empty (returnContents dir)

cabExtractor = Extractor $ fix $ \fn item ->
  case item of
    DataStream d -> extractByTemp d ".cab" fn
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

dumbExtractor :: String -> String -> String -> Extraction -> IO ExtractionResult
dumbExtractor cmd arg ext item =
  case item of
    DataStream d -> extractByTemp d ext (dumbExtractor cmd ext arg)
    FileName f   -> extractInTempDir cmd [arg, f] B.empty

arjExtractor      = Extractor $ dumbExtractor "unarj" "x" ".arj"
lhaExtractor      = Extractor $ dumbExtractor "lha" "x" ".lha"
rarExtractor      = Extractor $ dumbExtractor "unrar" "x" ".rar"
arExtractor       = Extractor $ dumbExtractor "ar" "x" ".ar"
shrinkItExtractor = Extractor $ dumbExtractor "nulib2" "-x" ".shk"

-- Disk images are mountable archives, which means the data must be copied out
-- in order to "extract" it.  jww (2010-07-09): We should handle Linux
-- loopbook mounts too.

diskImageExtractor = Extractor $ fix $ \fn item ->
  case item of
    DataStream d -> extractByTemp d ".dmg" fn
    FileName f   -> do
      let args = ["attach", "-readonly", "-mountrandom", "/tmp",
                  "-noverify", "-noautofsck", f]

      loud <- isLoud
      when loud $ putStrLn $ "! hdiutil " ++ unwords args

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
              when loud $ 
                putStrLn $ "! ditto " ++ unwords [dir, tmpDir]
              readProcessWithExitCode "ditto" [dir, tmpDir] []
              when loud $ 
                putStrLn $ "! hdiutil " ++ unwords ["detach", dir, "-force"]
              readProcessWithExitCode "hdiutil" ["detach", dir, "-force"] []
              examineContents tmpDir True
              
-- StuffIt Expander is its own creature.  We talk to it via Applescript, as I
-- know of no better way.

stuffItExtractor archivep = Extractor $ fix $ \fn item ->
  case item of
    DataStream d -> extractByTemp d ".sit" fn
    FileName f   -> do
      tmpDir <- createTempDirectory
      canonTmp <- canonicalizePath tmpDir

      let script =    "  tell application \"StuffIt Expander\"\n"
                   ++ "    run\n"
                   ++ "    expand {POSIX file \"" ++ f ++ "\"}"
                   -- ++ " with delete originals"
                   ++ " to POSIX file \"" ++ canonTmp ++ "\"\n"
                   ++ "  end tell"

      loud <- isLoud
      when loud $ putStrLn "! invoke StuffIt Expander"

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
extract rpath overwrite = do
  path    <- canonicalizePath rpath
  pexists <- doesFileExist path
  unless pexists $ error $ "File does not exist: " ++ path
  
  destination <- getDestination

  fexists <- doesFileExist destination
  dexists <- doesDirectoryExist destination
  when (fexists || dexists) $
    if overwrite
    then if fexists
         then removeFile destination
         else removeDirectoryRecursive destination
    else unless (null typs) $
         error $ "Destination already exists: " ++ destination

  extract' typs (FileName path,return ())

  where (basename, typs) = findExtractors [] rpath
        
        getDestination = do 
          destpath <- getOption output
          return $ if null destpath
                   then basename
                   else destpath </> takeFileName basename
        
        -- The variations of extract' receive a list of archive types yet to
        -- be "unwrapped" from the previous extraction, plus a cleanup action
        -- which must be executed before the final result is returned.  The
        -- end result is that only the final extraction remains, indicated by
        -- the return value, with all temporaries having been properly cleaned
        -- up.

        extract' _  (x@(ArchiveError _),m) = do m; return x

        extract' [] (DataStream d,m) = do
          -- If we reach the final step of the unarchiving process, and
          -- the result is a data stream, write it to disk at the desired
          -- basename.
          destination <- getDestination
          B.writeFile destination d
          m; return $ FileName destination

        extract' [] (FileName f,m) = do
          destination <- getDestination
          -- Don't rename the file
          let realdest = dropFileName destination </> takeFileName f
          renameFile f realdest
          m; return $ FileName realdest

        extract' [] (DirectoryName f,m) = do
          destination <- getDestination
          renameDirectory f destination
          m; return $ DirectoryName destination

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
                 -> String      -- the temporary file extension
                 -> (Extraction -> IO ExtractionResult)
                                -- function to handle the new temp file
                 -> IO ExtractionResult
extractByTemp ds ext fn = do
  dir <- workingDirectory
  (path, handle) <- openBinaryTempFile dir ("file" ++ ext)
  loud <- isLoud
  when loud $ putStrLn $ "> " ++ path
  B.hPut handle ds
  hFlush handle
  (x,m) <- fn (FileName path)
  hClose handle
  return (x, do removeFile path; m)

createTempDirectory :: IO FilePath
createTempDirectory = do
  dir <- workingDirectory
  (path, handle) <- openBinaryTempFile dir "dir.tmp"
  hClose handle
  removeFile path
  createDirectory path
  return path

workingDirectory :: IO FilePath
workingDirectory = do
  sysp <- getOption sys_temp
  if sysp
    then getTemporaryDirectory
    else do dir <- getOption temp
            if null dir
              then return "."
              else return dir

removeFilePath :: FilePath -> IO ()
removeFilePath path = do
  fexists <- doesFileExist path
  dexists <- doesDirectoryExist path
  when (fexists || dexists) $
    if fexists
    then removeFile path
    else removeDirectoryRecursive path

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
        createProcess (proc cmd args){ std_in  = CreatePipe
                                     , std_out = CreatePipe
                                     , std_err = CreatePipe }

    outMVar <- newEmptyMVar

    -- fork off a thread to start consuming stdout
    hSetBinaryMode outh True
    out  <- B.hGetContents outh
    _ <- forkIO $ C.evaluate (B.length out) >> putMVar outMVar ()

    -- fork off a thread to start consuming stderr
    hSetBinaryMode errh False
    err  <- hGetContents errh
    _ <- forkIO $ C.evaluate (length err) >> putMVar outMVar ()

    -- now write and flush any input
    loud <- isLoud
    if loud
      then if B.null input
           then putStrLn $ "! " ++ cmd ++ " " ++ unwords args
           else do putStrLn $ "| " ++ cmd ++ " " ++ unwords args
                   B.hPutStr inh input; hFlush inh
      else unless (B.null input) $ do B.hPutStr inh input; hFlush inh
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
