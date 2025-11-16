# CLAUDE.md - AI Assistant Guide for the Una Project

This file contains specific instructions for AI assistants working on this codebase.

## Project Overview

**Una** is a universal unarchiver utility written in Haskell that automatically extracts various archive formats. It handles nested compressed archives (e.g., `.tar.gz`) through a recursive extraction pipeline.

## ⚠️ CRITICAL ISSUES - FIX IMMEDIATELY

Before making ANY other changes, these critical bugs MUST be addressed:

### 1. Missing `isLoud` Function (CRITICAL BUG)
**Problem**: The function `isLoud` is used 4 times but never defined.
**Location**: Lines 342, 361, 541, 607 in Main.hs
**Fix**:
```haskell
-- Add this function after getOption (around line 142)
isLoud :: IO Bool
isLoud = getOption verbose
-- Note: This assumes a 'verbose' field should be added to UnaOpts
```

### 2. Unsafe `fromJust` Usage
**Problem**: Will crash if tool not found or option missing
**Locations**: Lines 141, 207
**Fix**:
```haskell
-- Line 207: Replace bestExe with safe version
bestExe :: [String] -> IO (Either String String)
bestExe xs = do
    results <- traverse findExecutable xs
    case msum results of
        Nothing -> return $ Left $ "No tool found: " ++ show xs
        Just path -> return $ Right path

-- Update callers to handle Either result
gzipExtractor = Extractor False $ \x -> do
    exePathE <- bestExe ["pigz", "gzip"]
    case exePathE of
        Left err -> return (ArchiveError err, noCleanup)
        Right exePath -> simpleExtractor exePath ["-qdc"] x
```

### 3. Unsafe `head` Usage
**Problem**: Crashes on empty list
**Location**: Line 381
**Fix**:
```haskell
-- Replace assertion with proper pattern matching
case elems of
    [single] -> return $ canonTmp </> single
    [] -> return (ArchiveError "Empty archive", cleanup)
    multiple -> return (ArchiveError $ "Expected single file, got: " ++ show multiple, cleanup)
```

### 4. Hidden Pattern Match Warnings
**Problem**: `-fno-warn-incomplete-patterns` hides bugs
**Fix**: Remove this pragma from line 6 and fix ALL incomplete patterns

## Common Development Commands

### Building
```bash
# Using Cabal (if cabal file exists)
cabal build

# Using Stack (if stack.yaml exists)
stack build

# Using Nix (flake.nix and default.nix present)
nix-build
nix build  # with flake

# Build and run directly (development)
runhaskell Main.hs archive.tar.gz
```

### Running the Application
```bash
# After building with Cabal
./.cabal-sandbox/bin/una archive.tar.gz
cabal run una -- archive.tar.gz

# After building with Stack
stack exec una -- archive.tar.gz

# With Nix
./result/bin/una archive.tar.gz

# Direct execution (script mode - line 1: #!/usr/bin/env runhaskell)
./Main.hs archive.tar.gz
```

### Testing (Currently No Tests)
```bash
# Would run: cabal test
# Would run: stack test
# Need to create test/ directory first
```

### Installing External Tool Dependencies
```bash
# macOS (via MacPorts as mentioned in README)
sudo port install unarj unrar lha p7zip cabextract

# macOS (via Homebrew)
brew install p7zip unrar cabextract

# Linux (Debian/Ubuntu)
sudo apt-get install p7zip-full unrar cabextract lha unar
```

## Architecture - Cross-File Understanding

### The Extraction Pipeline (Requires Understanding Multiple Components)

The extraction logic spans several interconnected functions that must be understood together:

1. **Entry Point** (`main` → `extract`): CLI parsing feeds into extraction
2. **Extension Analysis** (`findExtractors`): Recursively strips extensions to build extractor chain
3. **Recursive Processing** (`extract'`): Applies extractors in sequence, handling cleanup
4. **Content Normalization** (`examineContents`): Ensures single file/directory output
5. **Resource Management** (`extractByTemp`, `createTempDirectory`): Handles intermediate files

#### Key Interaction: Global State via io-storage
The `io-storage` library creates hidden dependencies between functions:
- `main` stores options globally: `putValue "main" "opts" opts`
- Deep functions read it: `getOption` calls `getValue "main" "opts"`
- This breaks when running concurrent extractions (not thread-safe)

#### Key Interaction: Cleanup Chain
Cleanup actions propagate through the extraction pipeline:
```haskell
-- extract' accumulates cleanup actions through recursion
extract' (t:ts) (x,m) = wrap m $ do  -- 'm' is previous cleanup
    y <- extractor t x                -- Returns new (result, cleanup)
    extract' ... y                    -- Passes both forward
```

#### Key Interaction: Stream vs File Duality
Extractors must handle two input types because some tools can't read stdin:
- **Stream-capable** (tar, 7z): Can pipe data through
- **File-only** (zip, cab): Must write stream to temp file first
- This is why `extractByTemp` exists - converts streams to files for dumb tools

## Adding New Archive Formats

### Step 1: Add Extension Mapping
```haskell
-- In extractors list (line 148)
extractors :: [(String, [Extractor])]
extractors = [
    ...
    , (".newext", [newExtExtractor])  -- Add your format
]
```

### Step 2: Implement Extractor
Choose the appropriate pattern based on tool capabilities:

#### Pattern A: Simple Streaming Compressor (like gzip)
```haskell
newExtExtractor = Extractor False $ \x -> do
    exePath <- bestExe ["tool1", "tool2"]  -- Fix: Handle Either result!
    simpleExtractor exePath ["-decompress"] x
```

#### Pattern B: Archive with Directory Support (like tar)
```haskell
newExtExtractor = Extractor True $ \item -> do
    dir <- createTempDirectory
    case item of
        DataStream d ->
            performExtract "tool" ["extract", "-o", dir, "-"] d (returnContents dir)
        FileName f ->
            performExtract "tool" ["extract", "-o", dir, f] B.empty (returnContents dir)
```

#### Pattern C: Dumb Extractor (no streaming, no directory control)
```haskell
newExtExtractor = Extractor True $ dumbExtractor "tool" "x" ".ext"
```

### Step 3: Test Your Extractor
```haskell
-- Create test archive
echo "test content" > test.txt
tool compress test.txt test.newext

-- Test extraction
./una test.newext

-- Verify output
cat test.txt  # Should show "test content"
```


## Common Pitfalls to Avoid

### 1. Global State (io-storage)
**NEVER** use io-storage for new code. Use ReaderT instead:
```haskell
-- Bad (current)
getOption :: Data a => (a -> b) -> IO b
getOption option = do
    opts <- getValue "main" "opts"  -- Global state!
    return $ fromJust $ option <$> opts

-- Good
type UnaM = ReaderT UnaOpts IO

getOption :: (UnaOpts -> a) -> UnaM a
getOption selector = asks selector
```

### 2. Silent Failures
**ALWAYS** report errors clearly:
```haskell
-- Bad
performExtract cmd args ds fn = do
    (exit, out, err) <- bReadProcessWithExitCode cmd args ds
    if exit == ExitSuccess
        then fn out
        else return (ArchiveError err, noCleanup)  -- Error hidden!

-- Good
performExtract cmd args ds fn = do
    (exit, out, err) <- bReadProcessWithExitCode cmd args ds
    case exit of
        ExitSuccess -> fn out
        ExitFailure code -> do
            hPutStrLn stderr $ "Error: " ++ cmd ++ " failed with code " ++ show code
            hPutStrLn stderr $ "stderr: " ++ err
            return (ArchiveError $ cmd ++ " failed: " ++ err, noCleanup)
```

### 3. Missing Tool Validation
**ALWAYS** check for required tools on startup:
```haskell
validateTools :: IO [String]
validateTools = do
    let required = ["tar", "gzip", "unzip"]
    missing <- filterM (fmap isNothing . findExecutable) required
    unless (null missing) $ do
        putStrLn $ "Missing required tools: " ++ intercalate ", " missing
        putStrLn "Install with: brew install <tool>"
        exitFailure
    return missing
```


## External Tool Requirements

### Core Tools (Required)
- `tar` - Archive handling
- `gzip` or `pigz` - Gzip compression
- `bzip2` or `pbzip2` - Bzip2 compression
- `unzip` - ZIP archives

### Optional Tools (Format-specific)
- `7za` - 7-Zip archives
- `unrar` - RAR archives
- `cabextract` - CAB archives
- `xz` or `pxz` - XZ compression
- `lha` - LHA/LZH archives
- `unarj` - ARJ archives

### Platform-Specific
- macOS: `hdiutil`, `ditto` - Disk images
- macOS: StuffIt Expander - Legacy Mac formats


## Example Modifications

### Example 1: Adding verbose flag
```haskell
-- Step 1: Add to UnaOpts
data UnaOpts = UnaOpts
    { verbose :: Bool  -- Add this
    , delete_ :: Bool
    ...
    }

-- Step 2: Add to command line options
unaOpts = UnaOpts
    { verbose = def &= name "v" &= help "Verbose output"
    , ...
    }

-- Step 3: Implement isLoud
isLoud :: IO Bool
isLoud = getOption verbose
```

### Example 2: Adding new format (.zst for Zstandard)
```haskell
-- Step 1: Add to extractors
extractors = [
    ...
    , (".zst", [zstdExtractor])
]

-- Step 2: Implement extractor
zstdExtractor = Extractor False $ \x -> do
    exePath <- bestExe ["zstd"]
    case exePath of
        Left err -> return (ArchiveError $ "zstd not found: " ++ err, noCleanup)
        Right path -> simpleExtractor path ["-qdc"] x
```

### Example 3: Better error reporting
```haskell
-- Create error type
data UnaError
    = FileNotFound FilePath
    | ToolMissing String [String]
    | ExtractionFailed String Int String  -- tool, exit code, stderr
    deriving (Show, Eq)

-- Use in extraction
performExtract :: String -> [String] -> B.ByteString
               -> (B.ByteString -> IO ExtractionResult)
               -> IO ExtractionResult
performExtract cmd args input handler = do
    result <- try $ bReadProcessWithExitCode cmd args input
    case result of
        Left (e :: IOException) ->
            return (ArchiveError $ "Failed to run " ++ cmd ++ ": " ++ show e, noCleanup)
        Right (ExitSuccess, out, _) ->
            handler out
        Right (ExitFailure code, _, err) ->
            return (ArchiveError $ cmd ++ " failed (exit " ++ show code ++ "): " ++ err, noCleanup)
```

