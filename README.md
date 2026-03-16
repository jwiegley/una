# una -- Universal un-archiver

I got tired of remembering which tool to use for which archive format.
`tar xzf`? `unzip`? `7za x`? `cabextract`? It's 2025 -- I just want to
point at a file and have the right thing happen. That's what `una` does.

Point it at any archive or compressed file, and it produces a single file
or directory in the current directory. No more archive bombs scattering
files everywhere. If the archive contains multiple top-level entries,
they're placed in a directory named after the archive. If it's a nested
format like `.tar.gz`, both layers are unwrapped automatically.

## Usage

```bash
una archive.tar.gz
una something.tar.bz2
una package.7z
una disk.dmg
```

That's basically it. Some useful options:

```
-d          Delete the original archive after successful extraction
-f          Overwrite existing files/directories
--verbose   Verbose output (show commands being run)
-o DIR      Extract to DIR instead of the current directory
-T          Use the system temp directory during extraction
--test      Test extraction without keeping the result (sets exit code)
```

So `una -d archive.tar.xz` extracts and cleans up the archive, and
`una -f blob.zip` overwrites whatever was there before.

## Supported formats

tar, gz, bz2, xz, Z, zip, jar, 7z, rar, arj, lha, lzh, cab, cpio,
shar, uu, a, gpg, asc, dmg, iso, cdr, sparseimage, sparsebundle,
sit, sea, bin, hqx, sdk, shk, bxy, bny, bqy

Compound extensions work too -- `.tar.gz`, `.tar.bz2`, `.tar.xz`, `.tgz`,
`.tbz`, `.txz`, and so on.

## External tools

`una` itself is just the dispatch logic. The actual extraction is done by
whatever tools you have installed. The basics (`tar`, `gzip`, `bzip2`,
`unzip`) are probably already on your system. For everything else:

**macOS (MacPorts):**
```bash
sudo port install cabextract unarj unrar lha p7zip
```

**macOS (Homebrew):**
```bash
brew install p7zip unrar cabextract
```

**Debian/Ubuntu:**
```bash
sudo apt-get install p7zip-full unrar cabextract lha unar
```

Where parallel versions exist (like `pigz` for gzip, `pbzip2` for bzip2,
`pxz` for xz), `una` prefers them for speed.

## Building

With Nix (recommended):

```bash
nix build
./result/bin/una archive.tar.gz
```

With Cabal:

```bash
cabal build
cabal run una -- archive.tar.gz
```

## Development

```bash
nix develop
```

This gives you GHC, Cabal, HLS, hlint, fourmolu, and lefthook. The whole
program is a single file (`Main.hs`), so there's not much ceremony involved.

Pre-commit hooks (via lefthook) check formatting and linting before each
commit. To format the code:

```bash
nix run .#format
```

`nix flake check` runs the full suite -- build, hlint, fourmolu, a
`-Werror` build, and integration tests against real archives.

## License

BSD-3-Clause. See [LICENSE.md](LICENSE.md) for details.
