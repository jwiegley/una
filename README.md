una: Universal un-archiver (mainly tested on Mac OS X for now)

Version 1.0, by John Wiegley <johnw@newartisans.com>

This is a "universal", recursive unarchiver, written because I'm too lazy to
remember all the extraction options for the large number of archive formats I
deal with.

Optional dependencies:
  StuffIt Expander (free, expander-only version)
  MacPorts: unarj, unrar, lha, p7zip, cabextract

.h2 Usage

una [OPTION] ARCHIVE...

If no OPTION is specified, the default action is to extract the archive's
contents into the current directory.

Options:
  -h, --help        show help
  -d, --delete      delete the archive if sucessfully extracted
  -f, --overwrite   overwrite any existing file

This script is also smart about unarchiving:

  a) if all the contents of an archive would already extract into a single
     directory, do that;

  b) if the archive contains only one item, extract it into the current
     directory;

  c) otherwise, if the archive would dump multiple contents into the current
     directory, create a new directory based on the name of the archive,
     sans extension, and put everything there.
