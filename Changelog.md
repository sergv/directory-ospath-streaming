# 0.3

- Remove `DirReadCache` since it was not doing anything most of the time. It was only used for the `readdir_r` libc function which is deprecated and is not typically used by default in the `unix` package (cf https://github.com/haskell/unix/pull/349)
- Add new function `readDirStreamFull` that is like `readDirStream` but also returns full path to the directory entry to let clients reuse the full path from the stream root that would be created anyway

# 0.2.2

- Add `getDirectoryContentsWithFilterRecursive` for recursively listing directory contents with commonly needed filtering

# 0.2.1

- Fix `listContentsRecFold` to not mask exceptions unnecessarily which could cause hangups. The `getDirectoryContentsRecursive` gets the fix as well
- Make `closeDirStream` hold on to the stream so it’s not GC’ed prematurely causing errors on reads.

# 0.2

- New function for listing directory contents recursively `getDirectoryContentsRecursive`
- New function for defining custom recursive directory traversals `listContentsRecFold`
- `readDirStream` now returns file type in addition to basename
- `DirStream` is now safe to close multiple times and it will be automatically closed by GC when it becomes unreachable
- The `FileType` type now has only 3 constructors, symlink status is now field of some of them

# 0.1.0.3

- Lower `base` minimum required base to 4.12 (GHC 8.6). Minimum
  supported `unix` is still 2.8 because of `OsString`

# 0.1.0.2

- Fix compatibility with `filepath-1.5`

# 0.1.0.1

- Add missing test inputs

# 0.1

Initial release
