# 0.2

- New function for listing directory contents recursively ‘getDirectoryContentsRecursive’
- New function for defining custom recursive directory traversals ‘listContentsRecFold’
- ‘readDirStream’ now returns file type in addition to basename
- ‘DirStream’ is now safe to close multiple times and it will be automatically closed by GC when it becomes unreachable
- The ‘FileType’ type now has only 3 constructors, symlink status is now field of some of them

# 0.1.0.3

- Lower `base` minimum required base to 4.12 (GHC 8.6). Minimum
  supported `unix` is still 2.8 because of `OsString`

# 0.1.0.2

- Fix compatibility with `filepath-1.5`

# 0.1.0.1

- Add missing test inputs

# 0.1

Initial release
