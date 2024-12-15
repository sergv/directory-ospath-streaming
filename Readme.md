[![build](https://github.com/sergv/directory-ospath-streaming/actions/workflows/haskell-ci.yaml/badge.svg)](https://github.com/sergv/directory-ospath-streaming/actions/workflows/haskell-ci.yaml)

# Synopsis

Reading of directory contents in constant memory, i.e. in an iterative
fashion without storing all directory elements in memory. From another
perspective, this reading interface allows stopping at any point
without loading every directory element.

Also defines general-purpose recursive directory traversals.

Both Windows and Unix systems are supported.
