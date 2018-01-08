# haskell-re2

re2 is a regular expression library offering predictable run-time and memory consumption. This package is a binding to re2.

Supported expression syntax is documented at https://github.com/google/re2.
```
$ ghci -XOverloadedStrings
ghci> import Regex.RE2

ghci> find "\\w+" "hello world"
Just (Match [Just "hello"])

ghci> find "\\w+$" "hello world"
Just (Match [Just "world"])

ghci> find "^\\w+$" "hello world"
Nothing
```
