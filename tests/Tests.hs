{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.ByteString.Char8 as B
import           Test.Chell

import qualified Regex.RE2 as RE2

main :: IO ()
main = Test.Chell.defaultMain [tests]

tests :: Suite
tests = suite "re2"
	[ test_QuoteMeta
	]

test_QuoteMeta :: Test
test_QuoteMeta = assertions "quoteMeta" $ do
	$expect (equal (RE2.quoteMeta (b "^foo$")) (b "\\^foo\\$"))

b :: String -> B.ByteString
b = B.pack
