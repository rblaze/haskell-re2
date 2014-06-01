{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.ByteString.Char8 as B
import           Test.Chell

import           Regex.RE2

main :: IO ()
main = Test.Chell.defaultMain [tests]

tests :: Suite
tests = suite "re2"
	[ test_CompileSuccess
	, test_CompileFailure
	, test_QuoteMeta
	]

test_CompileSuccess :: Test
test_CompileSuccess = assertions "compile.success" $ do
	let compiled = compile defaultOptions (b "^foo$")
	$assert (right compiled)
	let Right p = compiled
	$expect (equal (patternInput p) (b "^foo$"))

test_CompileFailure :: Test
test_CompileFailure = assertions "compile.failure" $ do
	let compiled = compile defaultOptions (b "^f(oo$")
	$assert (left compiled)
	let Left err = compiled
	$expect (equal (errorMessage err) "missing ): ^f(oo$")
	$expect (equal (errorCode err) ErrorMissingParen)

test_QuoteMeta :: Test
test_QuoteMeta = assertions "quoteMeta" $ do
	$expect (equal (quoteMeta (b "^foo$")) (b "\\^foo\\$"))
	$expect (equal (quoteMeta (b "^f\NULoo$")) (b "\\^f\\x00oo\\$"))

b :: String -> B.ByteString
b = B.pack
