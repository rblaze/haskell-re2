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
	, test_Replace
	, test_ReplaceAll
	, test_Extract
	, test_QuoteMeta
	]

test_CompileSuccess :: Test
test_CompileSuccess = assertions "compile.success" $ do
	p <- $requireRight (compile defaultOptions (b "^foo$"))
	$expect (equal (patternInput p) (b "^foo$"))

test_CompileFailure :: Test
test_CompileFailure = assertions "compile.failure" $ do
	err <- $requireLeft (compile defaultOptions (b "^f(oo$"))
	$expect (equal (errorMessage err) "missing ): ^f(oo$")
	$expect (equal (errorCode err) ErrorMissingParen)

test_Replace :: Test
test_Replace = assertions "replace" $ do
	p <- $requireRight (compile defaultOptions (b "foo"))
	$expect (equal (replace p (b "no match") (b "baz")) (b "no match", False))
	$expect (equal (replace p (b "foo bar foo bar") (b "baz")) (b "baz bar foo bar", True))

test_ReplaceAll :: Test
test_ReplaceAll = assertions "replaceAll" $ do
	p <- $requireRight (compile defaultOptions (b "foo"))
	$expect (equal (replaceAll p (b "no match") (b "baz")) (b "no match", 0))
	$expect (equal (replaceAll p (b "foo bar foo bar") (b "baz")) (b "baz bar baz bar", 2))

test_Extract :: Test
test_Extract = assertions "extract" $ do
	p <- $requireRight (compile defaultOptions (b "(foo)"))
	$expect (equal (extract p (b "no match") (b "baz")) Nothing)
	$expect (equal (extract p (b "foo bar foo bar") (b "baz")) (Just (b "baz")))
	$expect (equal (extract p (b "foo bar foo bar") (b "\\1baz")) (Just (b "foobaz")))

test_QuoteMeta :: Test
test_QuoteMeta = assertions "quoteMeta" $ do
	$expect (equal (quoteMeta (b "^foo$")) (b "\\^foo\\$"))
	$expect (equal (quoteMeta (b "^f\NULoo$")) (b "\\^f\\x00oo\\$"))

b :: String -> B.ByteString
b = B.pack
