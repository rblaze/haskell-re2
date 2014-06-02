{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V
import           Test.Chell

import           Regex.RE2

main :: IO ()
main = Test.Chell.defaultMain [tests]

tests :: Suite
tests = suite "re2"
	[ test_CompileSuccess
	, test_CompileFailure
	, test_PatternGroups
	, test_Find
	, test_Replace
	, test_ReplaceAll
	, test_Extract
	, test_OptionEncoding
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

test_PatternGroups :: Test
test_PatternGroups = assertions "patternGroups" $ do
	p <- $requireRight (compile defaultOptions (b "^(foo)(?P<named1>bar)(?P<named2>baz)$"))
	$expect (equal (patternGroups p) (V.fromList
		[ Nothing
		, Just (b "named1")
		, Just (b "named2")
		]))

test_Find :: Test
test_Find = assertions "find" $ do
	p <- $requireRight (compile defaultOptions (b "(foo)|(\\d)(\\d)\\d"))
	let found = find p (b "abc 123")
	$assert (just found)
	let Just match = found
	$expect (equal (matchGroups match) (V.fromList
		[ Just (b "123")
		, Nothing
		, Just (b "1")
		, Just (b "2")
		]))

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

test_OptionEncoding :: Test
test_OptionEncoding = assertions "optionEncoding" $ do
	let utfOpts = defaultOptions
	utfP <- $requireRight (compile utfOpts (b "^(.)"))
	$expect (equal (extract utfP (b "\xCE\xBB") (b "\\1")) (Just (b "\xCE\xBB")))
	
	let latinOpts = defaultOptions { optionEncoding = EncodingLatin1 }
	latinP <- $requireRight (compile latinOpts (b "^(.)"))
	$expect (equal (extract latinP (b "\xCE\xBB") (b "\\1")) (Just (b "\xCE")))

test_QuoteMeta :: Test
test_QuoteMeta = assertions "quoteMeta" $ do
	$expect (equal (quoteMeta (b "^foo$")) (b "\\^foo\\$"))
	$expect (equal (quoteMeta (b "^f\NULoo$")) (b "\\^f\\x00oo\\$"))

b :: String -> B.ByteString
b = B.pack
