{-# LANGUAGE ForeignFunctionInterface #-}

-- |
--
-- TODO: write docs
module Regex.RE2
	( Pattern
	, Error
	, ErrorCode(..)
	, errorMessage
	, errorCode
	, compile
	, patternInput
	, replace
	, replaceAll
	, extract
	, quoteMeta
	
	, Options
	, defaultOptions
	, optionPosixSyntax
	, optionLongestMatch
	, optionMaxMemory
	, optionLiteral
	, optionNeverNewline
	, optionNeverCapture
	, optionCaseSensitive
	, optionPerlClasses
	, optionWordBoundary
	, optionOneLine
	) where

import           Control.Exception (bracket, mask_)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Unsafe as B
import           Data.Int
import           Data.String (IsString, fromString)
import           Foreign.C
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO.Unsafe (unsafePerformIO)

newtype Pattern = Pattern (ForeignPtr Pattern)

instance Show Pattern where
	showsPrec d pattern = showParen (d > 10) (showString "Pattern " . shows (patternInput pattern))

instance IsString Pattern where
	fromString s = case compile defaultOptions (B8.pack s) of
		Left err -> error ("re2: failed to compile pattern " ++ show s ++ ": " ++ errorMessage err)
		Right pattern -> pattern

data Error = Error ErrorCode String
	deriving (Eq, Show)

data ErrorCode
	= ErrorInternal
	| ErrorBadEscape
	| ErrorBadCharClass
	| ErrorBadCharRange
	| ErrorMissingBracket
	| ErrorMissingParen
	| ErrorTrailingBackslash
	| ErrorRepeatArgument
	| ErrorRepeatSize
	| ErrorRepeatOp
	| ErrorBadPerlOp
	| ErrorBadUTF8
	| ErrorBadNamedCapture
	| ErrorPatternTooLarge
	deriving (Eq, Show)

errorCode :: Error -> ErrorCode
errorCode (Error x _) = x

errorMessage :: Error -> String
errorMessage (Error _ x) = x

data Options = Options
	{ optionPosixSyntax :: Bool
	, optionLongestMatch :: Bool
	, optionMaxMemory :: Int64
	, optionLiteral :: Bool
	, optionNeverNewline :: Bool
	, optionDotNewline :: Bool
	, optionNeverCapture :: Bool
	, optionCaseSensitive :: Bool
	
	-- only checked in posix mode
	, optionPerlClasses :: Bool
	, optionWordBoundary :: Bool
	, optionOneLine :: Bool
	}

defaultOptions :: Options
defaultOptions = Options
	{ optionPosixSyntax = False
	, optionLongestMatch = False
	, optionMaxMemory = 8388608  -- 8 << 20
	, optionLiteral = False
	, optionNeverNewline = False
	, optionDotNewline = False
	, optionNeverCapture = False
	, optionCaseSensitive = True
	, optionPerlClasses = False
	, optionWordBoundary = False
	, optionOneLine = False
	}

withOptions :: Options -> (Ptr Options -> IO a) -> IO a
withOptions opts io = bracket c_alloc_options c_free_options $ \ptr -> do
	c_setopt_posix_syntax ptr (optionPosixSyntax opts)
	c_setopt_longest_match ptr (optionLongestMatch opts)
	c_setopt_max_mem ptr (optionMaxMemory opts)
	c_setopt_literal ptr (optionLiteral opts)
	c_setopt_never_nl ptr (optionNeverNewline opts)
	c_setopt_dot_nl ptr (optionDotNewline opts)
	c_setopt_never_capture ptr (optionNeverCapture opts)
	c_setopt_case_sensitive ptr (optionCaseSensitive opts)
	c_setopt_perl_classes ptr (optionPerlClasses opts)
	c_setopt_word_boundary ptr (optionWordBoundary opts)
	c_setopt_one_line ptr (optionOneLine opts)
	io ptr

foreign import ccall unsafe "haskell-re2.h haskell_re2_alloc_options"
	c_alloc_options :: IO (Ptr Options)

foreign import ccall unsafe "haskell-re2.h haskell_re2_free_options"
	c_free_options :: Ptr Options -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_posix_syntax"
	c_setopt_posix_syntax :: Ptr Options -> Bool -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_longest_match"
	c_setopt_longest_match :: Ptr Options -> Bool -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_max_mem"
	c_setopt_max_mem :: Ptr Options -> Int64 -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_literal"
	c_setopt_literal :: Ptr Options -> Bool -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_never_nl"
	c_setopt_never_nl :: Ptr Options -> Bool -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_dot_nl"
	c_setopt_dot_nl :: Ptr Options -> Bool -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_never_capture"
	c_setopt_never_capture :: Ptr Options -> Bool -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_case_sensitive"
	c_setopt_case_sensitive :: Ptr Options -> Bool -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_perl_classes"
	c_setopt_perl_classes :: Ptr Options -> Bool -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_word_boundary"
	c_setopt_word_boundary :: Ptr Options -> Bool -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_one_line"
	c_setopt_one_line :: Ptr Options -> Bool -> IO ()

compile :: Options -> B.ByteString -> Either Error Pattern
compile opts input = unsafePerformIO $ withOptions opts $ \optsPtr -> do
	fptr <- mask_ $ do
		p <- unsafeUseAsCStringIntLen input (\(inPtr, inLen) -> c_compile_pattern optsPtr inPtr inLen)
		newForeignPtr c_delete_pattern p
	withForeignPtr fptr $ \ptr -> do
		errPtr <- c_error ptr
		if errPtr == nullPtr
			then return (Right (Pattern fptr))
			else do
				err <- peekCString errPtr
				errCodeInt <- c_error_code ptr
				let errCode = case errCodeInt of
					2 -> ErrorBadEscape
					3 -> ErrorBadCharClass
					4 -> ErrorBadCharRange
					5 -> ErrorMissingBracket
					6 -> ErrorMissingParen
					7 -> ErrorTrailingBackslash
					8 -> ErrorRepeatArgument
					9 -> ErrorRepeatSize
					10 -> ErrorRepeatOp
					11 -> ErrorBadPerlOp
					12 -> ErrorBadUTF8
					13 -> ErrorBadNamedCapture
					14 -> ErrorPatternTooLarge
					_ -> ErrorInternal
				return (Left (Error errCode err))

foreign import ccall unsafe "haskell-re2.h haskell_re2_compile_pattern"
	c_compile_pattern :: Ptr Options -> CString -> CInt -> IO (Ptr Pattern)

foreign import ccall unsafe "haskell-re2.h &haskell_re2_delete_pattern"
	c_delete_pattern :: FunPtr (Ptr Pattern -> IO ())

foreign import ccall unsafe "haskell-re2.h haskell_re2_error"
	c_error :: Ptr Pattern -> IO CString

foreign import ccall unsafe "haskell-re2.h haskell_re2_error_code"
	c_error_code :: Ptr Pattern -> IO CInt

patternInput :: Pattern -> B.ByteString
patternInput (Pattern fptr) = unsafePerformIO $
	withForeignPtr fptr $ \ptr -> do
		cstr <- c_pattern_input ptr
		B.packCString cstr

foreign import ccall unsafe "haskell-re2.h haskell_re2_pattern_input"
	c_pattern_input :: Ptr Pattern -> IO CString

replace :: Pattern -> B.ByteString -> B.ByteString -> (B.ByteString, Bool)
replace = undefined

replaceAll :: Pattern -> B.ByteString -> B.ByteString -> (B.ByteString, Integer)
replaceAll = undefined

extract :: Pattern -> B.ByteString -> B.ByteString -> Maybe B.ByteString
extract = undefined

quoteMeta :: B.ByteString -> B.ByteString
quoteMeta input = unsafePerformIO $
	unsafeUseAsCStringIntLen input $ \(inPtr, inLen) ->
	alloca $ \outPtr ->
	alloca $ \outLenPtr -> do
		c_quote_meta inPtr inLen outPtr outLenPtr
		out <- peek outPtr
		outLen <- peek outLenPtr
		unsafePackMallocCStringIntLen out outLen

foreign import ccall "haskell-re2.h haskell_re2_quote_meta"
	c_quote_meta :: CString -> CInt -> Ptr CString -> Ptr CInt -> IO ()

-- note: we assume that (maxBound::Int) >= (maxBound::CInt)
--
-- This is not technically correct, because the Haskell spec permits
-- (maxBound::Int) to be as small as 2^29-1. However, it is correct in
-- compilers such as GHC that use machine ints for Int.
unsafePackMallocCStringIntLen :: CString -> CInt -> IO B.ByteString
unsafeUseAsCStringIntLen :: B.ByteString -> ((CString, CInt) -> IO a) -> IO a

c_INT_MAX :: Int
c_INT_MAX = fromIntegral (maxBound :: CInt)

unsafeUseAsCStringIntLen bytes fn = B.unsafeUseAsCStringLen bytes (\(ptr, rawLen) -> if rawLen > c_INT_MAX
	then error ("re2: bytestring length " ++ show rawLen ++ " exceeds INT_MAX")
	else fn (ptr, fromIntegral rawLen))

unsafePackMallocCStringIntLen ptr len = B.unsafePackMallocCStringLen (ptr, fromIntegral len)
