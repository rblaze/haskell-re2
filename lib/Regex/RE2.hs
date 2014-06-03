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
	, patternGroups
	, replace
	, replaceAll
	, extract
	, quoteMeta
	
	, Match
	, Anchor(..)
	, matchGroup
	, matchGroups
	, match
	, find
	
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
	
	, Encoding(..)
	, optionEncoding
	) where

import           Control.Exception (bracket, mask_)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Unsafe as B
import           Data.Int
import           Data.String (IsString, fromString)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as V
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

data Encoding
	= EncodingUtf8
	| EncodingLatin1
	deriving (Eq, Show)

data Options = Options
	{ optionEncoding :: Encoding
	, optionPosixSyntax :: Bool
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
	{ optionEncoding = EncodingUtf8
	, optionPosixSyntax = False
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
	c_setopt_encoding ptr (case optionEncoding opts of
		EncodingUtf8 -> 1
		EncodingLatin1 -> 2)
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

foreign import ccall unsafe "haskell-re2.h haskell_re2_setopt_encoding"
	c_setopt_encoding :: Ptr Options -> CInt -> IO ()

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

patternGroups :: Pattern -> V.Vector (Maybe B.ByteString)
patternGroups (Pattern fptr) = unsafePerformIO $
	alloca $ \groupNamesPtr ->
	alloca $ \groupNameLensPtr ->
	withForeignPtr fptr $ \patternPtr -> do
		count <- c_pattern_groups patternPtr groupNamesPtr groupNameLensPtr
		if count == 0
			then return V.empty
			else do
				groupNames <- peek groupNamesPtr
				groupNameLens <- peek groupNameLensPtr
				peekPatternGroups (fromIntegral count) groupNames groupNameLens

peekPatternGroups :: Int -> Ptr CString -> Ptr CSize -> IO (V.Vector (Maybe B.ByteString))
peekPatternGroups groupCount groupNames groupNameLens = io where
	io = do
		vec <- V.new groupCount
		loop vec 0
		c_free groupNames
		c_free groupNameLens
		V.freeze vec
	loop _ idx | idx == groupCount = return ()
	loop vec idx = do
		cstr <- peekElemOff groupNames idx
		if cstr == nullPtr
			then V.write vec idx Nothing
			else do
				len <- peekElemOff groupNameLens idx
				bytes <- unsafePackMallocCStringSizeLen cstr len
				V.write vec idx (Just bytes)
		loop vec (idx+1)

foreign import ccall unsafe "stdlib.h free"
	c_free :: Ptr a -> IO ()

foreign import ccall unsafe "haskell-re2.h haskell_re2_pattern_groups"
	c_pattern_groups :: Ptr Pattern -> Ptr (Ptr CString) -> Ptr (Ptr CSize) -> IO CInt

newtype Match = Match (V.Vector (Maybe B.ByteString))
	deriving (Eq)

instance Show Match where
	showsPrec d (Match vec) = showParen (d > 10) (showString "Match " . shows (V.toList vec))

matchGroup :: Match -> Int -> Maybe B.ByteString
matchGroup (Match vals) idx = case vals V.!? idx of
	Nothing -> Nothing
	Just v -> v

matchGroups :: Match -> V.Vector (Maybe B.ByteString)
matchGroups (Match vals) = vals

data Anchor
	= AnchorStart
	| AnchorBoth
	deriving (Eq, Show)

match :: Pattern
      -> B.ByteString
      -> Int -- ^ Start position
      -> Int -- ^ End position
      -> Maybe Anchor
      -> Int -- ^ How many match groups to populate
      -> Maybe Match
match (Pattern fptr) input startPos endPos anchor maxCaptures = unsafePerformIO $
	alloca $ \capturesPtr ->
	alloca $ \captureLensPtr ->
	alloca $ \captureCountPtr ->
	unsafeUseAsCStringIntLen input $ \(inPtr, inLen) ->
	withForeignPtr fptr $ \patternPtr -> do
		let cStartPos = fromIntegral (min startPos c_INT_MAX)
		let cEndPos = fromIntegral (min endPos c_INT_MAX)
		let cAnchor = case anchor of
			Nothing -> 0
			Just AnchorStart -> 1
			Just AnchorBoth -> 2
		let cMaxCaptures = fromIntegral (max maxCaptures 0)
		matched <- c_match patternPtr inPtr inLen cStartPos cEndPos cAnchor cMaxCaptures capturesPtr captureLensPtr captureCountPtr
		if not matched
			then return Nothing
			else do
				captures <- peek capturesPtr
				captureLens <- peek captureLensPtr
				captureCount <- peek captureCountPtr
				vec <- peekCaptures (fromIntegral captureCount) captures captureLens
				return (Just (Match vec))

find :: Pattern -> B.ByteString -> Maybe Match
find (Pattern fptr) input = unsafePerformIO $
	alloca $ \capturesPtr ->
	alloca $ \captureLensPtr ->
	alloca $ \captureCountPtr ->
	unsafeUseAsCStringIntLen input $ \(inPtr, inLen) ->
	withForeignPtr fptr $ \patternPtr -> do
		matched <- c_match patternPtr inPtr inLen 0 inLen 0 (-1) capturesPtr captureLensPtr captureCountPtr
		if not matched
			then return Nothing
			else do
				captures <- peek capturesPtr
				captureLens <- peek captureLensPtr
				captureCount <- peek captureCountPtr
				vec <- peekCaptures (fromIntegral captureCount) captures captureLens
				return (Just (Match vec))

peekCaptures :: Int -> Ptr CString -> Ptr CSize -> IO (V.Vector (Maybe B.ByteString))
peekCaptures = peekPatternGroups

foreign import ccall "haskell-re2.h haskell_re2_match"
	c_match :: Ptr Pattern
	        -> CString -> CInt -- ^ Input
	        -> CInt -> CInt -- ^ startpos, endpos
	        -> CInt -- ^ anchor
	        -> CInt -- ^ num captures, -1 to capture all groups
	        -> Ptr (Ptr CString) -> Ptr (Ptr CSize) -- ^ Captures
	        -> Ptr CInt -- ^ How many groups were captured
	        -> IO Bool

replace :: Pattern -> B.ByteString -> B.ByteString -> (B.ByteString, Bool)
replace (Pattern fptr) input rewrite = unsafePerformIO $
	unsafeUseAsCStringSizeLen input $ \(inPtr, inLen) ->
	unsafeUseAsCStringIntLen rewrite $ \(rewritePtr, rewriteLen) ->
	alloca $ \outPtr ->
	alloca $ \outLenPtr ->
	withForeignPtr fptr $ \patternPtr -> do
		replaced <- c_replace patternPtr inPtr inLen rewritePtr rewriteLen outPtr outLenPtr
		if replaced
			then do
				out <- peek outPtr
				outLen <- peek outLenPtr
				outBytes <- unsafePackMallocCStringSizeLen out outLen
				return (outBytes, True)
			else return (input, False)

foreign import ccall unsafe "haskell-re2.h haskell_re2_replace"
	c_replace :: Ptr Pattern
	          -> CString -> CSize -- in, in_len
	          -> CString -> CInt  -- rewrite, rewrite_len
	          -> Ptr CString -> Ptr CSize -- out, out_len
	          -> IO Bool

replaceAll :: Pattern -> B.ByteString -> B.ByteString -> (B.ByteString, Int)
replaceAll (Pattern fptr) input rewrite = unsafePerformIO $
	unsafeUseAsCStringSizeLen input $ \(inPtr, inLen) ->
	unsafeUseAsCStringIntLen rewrite $ \(rewritePtr, rewriteLen) ->
	alloca $ \outPtr ->
	alloca $ \outLenPtr ->
	alloca $ \countPtr ->
	withForeignPtr fptr $ \patternPtr -> do
		c_global_replace patternPtr inPtr inLen rewritePtr rewriteLen outPtr outLenPtr countPtr
		count <- peek countPtr
		if count > 0
			then do
				out <- peek outPtr
				outLen <- peek outLenPtr
				outBytes <- unsafePackMallocCStringSizeLen out outLen
				return (outBytes, fromIntegral count)
			else return (input, 0)

foreign import ccall "haskell-re2.h haskell_re2_global_replace"
	c_global_replace :: Ptr Pattern
	                 -> CString -> CSize -- in, in_len
	                 -> CString -> CInt -- rewrite, rewrite_len
	                 -> Ptr CString -> Ptr CSize  -- out, out_len
	                 -> Ptr CInt  -- count
	                 -> IO ()

extract :: Pattern -> B.ByteString -> B.ByteString -> Maybe B.ByteString
extract (Pattern fptr) input rewrite = unsafePerformIO $
	unsafeUseAsCStringIntLen input $ \(inPtr, inLen) ->
	unsafeUseAsCStringIntLen rewrite $ \(rewritePtr, rewriteLen) ->
	alloca $ \outPtr ->
	alloca $ \outLenPtr ->
	withForeignPtr fptr $ \patternPtr -> do
		replaced <- c_extract patternPtr inPtr inLen rewritePtr rewriteLen outPtr outLenPtr
		if replaced
			then do
				out <- peek outPtr
				outLen <- peek outLenPtr
				outBytes <- unsafePackMallocCStringSizeLen out outLen
				return (Just outBytes)
			else return Nothing

foreign import ccall unsafe "haskell-re2.h haskell_re2_extract"
	c_extract :: Ptr Pattern
	          -> CString -> CInt -- in, in_len
	          -> CString -> CInt  -- rewrite, rewrite_len
	          -> Ptr CString -> Ptr CSize -- out, out_len
	          -> IO Bool

quoteMeta :: B.ByteString -> B.ByteString
quoteMeta input = unsafePerformIO $
	unsafeUseAsCStringIntLen input $ \(inPtr, inLen) ->
	alloca $ \outPtr ->
	alloca $ \outLenPtr -> do
		c_quote_meta inPtr inLen outPtr outLenPtr
		out <- peek outPtr
		outLen <- peek outLenPtr
		unsafePackMallocCStringSizeLen out outLen

foreign import ccall "haskell-re2.h haskell_re2_quote_meta"
	c_quote_meta :: CString -> CInt -> Ptr CString -> Ptr CSize -> IO ()

-- note: we assume that:
--   (maxBound::Int) >= (maxBound::CInt)
--   (maxBound::Int) <= (maxBound::CSize)
--
-- This is not technically correct, because the Haskell spec permits
-- (maxBound::Int) to be as small as 2^29-1. However, it is correct in
-- compilers such as GHC that use machine ints for Int.
unsafeUseAsCStringIntLen :: B.ByteString -> ((CString, CInt) -> IO a) -> IO a
unsafeUseAsCStringSizeLen :: B.ByteString -> ((CString, CSize) -> IO a) -> IO a
unsafePackMallocCStringSizeLen :: CString -> CSize -> IO B.ByteString

c_INT_MAX :: Int
c_INT_MAX = fromIntegral (maxBound :: CInt)

hs_INT_MAX :: CSize
hs_INT_MAX = fromIntegral (maxBound :: Int)

unsafeUseAsCStringIntLen bytes fn = B.unsafeUseAsCStringLen bytes (\(ptr, rawLen) -> if rawLen > c_INT_MAX
	then error ("re2: bytestring length " ++ show rawLen ++ " exceeds INT_MAX")
	else fn (ptr, fromIntegral rawLen))

unsafeUseAsCStringSizeLen bytes fn = B.unsafeUseAsCStringLen bytes (\(ptr, rawLen) -> fn (ptr, fromIntegral rawLen))

unsafePackMallocCStringSizeLen ptr len = if len > hs_INT_MAX
	then error ("re2: std::string length " ++ show len ++ " exceeds (maxBound::Int)")
	else B.unsafePackMallocCStringLen (ptr, fromIntegral len)
