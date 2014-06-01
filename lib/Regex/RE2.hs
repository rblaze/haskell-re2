{-# LANGUAGE ForeignFunctionInterface #-}

-- |
--
-- TODO: write docs
module Regex.RE2
	( Pattern
	, Error
	, Options
	, defaultOptions
	, compile
	, replace
	, replaceAll
	, extract
	, quoteMeta
	) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import           Data.String (IsString, fromString)
import           Foreign.C
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO.Unsafe (unsafePerformIO)

data Pattern = Pattern

instance Eq Pattern where

instance Show Pattern where

instance IsString Pattern where

data Options = Options

defaultOptions :: Options
defaultOptions = undefined

data Error = Error

compile :: Options -> B.ByteString -> Either Error Pattern
compile = undefined

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
