module System.Posix.MMap
       (
         withMMap
       , mmap
       , munmap
       , Protection(..)
       , MapFlag(..)
       , CVoid
       ) where

import Control.Exception
import Data.Bits
import Data.Int
import Data.Typeable
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt(..), CLLong(..), CSize(..))
import System.Posix.Types

data CVoid

foreign import ccall "system_posix_mmap_PROT_NONE"   c_system_posix_mmap_PROT_NONE :: CInt
foreign import ccall "system_posix_mmap_PROT_READ"   c_system_posix_mmap_PROT_READ :: CInt
foreign import ccall "system_posix_mmap_PROT_WRITE"  c_system_posix_mmap_PROT_WRITE :: CInt
foreign import ccall "system_posix_mmap_PROT_EXEC"   c_system_posix_mmap_PROT_EXEC :: CInt

foreign import ccall "system_posix_mmap_MAP_SHARED"  c_system_posix_mmap_MAP_SHARED :: CInt
foreign import ccall "system_posix_mmap_MAP_PRIVATE" c_system_posix_mmap_MAP_PRIVATE :: CInt

foreign import ccall "system_posix_mmap_mmap"        c_system_posix_mmap_mmap :: CSize -> CInt -> CInt -> CInt -> CLLong -> IO (Ptr CVoid)
foreign import ccall "system_posix_mmap_munmap"      c_system_posix_mmap_munmap :: Ptr CVoid -> CSize -> IO ()

data Protection = PROT_NONE
                | PROT_READ
                | PROT_WRITE
                | PROT_EXEC
                deriving (Eq, Show, Typeable)

data MapFlag = MAP_SHARED
             | MAP_PRIVATE
             deriving (Eq, Show, Typeable)

protections :: [Protection] -> CInt
protections = foldr ((.|.) . f) 0 where
  f PROT_NONE  = c_system_posix_mmap_PROT_NONE
  f PROT_READ  = c_system_posix_mmap_PROT_READ
  f PROT_WRITE = c_system_posix_mmap_PROT_WRITE
  f PROT_EXEC  = c_system_posix_mmap_PROT_EXEC

mapFlags :: [MapFlag] -> CInt
mapFlags = foldr ((.|.) . f) 0 where
  f MAP_SHARED  = c_system_posix_mmap_MAP_SHARED
  f MAP_PRIVATE = c_system_posix_mmap_MAP_PRIVATE

mmap :: Int -> [Protection] -> [MapFlag] -> Fd -> Int64 -> IO (Ptr CVoid)
mmap size prots flags fd offset = c_system_posix_mmap_mmap (fromIntegral size) (protections prots) (mapFlags flags) (fromIntegral fd) (fromIntegral offset)

munmap :: Ptr CVoid -> Int -> IO ()
munmap ptr size = c_system_posix_mmap_munmap ptr (fromIntegral size)

withMMap :: Int -> [Protection] -> [MapFlag] -> Fd -> Int64 -> (Ptr CVoid -> IO a) -> IO a
withMMap size prots flags fd offset = bracket acquire release where
  acquire = mmap size prots flags fd offset
  release = flip munmap size
