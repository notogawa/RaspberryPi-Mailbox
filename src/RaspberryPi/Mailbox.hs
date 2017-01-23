{-# LANGUAGE MultiParamTypeClasses #-}
-- https://github.com/raspberrypi/firmware/wiki/Mailbox-property-interface
module RaspberryPi.Mailbox
       (
         Mailbox
       , withMailbox

       , getFirmwareRevision

       , getBoardModel
       , getBoardRevision
       , getBoardMACAddress
       , getBoardSerial
       , getARMMemory
       , getVCMemory

       , getDMAChannels
       , DeviceId(..)
       , DeviceState(..)
       , getPowerState
       , getTiming
       , setPowerState

       , ClockId(..)
       , ClockState(..)
       , getClockState
       , setClockState
       , getClockRate
       , setClockRate
       , getMaxClockRate
       , getMinClockRate
       , getTurbo
       , setTurbo

       , VoltageId(..)
       , getVoltage
       , setVoltage
       , getMaxVoltage
       , getMinVoltage

       , getTemperature
       , getMaxTemperature

       , withCPUMappedGPUMemory
       , MappedPtr(..)

       , withGPUMemory
       , allocateGPUMemory
       , releaseGPUMemory

       , withLockedGPUMemory
       , lockGPUMemory
       , unlockGPUMemory

       , withQPU
       , enableQPU
       , disableQPU

       , executeQPU
       ) where

import Control.Applicative
import Control.Exception (bracket, bracket_)
import Control.Monad
import Data.Typeable
import Foreign
import System.Posix.IO
import System.Posix.IOCtl
import System.Posix.Types
import System.Posix.MMap
import Foreign.C.Types (CInt(..))

foreign import ccall "raspberrypi_mailbox_ioctlReq"   c_raspberrypi_mailbox_ioctlReq :: CInt

newtype Mailbox = Mailbox { unMailbox :: Fd }

withMailbox :: (Mailbox -> IO a) -> IO a
withMailbox = bracket acquire release where
  acquire = Mailbox <$> openFd "/dev/vcio" System.Posix.IO.ReadOnly Nothing defaultFileFlags { nonBlock = True }
  release = closeFd . unMailbox

data RaspberryPi a = RaspberryPi

data Tag a = Tag { tagId :: Word32
                 , tagBuf :: a
                 } deriving (Eq, Show, Typeable)

instance Storable a => Storable (Tag a) where
  sizeOf tag = 4 * 3 + sizeOf (tagBuf tag) `div` 4 * 4 + 4
  alignment _tag = 4
  peek ptr = do
    a <- peekByteOff ptr 0
    d <- peekByteOff ptr 12
    return $ Tag { tagId = a, tagBuf = d }
  poke ptr tag = do
    let s = sizeOf (tagBuf tag) `div` 4 * 4 + 4
    pokeByteOff ptr 0 $ tagId tag
    pokeByteOff ptr 4 s
    pokeByteOff ptr 8 s
    pokeByteOff ptr 12 $ tagBuf tag

data Msg a = Msg { msgCode :: Word32
                 , msgTag :: Tag a
                 } deriving (Eq, Show, Typeable)

instance Storable a => Storable (Msg a) where
  sizeOf msg = 4 * 2 + sizeOf (msgTag msg) + 4
  alignment _msg = 4
  peek ptr = do
    code <- peekByteOff ptr 4
    tag <- peekByteOff ptr 8
    return $ Msg { msgCode = code, msgTag = tag }
  poke ptr msg = do
    let s = 4 * 2 + sizeOf (msgTag msg) + 4
    pokeByteOff ptr 0 s
    pokeByteOff ptr 4 $ msgCode msg
    pokeByteOff ptr 8 $ msgTag msg
    pokeByteOff ptr (8 + sizeOf (msgTag msg)) (0 :: Word32)

instance Storable a => IOControl (RaspberryPi a) (Msg a) where
  ioctlReq RaspberryPi = c_raspberrypi_mailbox_ioctlReq

property :: Storable b => Mailbox -> Word32 -> b -> IO b
property mb tag buf = do
  result <- ioctl (unMailbox mb) RaspberryPi
            $ Msg { msgCode = 0
                  , msgTag = Tag { tagId = tag
                                 , tagBuf = buf
                                 }
                  }
  case msgCode result of
    0x80000000 -> return . tagBuf . msgTag $ result
    0x80000001 -> error "error parsing request buffer (partial response)"
    _          -> error "unknown error"

getFirmwareRevision :: Mailbox -> IO Word32
getFirmwareRevision mb = property mb 0x00000001 0

getBoardModel :: Mailbox -> IO Word32
getBoardModel mb = property mb 0x00010001 0

getBoardRevision :: Mailbox -> IO Word32
getBoardRevision mb = property mb 0x00010002 0

data Word8x6 = Word8x6 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8

instance Storable Word8x6 where
  sizeOf _ = 1 * 6
  alignment _ = 4
  peek ptr = Word8x6
             <$> peekByteOff ptr 0
             <*> peekByteOff ptr 1
             <*> peekByteOff ptr 2
             <*> peekByteOff ptr 3
             <*> peekByteOff ptr 4
             <*> peekByteOff ptr 5
  poke ptr (Word8x6 a b c d e f) = do
    pokeByteOff ptr 0 a
    pokeByteOff ptr 1 b
    pokeByteOff ptr 2 c
    pokeByteOff ptr 3 d
    pokeByteOff ptr 4 e
    pokeByteOff ptr 5 f

getBoardMACAddress :: Mailbox -> IO [Word8]
getBoardMACAddress mb = do
  Word8x6 a b c d e f <- property mb 0x00010003 $ Word8x6 0 0 0 0 0 0
  return [a,b,c,d,e,f]

getBoardSerial :: Mailbox -> IO Word64
getBoardSerial mb = property mb 0x00010004 0

data Word32x2 = Word32x2 !Word32 !Word32

instance Storable Word32x2 where
  sizeOf _ = 4 * 2
  alignment _ = 4
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 4
    return $ Word32x2 a b
  poke ptr (Word32x2 a b) = do
    pokeByteOff ptr 0 a
    pokeByteOff ptr 4 b

getARMMemory :: Mailbox -> IO (Word32, Word32)
getARMMemory mb = do
  Word32x2 baseAddr size <- property mb 0x00010005 $ Word32x2 0 0
  return (baseAddr, size)

getVCMemory :: Mailbox -> IO (Word32, Word32)
getVCMemory mb = do
  Word32x2 baseAddr size <- property mb 0x00010006 $ Word32x2 0 0
  return (baseAddr, size)

-- getClocks :: Mailbox-> IO [(Word32, Word32]

-- getCommandLine :: Mailbox -> IO ByteString

getDMAChannels :: Mailbox -> IO Word32
getDMAChannels mb = property mb 0x00060001 0

data DeviceId = SD_Card
              | UART0
              | UART1
              | USB_HCD
              | I2C0
              | I2C1
              | I2C2
              | SPI
              | CCP2TX

fromDeviceId :: DeviceId -> Word32
fromDeviceId SD_Card = 0x00000000
fromDeviceId UART0   = 0x00000001
fromDeviceId UART1   = 0x00000002
fromDeviceId USB_HCD = 0x00000003
fromDeviceId I2C0    = 0x00000004
fromDeviceId I2C1    = 0x00000005
fromDeviceId I2C2    = 0x00000006
fromDeviceId SPI     = 0x00000007
fromDeviceId CCP2TX  = 0x00000008

data DeviceState =
  DevicePowerState
  { deviceExists :: Bool
  , deviceEnable :: Bool
  }

toDevicePowerState :: Word32 -> DeviceState
toDevicePowerState state =
  DevicePowerState { deviceExists = state .|. 0x00000002 == 0x00000002
                   , deviceEnable = state .|. 0x00000001 == 0x00000001
                   }

getPowerState :: Mailbox -> DeviceId -> IO DeviceState
getPowerState mb deviceId = do
  Word32x2 _ state <- property mb 0x00020001 $ Word32x2 (fromDeviceId deviceId) 0
  return $ toDevicePowerState state

getTiming :: Mailbox -> DeviceId -> IO Word32
getTiming mb deviceId = do
  Word32x2 _ usec <- property mb 0x00020002 $ Word32x2 (fromDeviceId deviceId) 0
  return usec

setPowerState :: Mailbox -> DeviceId -> Bool -> Bool -> IO DeviceState
setPowerState mb deviceId on wait = do
  let setState = sum [if on then 1 else 0, if wait then 2 else 0]
  Word32x2 _ state <- property mb 0x00028001 $ Word32x2 (fromDeviceId deviceId) setState
  return $ toDevicePowerState state

data ClockId = EMMC
             | UART
             | ARM
             | CORE
             | V3D
             | H264
             | ISP
             | SDRAM
             | PIXEL
             | PWM

fromClockId :: ClockId -> Word32
fromClockId EMMC  = 0x00000001
fromClockId UART  = 0x00000002
fromClockId ARM   = 0x00000003
fromClockId CORE  = 0x00000004
fromClockId V3D   = 0x00000005
fromClockId H264  = 0x00000006
fromClockId ISP   = 0x00000007
fromClockId SDRAM = 0x00000008
fromClockId PIXEL = 0x00000009
fromClockId PWM   = 0x0000000a

data ClockState =
  ClockState
  { clockExists :: Bool
  , clockEnable :: Bool
  }

toClockState :: Word32 -> ClockState
toClockState state =
  ClockState { clockExists = state .|. 0x00000002 == 0x00000002
             , clockEnable = state .|. 0x00000001 == 0x00000001
             }

getClockState :: Mailbox -> ClockId -> IO ClockState
getClockState mb clockId = do
  Word32x2 _ state <- property mb 0x00030001 $ Word32x2 (fromClockId clockId) 0
  return $ toClockState state

setClockState :: Mailbox -> ClockId -> Bool -> Bool -> IO ClockState
setClockState mb clockId on exist = do
  let setState = sum [if on then 1 else 0, if exist then 2 else 0]
  Word32x2 _ state <- property mb 0x00038001 $ Word32x2 (fromClockId clockId) setState
  return $ toClockState state

getClockRate :: Mailbox -> ClockId -> IO Word32
getClockRate mb clockId = do
  Word32x2 _ rate <- property mb 0x00030002 $ Word32x2 (fromClockId clockId) 0
  return rate

setClockRate :: Mailbox -> ClockId -> Word32 -> Word32 -> IO Word32
setClockRate mb clockId setRate skip = do
  Word32x3 _ _ rate <- property mb 0x00038002 $ Word32x3 (fromClockId clockId) setRate skip
  return rate

getMaxClockRate :: Mailbox -> ClockId -> IO Word32
getMaxClockRate mb clockId = do
  Word32x2 _ rate <- property mb 0x00030004 $ Word32x2 (fromClockId clockId) 0
  return rate

getMinClockRate :: Mailbox -> ClockId -> IO Word32
getMinClockRate mb clockId = do
  Word32x2 _ rate <- property mb 0x00030007 $ Word32x2 (fromClockId clockId) 0
  return rate

getTurbo :: Mailbox -> ClockId -> IO Word32
getTurbo mb clockId = do
  Word32x2 _ level <- property mb 0x00030009 $ Word32x2 (fromClockId clockId) 0
  return level

setTurbo :: Mailbox -> ClockId -> Word32 -> IO Word32
setTurbo mb clockId setLevel = do
  Word32x2 _ level <- property mb 0x00038009 $ Word32x2 (fromClockId clockId) setLevel
  return level

data VoltageId = Core
               | SDRAM_C
               | SDRAM_P
               | SDRAM_I


fromVoltageId :: VoltageId -> Word32
fromVoltageId Core    = 0x00000001
fromVoltageId SDRAM_C = 0x00000002
fromVoltageId SDRAM_P = 0x00000003
fromVoltageId SDRAM_I = 0x00000004

getVoltage :: Mailbox -> VoltageId -> IO Word32
getVoltage mb voltageId = do
  Word32x2 _ voltage <- property mb 0x00030003 $ Word32x2 (fromVoltageId voltageId) 0
  return voltage

setVoltage :: Mailbox -> VoltageId -> Word32 -> IO Word32
setVoltage mb voltageId newVoltage = do
  Word32x2 _ voltage <- property mb 0x00038003 $ Word32x2 (fromVoltageId voltageId) newVoltage
  return voltage

getMaxVoltage :: Mailbox -> VoltageId -> IO Word32
getMaxVoltage mb voltageId = do
  Word32x2 _ voltage <- property mb 0x00030005 $ Word32x2 (fromVoltageId voltageId) 0
  return voltage

getMinVoltage :: Mailbox -> VoltageId -> IO Word32
getMinVoltage mb voltageId = do
  Word32x2 _ voltage <- property mb 0x00030008 $ Word32x2 (fromVoltageId voltageId) 0
  return voltage

getTemperature :: Mailbox -> Word32 -> IO Word32
getTemperature mb temperatureId = do
  Word32x2 _ temperature <- property mb 0x00030006 $ Word32x2 temperatureId 0
  return temperature

getMaxTemperature :: Mailbox -> Word32 -> IO Word32
getMaxTemperature mb temperatureId = do
  Word32x2 _ temperature <- property mb 0x0003000a $ Word32x2 temperatureId 0
  return temperature

data Word32x3 = Word32x3 !Word32 !Word32 !Word32

instance Storable Word32x3 where
  sizeOf _ = 4 * 3
  alignment _ = 4
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 4
    c <- peekByteOff ptr 8
    return $ Word32x3 a b c
  poke ptr (Word32x3 a b c) = do
    pokeByteOff ptr 0 a
    pokeByteOff ptr 4 b
    pokeByteOff ptr 8 c

data Word32x4 = Word32x4 !Word32 !Word32 !Word32 !Word32

instance Storable Word32x4 where
  sizeOf _ = 4 * 4
  alignment _ = 4
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 4
    c <- peekByteOff ptr 8
    d <- peekByteOff ptr 12
    return $ Word32x4 a b c d
  poke ptr (Word32x4 a b c d) = do
    pokeByteOff ptr 0 a
    pokeByteOff ptr 4 b
    pokeByteOff ptr 8 c
    pokeByteOff ptr 12 d

allocateGPUMemory :: Mailbox -> Word32 -> Word32 -> Word32 -> IO Word32
allocateGPUMemory mb size align flags = do
  Word32x3 handle _ _ <- property mb 0x0003000c $ Word32x3 size align flags
  return handle

releaseGPUMemory :: Mailbox -> Word32 -> IO Word32
releaseGPUMemory mb handle = property mb 0x0003000f handle

withGPUMemory :: Mailbox -> Word32 -> Word32 -> Word32 -> (Word32 -> IO a) -> IO a
withGPUMemory mb size align flags = bracket acquire release where
  acquire = allocateGPUMemory mb size align flags
  release = releaseGPUMemory mb

lockGPUMemory :: Mailbox -> Word32 -> IO Word32
lockGPUMemory mb handle = property mb 0x0003000d handle

unlockGPUMemory :: Mailbox -> Word32 -> IO Word32
unlockGPUMemory mb handle = property mb 0x0003000e handle

withLockedGPUMemory :: Mailbox -> Word32 -> (Word32 -> IO a) -> IO a
withLockedGPUMemory mb handle = bracket acquire release where
  acquire = lockGPUMemory mb handle
  release = unlockGPUMemory mb

data MappedPtr a = MappedPtr { cpuPtr :: Ptr a
                             , gpuPtr :: Word32
                             } deriving (Eq, Show, Typeable)

withDevMem :: (Fd -> IO a) -> IO a
withDevMem = bracket acquire release where
  acquire = openFd "/dev/mem" ReadWrite (Just 0o644) defaultFileFlags
  release = closeFd

withCPUMappedGPUMemory :: Mailbox -> Word32 -> Word32 -> (MappedPtr CVoid -> IO a) -> IO a
withCPUMappedGPUMemory mb size align action =
  withGPUMemory mb size align 4 $ \handle -> do
    when (handle == 0) $ error ("Failed to allocate " ++ show size ++ " bytes of memory on GPU")
    withLockedGPUMemory mb handle $ \bus -> do
      when (bus == 0) $ error ("Failed to lock " ++ show size ++ " bytes of memory on GPU")
      let phys = toEnum $ fromEnum $ bus .&. 0x3FFFFFFF
      withDevMem $ \fd -> withMMap (fromIntegral size) [PROT_READ, PROT_WRITE] [MAP_SHARED] fd phys $ \ptr ->
        action $ MappedPtr { cpuPtr = ptr, gpuPtr = bus }

executeQPU :: Mailbox -> Word32 -> MappedPtr a -> Word32 -> Word32 -> IO Word32
executeQPU mb num_qpus control noflush timeout = do
  Word32x4 result _ _ _ <- property mb 0x00030011 $ Word32x4 num_qpus (gpuPtr control) noflush timeout
  return result

-- http://www.freelists.org/post/raspi-internals/GPU-FFT-Disassembly
enableQPU :: Mailbox -> IO Word32
enableQPU mb = property mb 0x00030012 1

disableQPU :: Mailbox -> IO Word32
disableQPU mb = property mb 0x00030012 0

withQPU :: Mailbox -> IO a -> IO a
withQPU mb = bracket_ acquire release where
  acquire = enableQPU mb
  release = disableQPU mb

-- getDispmanxResourceMemoryHandle :: Mailbox -> Word2 -> IO Word32

-- getEDIDBlock :: Mailbox -> Word32 -> IO ...
