{-# LANGUAGE Strict #-}

module Machine where

import Control.Monad.State.Lazy
import Data.Bits (Bits (complement), shiftL, shiftR, testBit, (.&.))
import qualified Data.ByteString as B
import Data.Char (chr, ord)
import Data.Functor ((<&>))
import qualified Data.Vector as V
import Data.Word (Word16, Word8)
import GHC.Num (wordToInteger)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (BufferMode (NoBuffering), hFlush, hSetBuffering, stdin, stdout)
import Util
  ( fromBits,
    lowerMask,
    processBits,
    signExtend,
    slice,
    toWord,
    update,
    upperMask,
  )

-- NOTE: all methods postfixed by ' refer to methods bound by the state monad

-- to be precise, this is actually a bounded array of size 65,536.
type Memory = [Word16]

-- TODO: actually make this correct
-- type RegisterIndex = Word3

-- 8 general purpose registers; program counter; condition register
data Registers = Registers [Word16] Word16 Condition deriving (Eq, Show)

-- all possible instructions
-- note: not supporting rti/reserve
data Instruction = Branch | Add | Load | Store | JumpRegister | And | LoadRegister | StoreRegister | Not | LoadIndirect | StoreIndirect | Jump | LoadEffectiveAddr | ExecuteTrap deriving (Eq, Show)

-- An opcode is an instruction (left 4 bits technically cos ~16 instructions) + data of instructions (remaining array of size 12)
-- This is technically a bit-array of size 16
data OpCode = OpCode Instruction [Bool] deriving (Eq, Show)

-- indicates result of previous calc
data Condition = Positive | Zero | Negative deriving (Show, Eq)

data Machine = Machine Memory Registers

-- current result of instruction
type MachineState = StateT Machine IO

data Trap = GetC | Out | Puts | In | Putsp | Halt deriving (Show, Eq)

mrKBSR :: Integer
mrKBSR = 0xFE00 -- /* keyboard status */

mrKBDR :: Integer
mrKBDR = 0xFE02 -- /* keyboard data */

-- accessors
checkKey :: IO (Maybe Word16)
checkKey = do
  result <- B.hGetNonBlocking stdin 1
  case result of
    x
      | B.null x -> pure Nothing
      | otherwise -> do
        let [l] = B.unpack x
        pure $ Just $ fromIntegral l

memRead :: Word16 -> MachineState Word16
memRead addr
  | addr == fromIntegral mrKBSR = handleKey
  | otherwise = do
    mem <- getMemory'
    return (mem !! fromIntegral addr)
  where
    handleKey = do
      maybeKey <- liftIO checkKey
      case maybeKey of
        Just key -> do
          mem <- getMemory'
          setMemory' (fromIntegral mrKBSR) (1 `shiftL` 15)
          setMemory' (fromIntegral mrKBDR) key
        Nothing ->
          setMemory' (fromIntegral mrKBSR) 0
      mem <- getMemory'
      return (mem !! fromIntegral addr)

getMemory :: Machine -> Memory
getMemory (Machine m _) = m

getMemory' :: MachineState Memory
getMemory' = get <&> getMemory

getRegisters :: Machine -> Registers
getRegisters (Machine _ r) = r

getRegisters' :: MachineState Registers
getRegisters' =
  getRegisters <$> get

getGeneralRegister :: Registers -> [Word16]
getGeneralRegister (Registers r _ _) = r

getGeneralRegisterContent :: Registers -> Word8 -> Word16
getGeneralRegisterContent r idx = getGeneralRegister r !! fromIntegral idx

getGeneralRegisterContent' :: Word8 -> MachineState Word16
getGeneralRegisterContent' idx = do
  machine <- get
  let reg = getRegisters machine
  return (getGeneralRegisterContent reg idx)

getPc :: Registers -> Word16
getPc (Registers _ p _) = p

-- less boilerplate version
getPc' :: MachineState Word16
getPc' = do
  machine <- get
  let registers = getRegisters machine
  return (getPc registers)

getCondition :: Registers -> Condition
getCondition (Registers _ _ c) = c

getCondition' :: MachineState Condition
getCondition' =
  getCondition . getRegisters <$> get

toCondition :: (Integral a) => a -> Condition
toCondition 0 = Zero
toCondition x = if x < 0 then Negative else Positive

-- mutators
setConditionRegister :: Registers -> Condition -> Registers
setConditionRegister (Registers g pc _) = Registers g pc

setConditionRegister' :: Word16 -> MachineState ()
setConditionRegister' val
  | val == 0 = aux Zero
  | val `testBit` 15 = aux Negative
  | otherwise = aux Positive
  where
    aux x = do
      m <- get
      r <- getRegisters'
      let r' = setConditionRegister r x
          m' = setRegisters m r'
      put m'

-- sets register at idx to val
-- automatically sets condition register based on result
setGeneralRegister :: Registers -> Word8 -> Word16 -> Registers
setGeneralRegister (Registers g pc cr) idx val =
  let newVals = update g (fromIntegral idx) val
   in Registers newVals pc cr

setGeneralRegister' :: Word8 -> Word16 -> MachineState ()
setGeneralRegister' idx val = do
  m <- get
  r <- getRegisters'
  let r' = setGeneralRegister r idx val
  put (setRegisters m r')

setRegisters :: Machine -> Registers -> Machine
setRegisters (Machine m _) = Machine m

setPc :: Registers -> Word16 -> Registers
setPc (Registers a _ c) pc = Registers a pc c

setPc' :: Word16 -> MachineState ()
setPc' pc = do
  machine <- get
  let newRegisters = setPc (getRegisters machine) pc
  put (setRegisters machine newRegisters)

setMemory :: Machine -> Memory -> Machine
setMemory (Machine _ r) mem = Machine mem r

setMemory' :: Word16 -> Word16 -> MachineState ()
setMemory' idx val = do
  mem <- getMemory'
  machine <- get
  -- haskell lists are linked lists :/
  -- ideally, this would be a vector but due to laziness, is left as-is for now.
  let (pref, suff) = splitAt (fromIntegral idx) mem
      suff' = drop 1 suff
  put (setMemory machine (pref ++ [val] ++ suff'))

incrementPc' :: MachineState ()
incrementPc' = do
  pc <- getPc'
  setPc' (pc + 1)

-- services to handle instructions

runRoutine = flip execStateT

dump :: MachineState ()
dump = do
  reg <- getRegisters'
  liftIO $ print reg

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  memory <- readImageFile
  let registers = Registers (replicate 8 0) 0x3000 Zero
      machine = Machine memory registers
  runRoutine machine (forever handleRawInst)
  print "vm done"

routine :: MachineState ()
routine = do
  setPc' 0x3000
  fix $ \loop -> do
    handleRawInst >> loop

-- reads an image file
readImageFile :: IO Memory
readImageFile = do
  args <- getArgs
  case args of
    fileName : _ -> do
      (origin : bytes) <- processBits . B.unpack <$> B.readFile fileName
      let pad = V.replicate (fromIntegral origin - 1) (0x0 :: Word16)
          mid = V.fromList (origin : bytes)
          end = V.replicate (65536 - (V.length pad + V.length mid)) (0x0 :: Word16)
      pure $ V.toList (pad <> mid <> end)
    _ -> do
      putStrLn "Please enter path to LC3 program"
      exitFailure

handleRawInst :: MachineState ()
handleRawInst = do
  pc <- getPc'
  mem <- getMemory'
  rawInst <- memRead (fromIntegral pc)
  let opCode = parseInst rawInst
  -- liftIO $ print opCode
  -- dump
  incrementPc'
  handleInstruction opCode

parseInst :: Word16 -> OpCode
parseInst raw =
  -- top 4 bits are the opcode
  let op = (raw .&. (15 `shiftL` 12)) `shiftR` 12
      rest = (2 ^ 12 - 1) .&. raw
   in OpCode (toInst op) (reverse $ toBits12 $ fromIntegral rest)

toInst :: Word16 -> Instruction
toInst 1 = Add
toInst 5 = And
toInst 0 = Branch
toInst 12 = Jump
toInst 4 = JumpRegister
toInst 2 = Load
toInst 10 = LoadIndirect
toInst 6 = LoadRegister
toInst 14 = LoadEffectiveAddr
toInst 9 = Not
toInst 3 = Store
toInst 11 = StoreIndirect
toInst 7 = StoreRegister
toInst 15 = ExecuteTrap
toInst _ = undefined

toBitsBySize :: Int -> Int -> [Bool]
toBitsBySize 0 x = []
toBitsBySize sz 0 = [False | i <- [1 .. sz]]
toBitsBySize sz x =
  if k == 0
    then False : toBitsBySize n x
    else True : toBitsBySize n (x - k * m)
  where
    n = sz - 1
    m = 2 ^ n
    k = x `div` m

toBits12 = toBitsBySize 12

handleInstruction :: OpCode -> MachineState ()
handleInstruction (OpCode Add inst) = handleAdd inst
handleInstruction (OpCode LoadIndirect inst) = handleLoadIndirect inst
handleInstruction (OpCode Load inst) = handleLoad inst
handleInstruction (OpCode And inst) = handleAnd inst
handleInstruction (OpCode Branch inst) = handleBranch inst
handleInstruction (OpCode Jump inst) = handleJump inst
handleInstruction (OpCode JumpRegister inst) = handleJumpRegister inst
handleInstruction (OpCode LoadRegister inst) = handleLoadRegister inst
handleInstruction (OpCode Store inst) = handleStore inst
handleInstruction (OpCode StoreRegister inst) = handleStoreRegister inst
handleInstruction (OpCode Not inst) = handleNot inst
handleInstruction (OpCode StoreIndirect inst) = handleStoreIndirect inst
handleInstruction (OpCode LoadEffectiveAddr inst) = handleLoadEffectiveAddr inst
handleInstruction (OpCode ExecuteTrap inst) = handleTrap inst

handleAdd :: [Bool] -> MachineState ()
-- as per specification:
-- if bit 5 is 0, we know that this is addition using 2 registers
-- else, this is addition using a immediate value
-- this checks whether it is immediate or indirect,
-- extracts the value and defers downwards
handleAdd inst = do
  sr1 <- getGeneralRegisterContent' (toWord $ slice inst 6 8)
  sr2 <- getGeneralRegisterContent' (toWord $ slice inst 0 2)
  let isImmediate = inst !! 5
      dest = toWord $ slice inst 9 11
      val = if isImmediate then signExtend (fromBits $ slice inst 0 4) 5 else sr2
  setGeneralRegister' dest (val + sr1)
  setConditionRegister' (val + sr1)

handleAnd :: [Bool] -> MachineState ()
handleAnd inst = do
  sr1 <- getGeneralRegisterContent' (toWord $ slice inst 6 8)
  sr2 <- getGeneralRegisterContent' (toWord $ slice inst 0 2)
  let isImmediate = inst !! 5
      dest = toWord $ slice inst 9 11
      val = if isImmediate then signExtend (fromBits $ slice inst 0 4) 5 else sr2
  setGeneralRegister' dest (val .&. sr1)
  setConditionRegister' (val .&. sr1)

handleBranch :: [Bool] -> MachineState ()
handleBranch inst = do
  pc <- getPc'
  condReg <- getCondition'
  let n = inst !! 11
      z = inst !! 10
      p = inst !! 9
      offset = signExtend (toWord $ slice inst 0 8) 9
      updatedPc = if n && condReg == Negative || z && condReg == Zero || p && condReg == Positive then pc + offset else pc
  setPc' updatedPc

-- set PC to content of register at regIdx
jump :: Word8 -> MachineState ()
jump regIdx = do
  regContent <- getGeneralRegisterContent' regIdx
  setPc' regContent

handleJump :: [Bool] -> MachineState ()
handleJump inst = do
  let regIdx = toWord $ slice inst 6 8
  jump regIdx

handleRet :: MachineState ()
handleRet = jump 7

-- jump to subroutine
-- refer to lc3 isa for further details
handleJumpRegister :: [Bool] -> MachineState ()
handleJumpRegister inst = do
  pc <- getPc'
  -- store current pc value to return to
  setGeneralRegister' 7 pc
  let isImmediate = inst !! 11
      offset = signExtend (toWord $ slice inst 0 10) 11
  baseR <- memRead $ toWord (slice inst 6 8)
  let pcValue = if isImmediate then pc + offset else baseR
  setPc' pcValue

handleLoad :: [Bool] -> MachineState ()
handleLoad inst = do
  let destIndex = toWord $ slice inst 9 11
      offset = signExtend (toWord $ slice inst 0 8) 9
  pc <- getPc'
  memContents <- memRead (pc + offset)
  setGeneralRegister' destIndex memContents
  setConditionRegister' memContents

-- computes an address from the instruction and add it to the PC
handleLoadIndirect :: [Bool] -> MachineState ()
handleLoadIndirect inst = do
  mem <- getMemory'
  pc <- getPc'
  let dest = toWord $ slice inst 9 11
      val = pc + signExtend (toWord (slice inst 0 8)) 9
  memContents <- memRead (pc + signExtend (toWord (slice inst 0 8)) 9)
  memContents' <- memRead memContents
  setGeneralRegister' dest memContents'
  setConditionRegister' memContents'

handleLoadRegister :: [Bool] -> MachineState ()
handleLoadRegister inst = do
  baseR <- getGeneralRegisterContent' (toWord $ slice inst 6 8)
  let destIndex = toWord $ slice inst 9 11
      offset = signExtend (toWord $ slice inst 0 5) 6
  memContent <- memRead (baseR + offset)
  setGeneralRegister' destIndex memContent
  setConditionRegister' memContent

handleLoadEffectiveAddr :: [Bool] -> MachineState ()
handleLoadEffectiveAddr inst = do
  pc <- getPc'
  let offset = signExtend (toWord $ slice inst 0 8) 9
      dest = toWord $ slice inst 9 11
      val = pc + offset
  setGeneralRegister' dest val
  setConditionRegister' val

-- check if two's complement is correct
handleNot :: [Bool] -> MachineState ()
handleNot inst = do
  sr <- getGeneralRegisterContent' (toWord $ slice inst 6 8)
  let dr = toWord $ slice inst 9 11
      val = complement sr
  setGeneralRegister' dr val
  setConditionRegister' val

handleStore :: [Bool] -> MachineState ()
handleStore inst = do
  pc <- getPc'
  sr <- getGeneralRegisterContent' (toWord $ slice inst 9 11)
  let offset = signExtend (toWord $ slice inst 0 8) 9
  setMemory' (pc + offset) sr

handleStoreIndirect :: [Bool] -> MachineState ()
handleStoreIndirect inst = do
  sr <- getGeneralRegisterContent' (toWord $ slice inst 9 11)
  mem <- getMemory'
  pc <- getPc'
  let offset = signExtend (toWord $ slice inst 0 8) 9
  memContents <- memRead (pc + offset)
  memContents' <- memRead memContents
  setMemory' memContents' sr

handleStoreRegister :: [Bool] -> MachineState ()
handleStoreRegister inst = do
  baseR <- getGeneralRegisterContent' (toWord $ slice inst 6 8)
  sr <- getGeneralRegisterContent' (toWord $ slice inst 9 11)
  let offset = signExtend (toWord $ slice inst 0 5) 6
  mem <- getMemory'
  setMemory' (baseR + offset) sr

-- trap routines read/write to the IO stream
handleTrap :: [Bool] -> MachineState ()
handleTrap inst = do
  let trapvect8 = toTrap $ slice inst 0 8
  executeTrap trapvect8

toTrap :: [Bool] -> Trap
toTrap inst =
  let trap = toWord inst
   in case trap of
        0x20 -> GetC
        0x21 -> Out
        0x22 -> Puts
        0x23 -> In
        0x24 -> Putsp
        _ -> Halt

executeTrap :: Trap -> MachineState ()
executeTrap GetC = getc
executeTrap Out = out
executeTrap Puts = puts
executeTrap In = in'
executeTrap Putsp = putsp
executeTrap _ = liftIO halt

getc :: MachineState ()
getc = do
  c <- liftIO getChar
  let ascii = ord c
  setGeneralRegister' 0 (fromIntegral ascii)

out :: MachineState ()
out = do
  i <- getGeneralRegisterContent' 0
  -- only lower 8 digits should be taken
  -- perform bitwise AND operation
  let lower = i .&. lowerMask
      c = chr (fromIntegral lower)
  liftIO $ putChar c

puts :: MachineState ()
puts = do
  initialAddr <- getGeneralRegisterContent' 0
  mem <- getMemory'
  let memSlice = extractChars $ slice mem (fromIntegral initialAddr) (fromIntegral $ length mem - 1)
  liftIO $ putStr (fmap (chr . fromIntegral) memSlice)
  liftIO (hFlush stdout)
  where
    extractChars = takeWhile (/= 0)

-- to differentiate from keyword
in' :: MachineState ()
in' = do
  liftIO $ print "Please enter a character"
  c <- liftIO getChar
  setGeneralRegister' 0 (fromIntegral $ ord c)
  liftIO $ print c

-- copied b/c laze
putsp :: MachineState ()
putsp = do
  initialAddr <- getGeneralRegisterContent' 0
  mem <- getMemory'
  let memSlice = extractChars $ slice mem (fromIntegral initialAddr) (fromIntegral $ length mem - 1)
  liftIO $ forM_ memSlice print
  where
    extractChars =
      let extractList = takeWhile (/= 0)
       in join . map splitWord . extractList
    splitWord word = [word .&. upperMask, word .&. lowerMask]

-- kek
halt = do
  liftIO $ putStrLn "halted"
  liftIO exitSuccess
