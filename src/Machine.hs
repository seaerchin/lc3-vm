module Machine where

import Control.Monad.State.Lazy
import Data.Bits ((.&.))
import Data.Word (Word16, Word8)
import GHC.Num (wordToInteger)
import Util

-- NOTE: all methods postfixed by ' refer to methods bound by the state monad

-- to be precise, this is actually a bounded array of size 65,536.
type Memory = [Word16]

-- 8 general purpose registers; program counter; condition register
data Registers = Registers [Word16] Int Condition

-- all possible instructions
-- note: not supporting rti/reserve
data Instruction = Branch | Add | Load | Store | JumpRegister | And | LoadRegister | StoreRegister | Not | LoadIndirect | StoreIndirect | Jump | LoadEffectiveAddr | ExecuteTrap

-- An opcode is an instruction (left 4 bits technically cos ~16 instructions) + data of instructions (remaining array of size 12)
-- This is technically a bit-array of size 16
data OpCode = OpCode Instruction [Bool]

-- indicates result of previous calc
data Condition = Positive | Zero | Negative deriving (Show, Eq)

data Machine = Machine Memory Registers

-- current result of instruction
type MachineState a = State Machine a

-- accessors
getMemory :: Machine -> Memory
getMemory (Machine m _) = m

getMemory' :: MachineState Memory
getMemory' = do
  machine <- get
  let mem = getMemory machine
  return mem

getRegisters :: Machine -> Registers
getRegisters (Machine _ r) = r

getRegisters' :: MachineState Registers
getRegisters' = do
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

getPc :: Registers -> Int
getPc (Registers _ p _) = p

-- less boilerplate version
getPc' :: MachineState Int
getPc' = do
  machine <- get
  let registers = getRegisters machine
  return (getPc registers)

getCondition :: Registers -> Condition
getCondition (Registers _ _ c) = c

getCondition' :: MachineState Condition
getCondition' = do
  getCondition . getRegisters <$> get

toCondition :: (Integral a) => a -> Condition
toCondition 0 = Zero
toCondition x = if x < 0 then Negative else Positive

-- mutators
setConditionRegister :: Registers -> Condition -> Registers
setConditionRegister (Registers g pc _) = Registers g pc

-- sets register at idx to val
-- automatically sets condition register based on result
setGeneralRegister :: Registers -> Word8 -> Word16 -> Registers
setGeneralRegister (Registers g pc cr) idx val =
  let newVals = update g (fromIntegral idx) val
      cond = toCondition val
   in Registers newVals pc cond

setGeneralRegister' :: Word8 -> Word16 -> MachineState ()
setGeneralRegister' idx val = do
  m <- get
  r <- getRegisters'
  let r' = setGeneralRegister r idx val
  put (setRegisters m r')

setRegisters :: Machine -> Registers -> Machine
setRegisters (Machine m _) = Machine m

setPc :: Registers -> Int -> Registers
setPc (Registers a _ c) pc = Registers a pc c

setPc' :: Int -> MachineState ()
setPc' pc = do
  machine <- get
  let newRegisters = setPc (getRegisters machine) pc
  put (setRegisters machine newRegisters)

incrementPc :: Registers -> Registers
incrementPc r =
  let oldVal = getPc r
   in setPc r (oldVal + 1)

-- this handles reading in the data,
-- parse to the actual instruction
-- and handle incrementing of PC before execution of instruction
executeInstruction :: FilePath -> IO ()
executeInstruction s =
  let handle = readFile s
   in handle >>= \s -> print s

-- instruction handling logic
handleInstruction :: OpCode -> MachineState ()
handleInstruction (OpCode Add inst) = handleAdd inst
handleInstruction (OpCode LoadIndirect inst) = handleLoadIndirect inst
handleInstruction (OpCode Load inst) = handleLoad inst
handleInstruction (OpCode And inst) = handleAnd inst
handleInstruction (OpCode Branch inst) = handleBranch inst
handleInstruction (OpCode Jump inst) = handleJump inst
handleInstruction (OpCode JumpRegister inst) = handleJumpRegister inst
handleInstruction (OpCode LoadRegister inst) = handleLoadRegister inst

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
      val = if isImmediate then fromBits $ slice inst 0 4 else sr2
  setGeneralRegister' dest (val + sr1)

-- computes an address from the instruction and add it to the PC
handleLoadIndirect :: [Bool] -> MachineState ()
handleLoadIndirect inst = do
  mem <- getMemory'
  pc <- getPc'
  let dest = toWord $ slice inst 9 11
      memContents = mem !! (pc + toWord (slice inst 0 8))
  setGeneralRegister' dest (mem !! fromIntegral memContents)

handleLoad :: [Bool] -> MachineState ()
handleLoad inst = do
  let destIndex = toWord $ slice inst 9 11
      offset = toWord $ slice inst 0 8
  pc <- getPc'
  mem <- getMemory'
  setGeneralRegister' destIndex (mem !! (pc + offset))

handleAnd :: [Bool] -> MachineState ()
handleAnd inst = do
  sr1 <- getGeneralRegisterContent' (toWord $ slice inst 6 8)
  sr2 <- getGeneralRegisterContent' (toWord $ slice inst 0 2)
  let isImmediate = inst !! 5
      dest = toWord $ slice inst 9 11
      val = if isImmediate then fromBits $ slice inst 0 4 else sr2
  setGeneralRegister' dest (val .&. sr1)

handleBranch :: [Bool] -> MachineState ()
handleBranch inst = do
  pc <- getPc'
  condReg <- getCondition'
  let n = inst !! 11
      z = inst !! 10
      p = inst !! 9
      offset = toWord $ slice inst 0 8
      updatedPc = if n && condReg == Negative || z && condReg == Zero || p && condReg == Positive then pc + offset else pc
  setPc' updatedPc

handleJump :: [Bool] -> MachineState ()
handleJump inst = do
  let regIdx = toWord $ slice inst 6 8
  jump regIdx

handleRet :: MachineState ()
handleRet = jump 7

-- set PC to content of register at regIdx
jump :: Word8 -> MachineState ()
jump regIdx = do
  regContent <- getGeneralRegisterContent' regIdx
  setPc' (fromIntegral regContent)

-- jump to subroutine
-- refer to lc3 isa for further details
handleJumpRegister :: [Bool] -> MachineState ()
handleJumpRegister inst = do
  pc <- getPc'
  -- store current pc value to return to
  setGeneralRegister' 7 (fromIntegral pc)
  let isImmediate = inst !! 11
      pcValue = if isImmediate then pc + toWord (slice inst 10 0) else toWord $ slice inst 6 8
  setPc' pcValue

handleLoadRegister :: [Bool] -> MachineState ()
handleLoadRegister inst = do
  mem <- getMemory'
  baseR <- getGeneralRegisterContent' (toWord $ slice inst 6 8)
  let destIndex = toWord $ slice inst 9 11
      offset = toWord $ slice inst 0 5
  setGeneralRegister' destIndex (mem !! fromIntegral (baseR + offset))

handleLoadEffectiveAddr :: [Bool] -> MachineState ()
handleLoadEffectiveAddr inst = do
  pc <- getPc'
  let offset = toWord $ slice inst 0 8
      dest = toWord $ slice inst 9 11
  setGeneralRegister' dest (fromIntegral $ pc + offset)