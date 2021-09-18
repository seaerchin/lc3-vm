module Machine where

import Control.Monad.State.Lazy
import Data.Word (Word16, Word8)
import Util

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
data Condition = Positive | Zero | Negative

data Machine = Machine Memory Registers

-- current result of instruction
type MachineState a = State Machine a

-- accessors
getMemory :: Machine -> Memory
getMemory (Machine m _) = m

getRegisters :: Machine -> Registers
getRegisters (Machine _ r) = r

getGeneralRegister :: Registers -> [Word16]
getGeneralRegister (Registers r _ _) = r

getGeneralRegisterContent :: Registers -> Word8 -> Word16
getGeneralRegisterContent r idx = getGeneralRegister r !! fromIntegral idx

getPc :: Registers -> Int
getPc (Registers _ p _) = p

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

setRegisters :: Machine -> Registers -> Machine
setRegisters (Machine m _) = Machine m

setPc :: Registers -> Int -> Registers
setPc (Registers a _ c) pc = Registers a pc c

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
handleInstruction oc@(OpCode Add inst) = handleAdd inst
handleInstruction oc@(OpCode LoadIndirect inst) = handleLoadIndirect inst

handleAdd :: [Bool] -> MachineState ()
-- as per specification:
-- if bit 5 is 0, we know that this is addition using 2 registers
-- else, this is addition using a immediate value
handleAdd inst = do
  machine <- get
  let registers = getRegisters machine
      isImmediate = inst !! 4
      sourceRegister = toWord $ slice inst 6 8
      destinationRegister = toWord $ slice inst 9 11
      val = if isImmediate then fromBits $ slice inst 0 4 else getGeneralRegisterContent registers (toWord $ slice inst 0 2)
  addValues sourceRegister val destinationRegister

addValues :: Word8 -> Word16 -> Word8 -> MachineState ()
addValues registerIndex val dest = do
  machine <- get
  let registers = getRegisters machine
      registerVal = getGeneralRegisterContent registers registerIndex
      newValue = val + registerVal
      newRegisters = setGeneralRegister (getRegisters machine) dest newValue
      newMachine = setRegisters machine newRegisters
  put newMachine

-- computes an address from the instruction and add it to the PC
handleLoadIndirect :: [Bool] -> MachineState ()
handleLoadIndirect inst = do
  machine <- get
  let registers = getRegisters machine
      memory = getMemory machine
      destinationIndex = toWord $ slice inst 9 11
      addr = toWord $ slice inst 0 8
      pc = getPc registers
      val = memory !! fromIntegral (memory !! (addr + pc))
      m = setRegisters machine $ setGeneralRegister registers destinationIndex val
  put m
