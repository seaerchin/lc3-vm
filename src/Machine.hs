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

toCondition :: (Integral a) => a -> Condition
toCondition 0 = Zero
toCondition x = if x < 0 then Negative else Positive

-- mutators
setConditionRegister :: Registers -> Condition -> Registers
setConditionRegister (Registers g pc _) = Registers g pc

setGeneralRegister :: Registers -> Word8 -> Word16 -> Registers
setGeneralRegister (Registers g pc cr) idx val =
  let newVals = update g (fromIntegral idx) val
   in Registers newVals pc cr

setRegisters :: Machine -> Registers -> Machine
setRegisters (Machine m _) = Machine m

executeInstruction :: FilePath -> IO ()
executeInstruction s =
  let handle = readFile s
   in handle >>= \s -> print s

-- instruction handling logic
handleInstruction :: MachineState a -> OpCode -> MachineState a
handleInstruction s oc@(OpCode Add inst) = undefined

handleAdd :: MachineState a -> [Bool] -> MachineState ()
-- as per specification:
-- if bit 5 is 0, we know that this is addition using 2 registers
-- else, this is addition using a immediate value
handleAdd s inst =
  let isImmediate = inst !! 4
   in if isImmediate
        then do
          machine <- get
          let immediateValue = fromBits $ slice inst 0 4
              sourceRegister1 = fromBits $ slice inst 6 8
              newValue = immediateValue + sourceRegister1
              destinationRegister = fromIntegral . fromBits $ slice inst 9 11
              newRegisterVals = setGeneralRegister (getRegisters machine) destinationRegister newValue
              newRegisters = setConditionRegister newRegisterVals (toCondition newValue)
              newMachine = setRegisters machine newRegisters
          put newMachine
        else --   return $ setRegisters machine newRegisters
          undefined
