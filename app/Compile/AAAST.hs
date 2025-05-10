-- | This module provides a data type for the abstract assembly
-- representation of the abstract syntax tree used for instruction selection
module Compile.AAAST
    (
        AAAST(..)
        , Inst(..)
        , Operand(..)
    ) where

import Compile.AST (Op)
import Data.List (intercalate)

data AAAST =
    Block [Inst]

data Inst
    = Init String Operand
    | Asgn String Operand Op Operand

data Operand
    = Reg Integer
    | Con Integer
    | Temp String


instance Show AAAST where
    show (Block insts) =
        "Block: {\n" ++ intercalate "\n" (map show insts) ++ "\n}"

instance Show Inst where
    show (Init dest op) = "Init: " ++ dest ++ " <- " ++ show op
    show (Asgn dest op1 op op2) = "Assign: " ++ dest ++ " <- " ++ show op1 ++ " " ++ show op ++ " " ++ show op2

instance Show Operand where
    show (Reg i) = 'R' : show i
    show (Con c) = show c
    show (Temp t) = t