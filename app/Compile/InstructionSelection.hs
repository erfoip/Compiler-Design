module Compile.InstructionSelection () where

import Compile.AAAST ()
import Compile.AST (AST (..), Stmt (..))
import qualified Data.HashSet as HashSet

-- counts the unique initialized or assigned registers in an AST
countRegisters :: AST -> Int
countRegisters (Block stmts _) = HashSet.size $ HashSet.fromList $ filter (\s -> s /= "") $ map getVarName stmts
  where
    getVarName (Init s _ _) = s
    getVarName (Asgn s _ _ _) = s
    getVarName _ = ""
