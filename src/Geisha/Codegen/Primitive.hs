module Geisha.Codegen.Primitive (
  binops
) where

import LLVM.General.AST
import LLVM.General.AST.Global

import qualified Data.Map as M

import qualified Geisha.Codegen.LLVM as C
import qualified Geisha.AST as S

binops = M.fromList [
    ("+", fadd),
    ("-", fsub),
    ("*", fmul),
    ("/", fdiv)
  ]

fbinInsr f a b = C.instr $ f NoFastMathFlags a b []

fadd :: Operand -> Operand -> C.CodegenErrorT Operand
fadd = fbinInsr FAdd

fsub :: Operand -> Operand -> C.CodegenErrorT Operand
fsub = fbinInsr FSub

fmul :: Operand -> Operand -> C.CodegenErrorT Operand
fmul = fbinInsr FMul

fdiv :: Operand -> Operand -> C.CodegenErrorT Operand
fdiv = fbinInsr FDiv
