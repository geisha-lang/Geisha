{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Geisha.Codegen.LLVM where

import Data.String
import Data.List
import Data.Function

import Control.Monad.State
import Control.Applicative

import LLVM.General.AST as AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST.Constant as C

import qualified LLVM.General.AST.CallingConvention as CC

import qualified Data.Map as M
import Data.Map ((!))

import Geisha.AST as S (ASTType (..), PrimType (..))

instance IsString Name where
  fromString = Name . fromString

type SymbolTable = M.Map String Operand

type Names = M.Map String Int

type Blocks = M.Map Name BlockState
data CodegenState = CodegenState {
  currentBlock :: Name,
  blocks :: Blocks,
  symtable :: SymbolTable,
  blockCount :: Int,
  count :: Word,
  names :: Names
} deriving Show


data BlockState = BlockState {
  idx :: Int,
  stack :: [Named Instruction],
  term :: Maybe (Named Terminator)
} deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) M.empty M.empty 1 0 M.empty

entryBlockName = "entry"

newtype LLVM a = LLVM { unLLVM :: State Module a }
  deriving (Functor, Applicative, Monad, MonadState Module)

runLLVM :: Module -> LLVM a -> Module
runLLVM = flip (execState . unLLVM)

emptyModule :: String -> Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

---------------
-- Data Types
---------------

double :: Type
double = FloatingPointType 64 IEEE

integer :: Type
integer = IntegerType 32

-- function :: Type -> [Type] -> Type
-- function res args =  res args False

-- bool :: Type
-- bool

convertType :: ASTType -> Type
convertType (PrimType I32) = integer
convertType (PrimType F64) = double
convertType (S.FunctionType res args) = AST.FunctionType (convertType res)
                                                         (map convertType args)
                                                         False

convertType _ = error "Type havent been implement"



define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn . GlobalDefinition $ functionDefaults {
  name = Name label,
  parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False),
  returnType = retty,
  basicBlocks = body
}

---------------
-- Blocks
---------------

emptyBlock idx = BlockState idx [] Nothing

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ M.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ show l


entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock blkname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names

  let new = emptyBlock ix
      (qname, supply) = genName blkname nms

  modify $ \s -> s {
    blocks = M.insert (Name qname) new bls,
    blockCount = ix + 1,
    names = supply
  }

  return $ Name qname

setBlock :: Name -> Codegen Name
setBlock blkname = do
  modify $ \s -> s { currentBlock = blkname }
  return blkname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  act <- gets currentBlock
  modify $ \s -> s {
    blocks = M.insert act new $ blocks s
  }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case M.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block " ++ show c



--------------
--
--------------

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = i + 1 }
  return $ i + 1

genName :: String -> Names -> (String, Names)
genName n ns = case M.lookup n ns of
    Just i  -> (n ++ show i, M.insert n (i + 1) ns)
    Nothing -> (n, M.insert n 1 ns)


local :: Name -> Operand
local = LocalReference double

assignVar :: String -> Operand -> Codegen ()
assignVar var val = do
  locals <- gets symtable
  modify $ \s -> s { symtable = M.insert var val locals }

getVar :: String -> Codegen Operand
getVar var = do
  locals <- gets symtable
  case M.lookup var locals of
    Just v -> return v
    Nothing -> error $ "Local varable not in scope " ++ var

instr :: Instruction -> Codegen Operand
instr ins = do
  ref <- UnName <$> fresh
  blk <- current
  let i = stack blk
  modifyBlock $ blk { stack = i ++ [ ref := ins ] }
  return $ local ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock $ blk { term = Just trm }
  return trm

-- jump
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

-- conditional jump
condBr :: Operand -> Name -> Name -> Codegen (Named Terminator)
condBr cond tr fl = terminator $ Do $ CondBr cond tr fl []

-- return
ret :: Operand -> Codegen (Named Terminator)
ret val = terminator . Do $ Ret (Just val) []

call :: Operand -> [Operand] -> Codegen Operand
call fun args = instr $ Call Nothing CC.C [] (Right fun) (toArgs args) [] []
  where toArgs = map $ \arg -> (arg, [])

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []


externf :: Type -> Name -> Operand
externf t = ConstantOperand . C.GlobalReference t
