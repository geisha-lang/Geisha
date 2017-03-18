{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Geisha.Codegen.LLVM where

import Data.String
import Data.List
import Data.Function

import Control.Monad.State
import Control.Applicative
import Control.Monad.Except

import LLVM.General.AST as AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST.Constant as C

import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import qualified Data.Map as M
import Data.Map ((!))

import Geisha.AST as S (GType (..))
import qualified Geisha.Error as E

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
  names :: Names,
  errors :: [E.CompileErr]
} deriving Show


data BlockState = BlockState {
  idx :: Int,
  stack :: [Named Instruction],
  term :: Maybe (Named Terminator)
} deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

type CodegenErrorT = ExceptT E.CompileErr Codegen

execCodegen :: Codegen (E.ThrowsCompileErr a) -> E.ThrowsCompileErr CodegenState
execCodegen m = case r of
  Right _ -> return s
  Left err -> throwError err
  where (r, s) = runState (runCodegen m) emptyCodegen

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) M.empty M.empty 1 0 M.empty []

entryBlockName = "entry"

newtype LLVM a = LLVM { unLLVM :: State Module a }
  deriving (Functor, Applicative, Monad, MonadState Module)

runLLVM :: Module -> LLVM a -> Module
runLLVM = flip (execState . unLLVM)

runLLVMOrThrow :: Module -> LLVM (E.ThrowsCompileErr a) -> E.ThrowsCompileErr Module
runLLVMOrThrow m l = case a of
  Right _  -> return s
  Left err -> throwError err
  where (a, s) = runState (unLLVM l) m

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

-- convertType :: GType -> Type
-- convertType (Primitive I32) = integer
-- convertType (Primitive F64) = double
-- convertType (S.FunctionType res args) = AST.FunctionType (convertType res)
--                                                          (map convertType args)
--                                                          False

-- convertType _ = error "Type havent been implement"



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

createBlocks :: CodegenState -> E.ThrowsCompileErr [BasicBlock]
createBlocks m = return . map makeBlock . sortBlocks $ M.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ show l


entry :: CodegenErrorT Name
entry = lift $ gets currentBlock

addBlock :: String -> CodegenErrorT Name
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

setBlock :: Name -> CodegenErrorT Name
setBlock blkname = do
  modify $ \s -> s { currentBlock = blkname }
  return blkname

getBlock :: CodegenErrorT Name
getBlock = lift $ gets currentBlock

modifyBlock :: BlockState -> CodegenErrorT ()
modifyBlock new = lift $ do
  act <- gets currentBlock
  modify $ \s -> s {
    blocks = M.insert act new $ blocks s
  }

current :: CodegenErrorT BlockState
current = lift $ do
  c <- gets currentBlock
  blks <- gets blocks
  case M.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block " ++ show c


addError :: E.CompileErr -> CodegenErrorT ()
addError err = lift $ do
  cur <- gets errors
  modify $ \s -> s {
    errors = err : cur
  }
--------------
--
--------------

fresh :: CodegenErrorT Word
fresh = lift $ do
  i <- gets count
  modify $ \s -> s { count = i + 1 }
  return $ i + 1

genName :: String -> Names -> (String, Names)
genName n ns = case M.lookup n ns of
    Just i  -> (n ++ show i, M.insert n (i + 1) ns)
    Nothing -> (n, M.insert n 1 ns)


local :: Name -> Operand
local = LocalReference double

assignVar :: String -> Operand -> CodegenErrorT ()
assignVar var val = lift $ do
  locals <- gets symtable
  modify $ \s -> s { symtable = M.insert var val locals }

getVar :: String -> CodegenErrorT Operand
getVar var = do
  locals <- gets symtable
  case M.lookup var locals of
    Just v -> return v
    Nothing -> throwError $ E.Unbound var

instr :: Instruction -> CodegenErrorT Operand
instr ins = do
  ref <- UnName <$> fresh
  blk <- current
  let i = stack blk
  modifyBlock $ blk { stack = i ++ [ ref := ins ] }
  return $ local ref

terminator :: Named Terminator -> CodegenErrorT (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock $ blk { term = Just trm }
  return trm

-- jump
br :: Name -> CodegenErrorT (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> CodegenErrorT (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []
-- conditional jump
condBr :: Operand -> Name -> Name -> CodegenErrorT (Named Terminator)
condBr cond tr fl = terminator $ Do $ CondBr cond tr fl []

-- return
ret :: Operand -> CodegenErrorT (Named Terminator)
ret val = terminator . Do $ Ret (Just val) []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> CodegenErrorT Operand
fcmp cond a b = instr $ FCmp cond a b []

phi :: Type -> [(Operand, Name)] -> CodegenErrorT Operand
phi ty incoming = instr $ Phi ty incoming []

call :: Operand -> [Operand] -> CodegenErrorT Operand
call fun args = instr $ Call Nothing CC.C [] (Right fun) (toArgs args) [] []
  where toArgs = map $ \arg -> (arg, [])

alloca :: Type -> CodegenErrorT Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> CodegenErrorT Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> CodegenErrorT Operand
load ptr = instr $ Load False ptr Nothing 0 []


externf :: Type -> Name -> Operand
externf t = ConstantOperand . C.GlobalReference t
