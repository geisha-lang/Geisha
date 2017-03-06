module Geisha.Pass.Instruction (

) where

import Data.Maybe
import qualified Data.HashMap as M
import qualified Data.Set as S 
import Data.Set ((\\))
import Data.HashMap ((!))

import Control.Monad.ST
import Data.STRef
import Control.Monad

import qualified Geisha.Pass.Flatten as F
import Geisha.Utils

data InstrArg = Reg String
              | Deref Integer String
              | Var String
              | Int Integer

-- data Instruction = Add InstrArg InstrArg
--                  | Sub InstrArg InstrArg
--                  | Mov InstrArg InstrArg
--                  | Neg InstrArg
--                  | Call InstrArg

data Instruction = Arith String InstrArg InstrArg

op "+" = Add
op "-" = Sub

arg (F.Var var) = Var var
arg (F.Int i)   = Int i
arg _           = undefined

selectInstruction :: [F.Statement] -> [Instruction]
selectInstruction = foldl assignInstr []
  where assignInstr res (F.Return ret) = res ++ [Mov (arg ret) (Reg "rax")]
        assignInstr res (F.Assign lhs rhs) = res ++ case rhs of
          F.Call fun               -> [Mov (Reg "rax") target]
          F.BinInstr bin elhs erhs -> transBin bin elhs erhs
          F.UnInstr uni e          -> [ Mov (arg e) target, Neg target ]
          where target = arg lhs
                transBin o l r = [ Mov (arg l) target,
                                   op o (arg r) target ]





type LiveAfter = S.Set F.Varable
type LiveAfters = [LiveAfter]

liveBefore instr afters = let
  lastAfter = case afters of
    []    -> S.empty
    (a:_) -> a
  
  in flip (:) afters $ case instr of
        Add (Var lhs) _         -> S.insert lhs lastAfter
        Sub (Var lhs) _         -> S.insert lhs lastAfter
        Mov (Var lhs) (Var rhs) -> S.insert lhs $ S.delete rhs lastAfter
        Mov _         (Var rhs) -> S.delete rhs lastAfter
        _                       -> lastAfter

uncoverLive :: [Instruction] -> LiveAfters -> LiveAfters
uncoverLive stmts liveAfters = foldr liveBefore liveAfters stmts

-- transInstrs :: F.Flattened -> (F.Arg, [Instruction], [F.Varable], LiveAfters)
-- transInstrs (arg, stmts, vars) = (arg, instrs, vars, uncoverLive instrs S.empty)
--   where instrs = selectInstruction stmts


buildInterference :: [Instruction] -> [F.Varable] -> LiveAfters -> Graph F.Varable
buildInterference intrs vars las = runST $ do
  let instrAndLa = zip intrs las
  graph <- newSTRef $ makeGraph vars
  forM_ instrAndLa $ \(i, la) -> let
    addToAll source predict = forM_ la $ \v ->
      when (predict v) $ modifySTRef graph $ addEdge source v
    in case i of
      Mov (Var lhs) (Var rhs) ->
        addToAll rhs $ \v -> v /= rhs && v /= lhs
      Mov _         (Var rhs) ->
        addToAll rhs $ const True
      Add _         (Var rhs) ->
        addToAll rhs $ \v -> v /= rhs
      Sub _         (Var rhs) ->
        addToAll rhs $ \v -> v /= rhs
      _                       -> return ()
  readSTRef graph

type ColorMap = M.Map F.Varable Integer

colorGraph :: Graph F.Varable -> [F.Varable] -> ColorMap
colorGraph g vars = runST $ do

  colors <- newSTRef M.empty
  remainder <- newSTRef $ S.fromList vars
  saturation <- newSTRef . M.fromList . zip vars $ repeat (0 :: Integer) -- :: ST s (STRef s (M.Map F.Varable Integer))
  colorOccu <- newSTRef $ makeGraph vars

  -- let getSatur v = fromMaybe 0 . (! v) <$> readSTRef saturation
  let setColor v = do
        l <- adjacent v <$> readSTRef colorOccu
        let minColor = S.findMin $ S.fromList (take (S.findMax l) [1..]) \\ l
        modifySTRef colors $ M.alter (const $ Just minColor) v
        modifySTRef remainder $ S.delete v
        forM_ (adjacent v g) $ \adj -> do
          modifySTRef colorOccu $ addEdge adj minColor
          modifySTRef saturation $ M.adjust (+ 1) adj

  let selectVar = do
        satur <- readSTRef saturation
        remain <- S.toList <$> readSTRef remainder
        let theMax l r = if lhs > rhs then l else r
              where
                lhs = satur ! l
                rhs = satur ! r
 
        return $ foldl theMax (head remain) remain

  forM_ [0..M.size g - 1] $ \i -> do
    next <- selectVar
    setColor next

  readSTRef colors

callerSave = ["rax", "rdx", "rcx", "rsi", "rdi", "r8", "r9", "r10", "r11"]

calleeSave = ["rbx", "r12", "r13", "r14", "r15"]

assignHome :: [Instruction] -> ColorMap -> (Integer, [Instruction])
assignHome instrs colors = ((,)) for intrs $ \instr -> case instr of
  Add 
