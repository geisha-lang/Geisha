import qualified LLVM.General.ExecutionEngine as EE

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where optlevel = Just 2  -- optimization level
        model    = Nothing -- code model ( Default )
        ptrelim  = Nothing -- frame pointer elimination
        fastins  = Nothing -- fast instruction selection
