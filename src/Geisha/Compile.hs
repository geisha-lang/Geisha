module Geisha.Compile where

import Geisha.AST

newtype CompileState = CompileState {
  _fname :: String,
  _source :: String,
  _synAST :: 
 }