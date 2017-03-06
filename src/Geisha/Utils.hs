module Geisha.Utils (
  Graph,
  makeGraph,
  addEdge,
  adjacent,
  vertices
) where

import Data.HashMap as M
import Data.Set as S

import Data.Maybe

type Graph a = M.Map a (S.Set a)

makeGraph vertices = M.fromList . zip vertices $ repeat S.empty

addEdge start end g = M.update Just start g
  where adj = adjacent start g
        newAdj = S.insert end adj

adjacent u g = fromMaybe S.empty $ M.lookup u g

vertices = M.keys