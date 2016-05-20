module PhaseFold where

import Data.List as List

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.BitVector
import Syntax

import Control.Monad.State

{-- Phase folding optimization -}
{- We have two options for implementation here:
 -
 -  1. Maintain a current state of each qubit
 -     over the standard basis. When a Hadamard
 -     gate is applied we then need to write all
 -     phases over the new standard basis.
 -     Cost: 1 XOR per CNOT,
 -           n^2 + n*num keys XORs per H
 -
 -  2. Express each phase as XORs over the current
 -     value of the qubits. Need some way to ensure
 -     the current values of all qubits forms a
 -     basis, otherwise might miss some collisions.
 -     Could map qubit IDs with unique values to unique
 -     indices. On application of a Hadamard we then
 -     only need to remove any phase with a 1 at the
 -     location the Hadamard was applied. Every CNOT
 -     triggers an update of all phases though
 -     Cost: num keys XORs/conditional bit flips per CNOT, 
 -           num keys bit tests per H -} 

data AnalysisState = SOP {
  dim     :: Int,
  qvals   :: Map ID BV,
  terms   :: Map BV (Set Loc, Int),
  orphans :: [(Set Loc, Int)]
} deriving Show

type Analysis = State AnalysisState

bitI :: Int -> Integer
bitI = bit

{- Get the bitvector for variable v, or otherwise allocate one -}
getBV :: ID -> Analysis BV
getBV v = do 
  st <- get
  case Map.lookup v (qvals st) of
    Just bv -> return bv
    Nothing -> do put $ st { dim = dim', qvals = qvals' }
                  return bv'
      where dim' = dim st + 1
            bv' = bitVec dim' $ bitI (dim' -1)
            qvals' = Map.insert v bv' (qvals st)

{- Find a change of coordinate matrix -}
changeOfBasis :: forall a. [(a, BV)] -> (Int, [(a, BV)])
changeOfBasis vs = (0, vs)

{- Write a vector over the given basis -}
writeOverBasis :: forall a. BV -> [BV] -> Maybe BV 
writeOverBasis bv lbv = 
  let bv' = List.foldl' (\s i -> if bv@.i then s `xor` lbv!!i else s) (bitVec 0 0) [0..size bv] in
    if popCount bv' == 0 then Nothing else Just bv'

{- exists removes a variable (existentially quantifies it) then 
 - canonicalizes the state by rewriting all terms over the set
 - of current qubit values, orphaning the ones with no 
 - representation -}
exists :: ID -> AnalysisState -> AnalysisState
exists v st = SOP { dim = dim', qvals = qvals', terms = terms', orphans = orphans' }
  where (rank, basis)  = changeOfBasis $ (Map.toList . Map.delete v) $ qvals st
        dim'           = rank + 1
        bv'            = bitVec dim' $ bitI (dim' -1)
        qvals'         = Map.insert v bv' $ Map.fromList basis
        (terms', orph) = 
          let f (m, xs) bv s = case writeOverBasis bv (snd $ List.unzip basis) of
                Just bv' -> (Map.insert bv' s m, xs)
                Nothing  -> (m, s:xs)
          in
            Map.foldlWithKey f (Map.empty, []) $ terms st
        orphans'       = orph ++ orphans st

updateQval :: ID -> BV -> AnalysisState -> AnalysisState
updateQval v bv st = st { qvals = Map.insert v bv $ qvals st }

addTerm :: Loc -> BV -> AnalysisState -> AnalysisState
addTerm l bv st = st { terms = Map.alter f bv $ terms st }
  where f oldt = case oldt of
          Just (s, x) -> Just (Set.insert l s, x + 1 `mod` 8)
          Nothing     -> Just (Set.singleton l, 1)
 
{-- The main analysis -}
applyGate :: (Primitive, Loc) -> Analysis ()
applyGate (H v, l) = do
  bv <- getBV v
  modify $ exists v

applyGate (CNOT c t, l) = do
  bvc <- getBV c
  bvt <- getBV t
  modify $ updateQval t (bvc `xor` bvt)

applyGate (T v, l) = do
  bv <- getBV v
  modify $ addTerm l bv

runAnalysis :: [ID] -> [ID] -> [Primitive] -> AnalysisState
runAnalysis vars inputs gates =
  let init = 
        SOP { dim = dim', 
              qvals = Map.fromList ivals, 
              terms = Map.empty,
              orphans = [] }
  in
    execState (mapM applyGate $ List.zip gates [0..]) init
  where dim'    = List.length inputs
        bitvecs = [bitVec dim' $ bitI x | x <- [0..]] 
        ivals   = List.zip (inputs ++ (vars \\ inputs)) bitvecs

{- Tests -}
foo = [ T "x", CNOT "x" "y", T "x", T "y", CNOT "y" "x" ]
runFoo = runAnalysis ["x", "y"] ["x", "y"] foo