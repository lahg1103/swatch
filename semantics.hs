module Semantics where
import Syntax
import Debug.Trace (trace)
import Data.List (elemIndex)
evaluate :: Program -> Env
evaluate p = evaluateProg p []

evaluateProg :: [Stmt] -> Env -> Env
evaluateProg [] env = env
evaluateProg (s : ss) env = let env' = evaluateStmt s env
                                in evaluateProg ss env'
