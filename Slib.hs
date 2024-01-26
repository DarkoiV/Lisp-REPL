module Slib where 
import Sexpr
import Eval

-- STANDARD LIB FUNCTIONS
add :: Fun 
add vs = let maybeNums = sequence $ map asNumber vs in
  case maybeNums of
    Nothing   -> Err "add requires all values to be numeric"
    Just nums -> Number $ sum nums

sub :: Fun 
sub (a:b:[]) = case (-) <$> (asNumber a) <*> (asNumber b) of
  Nothing  -> Err "sub requires numeric values"
  Just res -> Number res
sub _ = Err "sub requires two operands"

mul :: Fun 
mul vs = let maybeNums = sequence $ map asNumber vs in
  case maybeNums of
    Nothing   -> Err "mul requires all values to be numeric"
    Just nums -> Number $ product nums

divl :: Fun 
divl (a:b:[]) = case (/) <$> (asNumber a) <*> (asNumber b) of
  Nothing  -> Err "div requires numeric values"
  Just res -> Number res
divl _ = Err "div requires two operands"

standardlib = EvalCtx $ 
  [ ("+", Lambda add)
  , ("-", Lambda sub)
  , ("*", Lambda mul)
  , ("/", Lambda divl)
  ]
