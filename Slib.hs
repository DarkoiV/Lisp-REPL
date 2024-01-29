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

equal :: Fun
equal vs = Logic $ equal' vs
  where 
    equal' []  = True
    equal' [_] = True
    equal' ((Str x)   :(Str y)   :vs) = (x == y) && (equal' $ (Str y):vs)
    equal' ((Number x):(Number y):vs) = (x == y) && (equal' $ (Number y):vs)
    equal' ((Logic x) :(Logic y) :vs) = (x == y) && (equal' $ (Logic y):vs)
    equal' _ = False

isNumeric :: Fun 
isNumeric vs = case sequence $ map asNumber vs of 
  Just _  -> Logic True 
  Nothing -> Logic False

isString :: Fun 
isString vs = Logic $ isString' vs
  where
    isString' [(Str _)]    = True 
    isString' ((Str _):vs) = True && isString' vs
    isString' _            = False

standardlib = EvalCtx $ 
  [ ("+",          Lambda add)
  , ("-",          Lambda sub)
  , ("*",          Lambda mul)
  , ("/",          Lambda divl)
  , ("is-numeric", Lambda isNumeric)
  , ("is-string",  Lambda isString)
  , ("eq",         Lambda equal)
  ]
