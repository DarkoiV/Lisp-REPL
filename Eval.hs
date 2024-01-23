module Eval where
import Sexpr
import Data.List (find)
import Text.Read (readMaybe)

data EvalCtx = EvalCtx { getSymbols :: SymbolList }
data Eval a  = Eval { runEval :: EvalCtx -> (a, EvalCtx) }

instance Functor Eval where 
  fmap f (Eval eval) = Eval $ \ctx -> 
    let (res, ctx') = eval ctx 
    in (f res, ctx')

instance Applicative Eval where
  pure x = Eval $ \ctx -> (x, ctx)
  (Eval ef) <*> (Eval ev) = Eval $ \ctx -> 
    let (f, ctx')  = ef ctx 
        (v, ctx'') = ev ctx' 
    in (f v, ctx'') 

instance Monad Eval where
  (Eval eval) >>= f = Eval $ \ctx -> 
    let (v, ctx') = eval ctx 
    in runEval (f v) ctx'

findSymbol :: String -> Eval Val 
findSymbol sym = Eval $ \ctx ->
  let found = find (\(name,_) -> name == sym) (getSymbols ctx) in 
  case found of 
    Just x  -> (snd x, ctx)
    Nothing -> (Err $ "Symbol: \"" ++ sym ++ "\" not found", ctx)

isTruthy :: Val -> Bool
isTruthy (Number 0) = False
isTruthy (Number _) = True
isTruthy (Val True) = True
isTruthy _          = False

asNumber :: Val -> Maybe Float
asNumber (Number x) = Just x
asNumber (Str s)    = readMaybe s
asNumber _          = Nothing

eval :: SExpr -> Eval Val
eval (SExpr ((Symbol x):vs)) = findSymbol x >>= \resolved -> eval $ SExpr (resolved:vs)
eval (SExpr ((Lambda f):vs)) = return $ f vs
eval (SExpr ((Err e):_))     = return $ Err e
eval _                       = return $ Err "invalid expression"

add :: Fun 
add vs = let maybeNums = sequence $ map asNumber vs in
  case maybeNums of
    Nothing   -> Err "add requires all values to be numeric"
    Just nums -> Number $ sum nums

defaultCtx = EvalCtx $ [("+", Lambda add)]
