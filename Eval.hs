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

isTruthy :: Val -> Eval Bool
isTruthy (Number 0)      = return False
isTruthy (Number _)      = return True
isTruthy (Expr e)        = eval e >>= isTruthy
isTruthy (Symbol "true") = return True 
isTruthy (Symbol s)      = findSymbol s >>= isTruthy
isTruthy _               = return False

asNumber :: Val -> Eval (Maybe Float)
asNumber (Number x) = return $ Just x
asNumber (Str s)    = return $ readMaybe s
asNumber (Expr e)   = eval e >>= asNumber
asNumber (Symbol s) = findSymbol s >>= asNumber
asNumber _          = return $ Nothing

eval :: SExpr -> Eval Val
eval (SExpr ((Symbol x):vs)) = findSymbol x >>= \resolved -> eval $ SExpr (resolved:vs)
eval (SExpr ((Lambda f):vs)) = return $ f vs
eval (SExpr ((Err e):_))     = return $ Err e
eval _                       = return $ Err "invalid expression"
