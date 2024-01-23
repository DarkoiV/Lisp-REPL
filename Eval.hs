module Eval where
import Sexpr
import Data.List (find)

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
    Nothing -> (Err $ sym ++ " not found", ctx)

isTruthy :: Val -> Bool
isTruthy (Number 0) = False
isTruthy (Number _) = True
isTruthy (Nil)      = False
isTruthy (Expr e)   = False
isTruthy _          = False

