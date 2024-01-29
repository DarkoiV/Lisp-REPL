module Eval where
import Sexpr
import Data.List (find, findIndex, intercalate)
import Text.Read (readMaybe)
import Data.Either
import Control.Monad.State 

data EvalCtx = EvalCtx { getSymbols :: SymbolList }
type Eval    = StateT EvalCtx IO

findSymbol :: String -> Eval Val
findSymbol s = do
   ctx <- get
   let found = find (isSymbol s) (getSymbols ctx)
   case found of 
     Nothing       -> return . Err $ "Symbol " ++ s ++ " not found"
     Just (_, val) -> return val

putSymbol :: String -> Val -> Eval () 
putSymbol s v = do
  ctx <- get
  let slist = (getSymbols ctx)
  let new = (s,v)
  let indx = findIndex (isSymbol s) slist 
  case indx of 
    Nothing -> do put $ EvalCtx $ new:slist 
    Just i  -> do
      let pre = take i slist 
      let post = drop (i+1) slist 
      put $ EvalCtx $ pre ++ (new : post)

doIO :: IO a -> Eval a
doIO = lift

resolve :: Val -> Eval Val
resolve (Symbol s) = findSymbol s >>= resolve
resolve (Expr e)   = eval e
resolve x          = pure x

isTruthy :: Val -> Eval Bool
isTruthy (Number 0)   = return False
isTruthy (Number _)   = return True
isTruthy (Logic True) = return True
isTruthy (Expr e)     = eval e >>= isTruthy
isTruthy _            = return False

asNumber :: Val -> Maybe Float
asNumber (Number x) = Just x
asNumber (Str s)    = readMaybe s
asNumber _          = Nothing

cond :: [Val] -> Eval Val
cond [p,t,e] = do
  condRes <- resolve p >>= isTruthy
  if condRes
    then resolve t
    else resolve e
cond [p,t] = do
  condRes <- resolve p >>= isTruthy
  if condRes
    then resolve t
    else return Nil
cond _ = return $ Err "invalid cond statement"

define :: [Val] -> Eval Val
define [(Symbol name), x] = do
  x <- resolve x
  doIO $ putStrLn (name ++ " = " ++ (show x))
  putSymbol name x
  return Nil
define _ = return $ Err "invalid define statement"

printv :: [Val] -> Eval Val 
printv vals = doIO $ putStrLn (concatMap toPrint vals) >> return Nil
  where 
    toPrint (Str s) = s 
    toPrint x       = show x

doTasks :: [Val] -> Eval Val 
doTasks vs = do' Nil vs 
  where 
    do' res []   = return res 
    do' _ (t:ts) = resolve t >>= \r -> do' r ts

bind :: [Val] -> Val 
bind [(Expr binding),target] = case bindList binding of 
  Just list -> bind' list target
  Nothing   -> Err "failed to resolve bind list"
  where
    bindList []                  = Just []
    bindList ((Symbol s):val:vs) = (:) <$> pure (s,val) <*> bindList vs
    bindList _                   = Nothing 
    bind' list (Expr e)   = Expr $ map (bind' list) e
    bind' list (Symbol s) = let found = find ((s==) . fst) list in 
      case found of 
        Just (_, value) -> value 
        Nothing         -> Symbol s
    bind' _    x          = x
bind _ = Err "invalid bind statement"

lambda :: [Val] -> Val 
lambda [(Expr params),(Expr e)]
  | allSymbols params = Lambda $ \vals -> 
    if length vals /= length params 
      then Err $ "Lambda arity mismatch, expected: " ++ show (length params)
      else let args = Expr $ merge params vals 
        in bind [args, (Expr e)]
  | otherwise = Err "invalid lambda expressions"
  where 
    allSymbols []              = True 
    allSymbols ((Symbol _):vs) = True && (allSymbols vs)
    allSymbols _               = False
    merge []     []     = []
    merge (b:bs) (v:vs) = b : v : merge bs vs
lambda _ = Err "invalid lambda statement"

-- Eval --
eval ((Symbol x):vs) 
  | x == "cond"   = cond vs
  | x == "define" = define vs
  | x == "print"  = sequence (map resolve vs) >>= printv
  | x == "do"     = doTasks vs
  | x == "bind"   = resolve $ bind vs
  | x == "lambda" = return $ lambda vs
  | x == "read"   = Str <$> doIO getLine
  | otherwise     = findSymbol x >>= \resolved -> eval $ (resolved:vs)
eval ((Lambda f):vs) = resolve =<< f <$> (sequence $ map resolve vs)
eval ((Err e):_)     = return $ Err e
eval _               = return $ Err "invalid expression"

runEval :: (Eval a) -> EvalCtx -> IO ()
runEval fn ctx = runStateT fn ctx >> return () 
