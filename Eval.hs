module Eval where
import Sexpr
import Data.List (find, findIndex)
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

resolve :: [Val] -> Eval [Val]
resolve []              = pure []
resolve ((Expr e):vs)   = (:) <$> eval e <*> resolve vs
resolve ((Symbol s):vs) = (:) <$> (findSymbol s >>= resolve') <*> resolve vs
  where
    resolve' (Expr e) = eval vs
    resolve' x        = pure x

resolve (v:vs) = (:) <$> pure v <*> resolve vs

eval :: [Val] -> Eval Val 

-- Specialized eval --
eval ((Symbol "cond"):vs) = case vs of
  (p:t:e:[]) -> do
    condRes <- isTruthy p
    if condRes
      then return t
      else return e
  (p:t:[]) -> do
    condRes <- isTruthy p
    if condRes
      then return t
      else return Nil
  otherwise -> return $ Err "invalid cond statement"

eval ((Symbol "define"):vs) = case vs of
  ((Symbol name):x:[]) -> do
    doIO $ putStrLn (name ++ " = " ++ (show x))
    putSymbol name x
    return Nil
  otherwise -> return $ Err "invalid define statement"

-- Generic Eval --
eval ((Symbol x):vs) = findSymbol x >>= \resolved -> eval $ (resolved:vs)
eval ((Lambda f):vs) = f <$> (resolve vs)
eval ((Err e):_)     = return $ Err e
eval _               = return $ Err "invalid expression"

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

defaultCtx = EvalCtx $ [ ("+", Lambda add)
                       , ("-", Lambda sub)
                       , ("*", Lambda mul)
                       , ("/", Lambda divl)
                       ]
startEval :: (Eval a) -> IO ()
startEval fn = runStateT fn defaultCtx >> return ()
