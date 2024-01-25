module Eval where
import Sexpr
import Data.List (find)
import Text.Read (readMaybe)
import Data.Either
import Control.Monad.State 

data EvalCtx = EvalCtx { getSymbols :: SymbolList }
type Eval    = StateT EvalCtx IO

findSymbol :: String -> Eval Val
findSymbol s = do
   ctx <- get
   let found = find (\(name,_) -> s == name) (getSymbols ctx)
   case found of 
     Nothing       -> return . Err $ "Symbol " ++ s ++ " not found"
     Just (_, val) -> return val

doIO :: IO a -> Eval a
doIO = lift

isTruthy :: Val -> Bool
isTruthy (Number 0) = False
isTruthy (Number _) = True
isTruthy (Val True) = True
isTruthy _          = False

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
