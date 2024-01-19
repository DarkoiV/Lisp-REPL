module Parser where 
import Control.Applicative
import Control.Arrow

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where 
  fmap f (Parser p) = Parser $ \str -> 
    p str >>= \(x, str') -> 
    return (f x, str')

instance Applicative Parser where 
  pure x = Parser $ \str -> Just (x, str)

  (Parser pf) <*> (Parser px) = Parser $ \str -> 
    pf str  >>= \(f, str')  ->
    px str' >>= \(x, str'') ->
    return (f x, str'')

instance Monad Parser where 
  (Parser p) >>= f = Parser $ \str ->
    p str >>= \(x, str') ->
    runParser (f x) str'

instance Alternative Parser where 
  empty = Parser $ \_ -> Nothing
  
  (Parser lp) <|> (Parser rp) = Parser $ \str ->
    case lp str of 
      Just lp -> Just lp
      Nothing -> rp str

charP :: Char -> Parser Char
charP c = Parser fn
  where 
    fn []         = Nothing
    fn (x:xs) 
      | c == x    = Just (c, xs)
      | otherwise = Nothing 

allUntilCharP :: Char -> Parser String 
allUntilCharP c = Parser fn
  where 
    fn []         = Nothing
    fn (x:xs)
      | c == x    = Just ([], x:xs)
      | otherwise = (\(s, str) -> (x : s, str)) <$> fn xs 

stringP :: String -> Parser String 
stringP = mapM charP

whitespaceP :: Parser Char
whitespaceP = charP ' ' <|> charP '\n' <|> charP '\r' <|> charP '\t'

skipwsP :: Parser String
skipwsP = some whitespaceP

ignorewsP :: Parser String 
ignorewsP = many whitespaceP

digitP :: Parser Char 
digitP = foldr (<|>) empty $ map charP ['0' .. '9']

numberP :: Parser Float
numberP = read <$> (decimal <|> integral)
  where 
    integral = some digitP 
    decimal  = do
      bp <- some digitP
      p  <- charP '.'
      ap <- some digitP
      return (bp ++ (p : ap)) 

stringliteralP :: Parser String 
stringliteralP = do
  charP '\"'
  str <- allUntilCharP '\"'
  charP '\"'
  return str
