import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Control.Monad

tokenizer :: Eq a => ([a] -> (b, [a])) -> [a] -> [b]
tokenizer f [] = []
tokenizer f body = tok : tokenizer f rst where
 (tok, rst) = f body

data TokType = TName
             | TSymbol
             | TComment
             | TSpace
             | TStr
             | TChar
             | TDec
             | THex
             | TBool
             | TKeyword
             | TError
             deriving (Eq, Show)

data Token = Token TokType String deriving (Eq, Show)

type Split a = a -> (a, a)

takeLineComment :: Split String
takeLineComment = break (== '\n')

takeBlockComment :: Split String
takeBlockComment = take' 2 where
  take' :: Int -> Split String
  take' n str = case (drop n str) of
    ('*':'/':cs) -> splitAt (n+2) str
    (c:cs) -> take' (n+1) str
    [] -> error "unterminated block comment"

takeStr :: Split String
takeStr = take' 1 where
  take' :: Int -> Split String
  take' n str = case (drop n str) of
    ('\\':'"':cs) -> take' (n+2) str
    ('"':cs) -> splitAt (n+1) str
    (c:cs) -> take' (n+1) str
    [] -> error "unterminated string literal"

takeChar :: Split String
takeChar = take' 1 where
  take' :: Int -> Split String
  take' n str = case (drop n str) of
    ('\\':'\'':cs) -> take' (n+2) str
    ('\'':cs) -> splitAt (n+1) str
    (c:cs) -> take' (n+1) str
    [] -> error "unterminated character literal"

takeHex :: Split String
takeHex = (\(a, b) -> ("0x" ++ a, b)) . span isHexDigit . drop 2

takeSymbol :: Split String
takeSymbol = break (isLetter |.| isNumber |.| isSpace)

takeName :: Split String
takeName = span (isLetter |.| isNumber |.| (`elem` "_"))

(&.&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(f &.& g) x = f x && g x

(|.|) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(f |.| g) x = f x || g x

keywords :: [String]
keywords = ["if", "else", "while", "for"]

getToken :: String -> (Token, String)
getToken = takeToken =<< decideToken where
  takeToken :: (TokType, Split String) -> String -> (Token, String)
  takeToken (t, f) body = (Token t a, b) where (a, b) = f body

  decideToken :: String -> (TokType, Split String)
  decideToken "" = error "decideToken applied to empty string"
  decideToken body
    | a "/*" = (TComment, takeBlockComment)
    | a "//" = (TComment, takeLineComment)
    | a "\"" = (TStr,     takeStr)
    | a "'"  = (TChar,    takeChar)
    | a "0x" = (THex,     span isHexDigit)
 -- |
    | b isDigit  = (TDec,   span isDigit)
    | b isLetter = (TName,  takeName)
    | b isSpace  = (TSpace, span isSpace)
 -- |
    | otherwise = (TSymbol, takeSymbol)
    where a :: String -> Bool
          a = (`isPrefixOf` body)
          b :: (Char -> Bool) -> Bool
          b = ($ head body)

tokenize :: String -> [Token]
tokenize = tokenizer getToken

type Foo a = [(String, a)] -- need a better name

data BinaryOp = BAdd
              | BSubtract
              | BMultiply
              | BDivide
              | BEqual
              | BNotEqual
              | BAssign
              | BLessThan
              | BGreaterThan
              | BLessEq
              | BGreaterEq
              deriving (Eq, Show)
binaryOps :: Foo BinaryOp
binaryOps = [
              ("+",  BAdd),
              ("-",  BSubtract),
              ("*",  BMultiply),
              ("-",  BDivide),
              ("==", BEqual),
              ("!=", BNotEqual),
              ("=",  BAssign),
              ("<",  BLessThan),
              (">",  BGreaterThan),
              ("<=", BLessEq),
              (">=", BGreaterEq),
            ]

{-
data UnaryPreOp = UNegative | UNot | USize deriving (Eq, Show)
unaryPreOps :: Foo UnaryPreOp
unaryPreOps = [("-", UNegative), ("!", UNot), ("#", USize)]
data UnaryPostOp = UIncrement | UDecrement | UBang deriving (Eq, Show)
unaryPostOps :: Foo UnaryPostOp
unaryPostOps = [("++", UIncrement), ("--", UDecrement), ("!",  UBang)]
-}
