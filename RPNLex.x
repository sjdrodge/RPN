{

module RPNLex
    ( tokens
    , untokens
    , Token (..)
    ) where

import Data.Map (Map)
import qualified Data.Map as M

}

%wrapper "posn"

$digit = 0-9

tokens :-

  $white+                ;
  $digit+(\.$digit+)?    { \_ s -> Value (read s) }
  [\(\)\+\-\*\/\^]       { \_ s -> nonValueTokens M.! s }

{

data Token = Value Double
           | Operator  { symbol :: String
                       , associativity :: Ordering -- LT = left-associative, GT = right-associative
                       , precedence :: Int
                       , operation :: Double -> Double -> Double
                       }
           | Delimiter { symbol :: String
                       , associativity :: Ordering
                       }

instance Show Token where
    show (Value x) = show x
    show x = symbol x

nonValueTokens = M.fromList $ map ( (,) =<< symbol )
                 [ Delimiter "(" GT
                 , Delimiter ")" LT
                 , Operator  "+" EQ 6 (+)
                 , Operator  "*" EQ 7 (*)
                 , Operator  "-" LT 6 (-)
                 , Operator  "/" LT 7 (/)
                 , Operator  "^" GT 8 (**)
                 ]

tokens :: String -> [Token]
tokens = alexScanTokens

untokens :: [Token] -> String
untokens = unwords . map show

}
