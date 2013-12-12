module RPN
    ( infixToPostfix
    , evaluatePostfix
    , tokens
    , untokens
    , isValidInfix
    ) where

import Control.Monad.State
import Data.List
import Data.List.Zipper (Zipper)
import Data.Ord
import qualified Data.List.Zipper as Z
import qualified Data.Map as M

import RPNLex

-- TODO: type synonyms for associativity, and the stack
-- TODO: better types for precedence and operation

instance Read Token where
    readsPrec x str
        | M.member str nonValueTokens = [(nonValueTokens M.! str, "")]
        | otherwise                   = do
              (y, s) <- readsPrec x str :: [(Double, String)]
              return (Value y, s)

instance Eq Token where
    (==) (Value x) (Value y) = x == y
    (==) (Value _) _         = False
    (==) _ (Value _)         = False
    (==) x y                 = symbol x == symbol y

instance Ord Token where
    compare x@(Operator {}) y@(Operator {}) = comparing precedence x y
    compare (Delimiter {}) (Operator {})    = LT
    compare (Operator {})  (Delimiter {})   = GT
    compare _ _                             = EQ

-- TODO: Better reporting of error conditions
--      More than one value on stack after expression evaluated
--      Not enough values to pop
evaluateToken :: Token -> Zipper Token -> Maybe (Zipper Token)
evaluateToken tok@(Value _) z             = return $ Z.insert tok z
evaluateToken (Operator {operation=op}) z = do
    (Value x) <- Z.safeCursor z
    let z' = Z.delete z
    (Value y) <- Z.safeCursor z'
    return $ Z.replace (Value (y `op` x)) z'

evaluatePostfix :: [Token] -> Maybe Double
evaluatePostfix xs = do
    [Value result] <- liftM Z.toList . foldl' (>>=) (return Z.empty) . map evaluateToken $ xs
    return result

popUntil :: (Token -> Bool) -> Zipper Token -> Zipper Token
popUntil p z
    | (not . Z.endp) z && (not . p) (Z.cursor z) = popUntil p (Z.right z)
    | otherwise                                  = z

opPopPred :: Token -> Token -> Bool
opPopPred op = case associativity op of
                    GT -> (<=op)
                    _  -> (<op)

processToken :: Token -> Zipper Token -> Zipper Token -- Add error handling
processToken tok@(Delimiter {symbol = "("}) = Z.insert tok -- push onto stack
processToken     (Delimiter {symbol = ")"}) = Z.delete . popUntil (== read "(")
processToken tok@(Operator {})              = Z.insert tok . popUntil (opPopPred tok)
processToken tok@(Value _)                  = Z.push tok -- append to result

infixToPostfix :: [Token] -> [Token]
infixToPostfix = Z.toList . flip execState Z.empty . mapM (modify . processToken)

-- TODO: Add an actual user interface. Parse better (whitespace). Give meaningful errors. Quickcheck. Write the Agda version.

-- Validator --

verifyInfix :: Int -> Bool -> [Token] -> Bool
verifyInfix 0 False []                              = True
verifyInfix x True  ((Delimiter {symbol = "("}):ys) = verifyInfix (x + 1) True ys
verifyInfix 0 _     ((Delimiter {symbol = ")"}):_)  = False
verifyInfix x False ((Delimiter {symbol = ")"}):ys) = verifyInfix (x - 1) False ys
verifyInfix x True  ((Value _):ys)                  = verifyInfix x False ys
verifyInfix x False ((Operator {}):ys)              = verifyInfix x True ys
verifyInfix _ _ _                                   = False

isValidInfix :: [Token] -> Bool
isValidInfix = verifyInfix 0 True
