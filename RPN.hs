module RPN
    ( evaluateRPN
    , infixToRPN
    ) where

import Control.Monad
import Data.List
import Data.Maybe

data Operator = Operator { symbol :: String
                         , associativity :: Ordering -- LT = left-associative, GT = right-associative
                         , precedence :: Int
                         , operation :: Double -> Double -> Double
                         }

instance Show Operator where
    show = show . symbol

instance Read Operator where
    readsPrec _ str = case find (\ op -> str == (symbol op)) operators of
                        Nothing -> []
                        Just op -> [(op,"")]

operators :: [Operator]
operators = [ ( Operator "(" EQ 0 (\ _ _ -> 0 ) )
            , ( Operator ")" EQ 0 (\ _ _ -> 0 ) )
            , ( Operator "+" EQ 1 (+) )
            , ( Operator "*" EQ 2 (*) )
            , ( Operator "-" LT 1 (-) )
            , ( Operator "/" LT 2 (/) )
            , ( Operator "^" GT 3 (**) )
            ]

readMaybe :: (Read a) => String -> Maybe a
readMaybe str = case reads str of
                  [(x,"")] -> Just x
                  _ -> Nothing

evaluateToken :: [Double] -> String -> Maybe [Double]
evaluateToken (x:y:ys) operatorString
    | isJust mop = do
        op <- liftM operation mop
        return $ (op y x):ys
    where mop = readMaybe operatorString
evaluateToken xs numberString = liftM (:xs) (readMaybe numberString)

evaluateRPN :: String -> Maybe Double
evaluateRPN str = do
    [result] <- foldM evaluateToken [] (words str)
    return result

opPopPred :: Operator -> Operator -> Bool
opPopPred op op' = case associativity op of
                     GT -> precedence op < precedence op'
                     _ -> precedence op <= precedence op'

splitStack :: [Operator] -> Operator -> ([Operator], [Operator])
splitStack xs op
    | symbol op == "(" = ([], op:xs)
    | symbol op == ")" = (ys, tail xs')
    where (ys, xs') = break ( (==) "(" . symbol ) xs
splitStack xs op = (ys, op:xs')
    where (ys, xs') = span (opPopPred op) xs

tokenToRPN :: ([Operator], [String]) -> String -> Maybe ([Operator], [String])
tokenToRPN (xs, rpn) operatorString | isJust mop = return $ (xs', rpn ++ (map symbol ys))
    where mop = readMaybe operatorString
          op = fromJust mop
          (ys, xs') = splitStack xs op
tokenToRPN (xs, rpn) numberString = liftM (\ d -> (xs, rpn ++ [show d]) ) (readMaybe numberString :: Maybe Double)

infixToRPN :: String -> Maybe String
infixToRPN str = do
    (stack, result) <- foldM tokenToRPN ([], []) (words str)
    return $ unwords (result ++ map symbol stack)
