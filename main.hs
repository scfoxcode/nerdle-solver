module Main (main) where
import System.IO
import Data.Maybe (fromJust, isNothing)
import Control.Monad.State
import Data.List
import Data.Text (splitOn, unpack, pack, Text) 
import Text.Read

wordLength = 6

allNumbers = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
allOperators = ['/', '*', '+', '-']
allPossibleValues = concat [allNumbers, allOperators, ['=']]

numbersExcludingZero = filter (\c -> not $ c == '0') allNumbers

data Token = TokenValue Int | TokenOperator Char
isValue :: Token -> Bool
isValue (TokenValue _) = True
isValue _ = False

isOperator :: Token -> Bool
isOperator (TokenOperator _) = True
isOperator _ = False
getOperator :: Token -> Char
getOperator (TokenOperator op) = op
getValue :: Token -> Int 
getValue (TokenValue val) = val 


-- BEGIN EVALUATE STRING EXPRESSION FUNCTIONS --

-- Returns true if a has higher priority than b
compareOperatorPriority :: Token -> Token -> Bool
compareOperatorPriority a b 
    | (isValue a) || (isValue b) = False
    | isNothing indexA || isNothing indexB = False
    | otherwise = indexA <= indexB 
    where
        indexA = elemIndex (getOperator a) allOperators
        indexB = elemIndex (getOperator b) allOperators


-- Finds the operator with the highest precidence in the tokens
findPriorityOperator :: Int -> Maybe (Token, Int) -> [Token] -> Maybe (Token, Int)
findPriorityOperator index found [] = found
findPriorityOperator index found tokens
    | isValue thisToken = findPriorityOperator newIndex found remaining 
    | isNothing found = findPriorityOperator newIndex (Just (thisToken, index)) remaining 
    | otherwise = do
    foundValue <- found
    if compareOperatorPriority thisToken (fst foundValue)
        then findPriorityOperator newIndex (Just (thisToken, index)) remaining 
        else findPriorityOperator newIndex found remaining 
    where
        newIndex = index + 1
        remaining = tail tokens
        thisToken = head tokens

-- build new token list with a number added 
addNumber :: String -> [Token] -> [Token]
addNumber value tokens = do
    let num = readMaybe value :: Maybe Int
    if isNothing num 
        then tokens
        else concat [tokens, [TokenValue (fromJust num)]]

-- build new token list with a operator added
addOp :: Char -> [Token] -> [Token]
addOp operator tokens = concat [tokens, [TokenOperator operator]]

-- Produce tokens from the raw strings
tokenize :: String -> String -> [Token] -> [Token]
tokenize input part tokens 
    | (length input) < 1 = if (length part) > 0 then addNumber part tokens else tokens
    | elem first allOperators =
        if (length part) > 0
            then do
                let output = addOp first (addNumber part tokens)
                tokenize remaining "" output
        else
            tokenize remaining "" (addOp first tokens)
    | otherwise = tokenize remaining (part ++ [first]) tokens 
    where
        first = head input
        remaining = tail input

-- Takes a token and two value operators, applying them and returning a new result token
performCalculation :: Token -> Token -> Token -> Token
performCalculation (TokenOperator op) (TokenValue left) (TokenValue right)
    | op == '/' = TokenValue (left / right)
    | op == '*' = TokenValue (left * right)
    | op == '+' = TokenValue (left + right)
    | op == '-' = TokenValue (left - right)
    | otherwise = TokenValue 100000 -- Another massive hack, we need error handling
    

-- recurse over operators
evaluateTokens :: [Token] -> Int
evaluateTokens tokens 
    | (length tokens) == 1 =
        if isValue token
            then getValue token     
            else 0  -- hack, we need proper error handling
    | otherwise = do
        let index = findPriorityOperator 0 Nothing tokens       
        if isNothing index
            then 0 -- another hack
            else do
                1 
        
    where token = head tokens

substring :: Int -> Int -> String -> String
substring i j k = take (j - i) (drop i k)

-- Splits the equation into it's left and right sides
splitOnEquals :: String -> (String, String)
splitOnEquals expression = do
    if (elem '=' expression) 
        then do
            let result = splitOn (pack "=") (pack expression)
            (unpack $ result !! 0, unpack $ result !! 1)
        else ("1", "0") -- Should never get here but return false expression if we do

-- Check if an equation is correct
equalityCheck :: String -> Bool
equalityCheck equation = do
    let sides = splitOnEquals equation 
    let left = tokenize (fst sides) "" []
    let right = tokenize (snd sides) "" []
    (evaluateTokens left) == (evaluateTokens right)

-- END EVALUATE STRING EXPRESSION FUNCTIONS --

-- Allowed values based on character index and previous value
allowedValues :: Int -> Char -> [Char]
allowedValues index prev
    | index >= (wordLength -1) = allNumbers
allowedValues index '=' = allNumbers 
allowedValues index '/' = numbersExcludingZero -- Rule out divide by zero
allowedValues index prev
    | elem prev allOperators = allNumbers
    | otherwise = allPossibleValues

-- Build strings that match basic rules
buildExpression :: Int -> Char -> String -> [String]
buildExpression index value acc 
    | (index >= wordLength) = [acc]
    | otherwise = do
        let values = allowedValues index value
        concat $ map (\x -> buildExpression (index + 1) x (acc ++ [x])) values

-- Build strings for all valid starting numbers
buildAllStrings :: [Char] -> [String]
buildAllStrings values = concat $ map (\x -> buildExpression 1 x [x]) values

-- We have symmetry, remove all where = appears in first 4 chars
-- Can only get away with this because Nerdle uses an even number of cells
filterSymmetrical :: String -> Bool
filterSymmetrical value
    | index == Nothing = False
    | index < (Just 4) = False
    | otherwise = True
    where index = elemIndex '=' value

-- Return True if the string has no operators 
hasNoOperators :: String -> Bool
hasNoOperators value =
    elem True (map (\x -> elem x value) allOperators) == False 

-- Filter the string list to remove obviously invalid answers 
isLegal :: String -> Bool
isLegal value
    | not $ (length $ filter (\x -> x == '=') value) == 1 = False -- Must have 1 equals
    | hasNoOperators value == True = False -- Must have >0 operators 
    | otherwise = filterSymmetrical value && equalityCheck value

-- First pass removing incorrect expressions from the output
removeClearlyWrongSolutions :: [String] -> [String]
removeClearlyWrongSolutions inputStrings = filter isLegal inputStrings
    
main :: IO ()
main = do
    putStrLn "Mr Anderson, Welcome back."
    putStrLn "Building expressions list. Please Standbye..."
    outfile <- openFile "expressions.txt" WriteMode
    let rawAnswers = buildAllStrings numbersExcludingZero
    let firstClean = removeClearlyWrongSolutions rawAnswers
    mapM_ (\x -> hPutStrLn outfile x) firstClean
    -- mapM_ print (buildAllStrings numbersExcludingZero)
    
