module Main (main) where
import System.IO
import Data.Maybe (fromJust, isNothing)
import Control.Monad.State
import Data.List
import Data.Text (splitOn, unpack, pack, Text) 
import Text.Read

wordLength = 8
maxDigits = 3

allNumbers = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
allOperators = ['/', '*', '+', '-']
allPossibleValues = concat [allNumbers, allOperators, ['=']]

numbersExcludingZero = filter (\c -> not $ c == '0') allNumbers

data Token = TokenValue Int | TokenOperator Char
    deriving (Show)
isValue :: Token -> Bool
isValue (TokenValue _) = True
isValue _ = False

isOperator :: Token -> Bool
isOperator (TokenOperator _) = True
isOperator _ = False

getOperator :: Token -> Char
getOperator (TokenOperator op) = op

getString :: Token -> String
getString (TokenOperator op) = [op]
getString (TokenValue val) = show val 

getValue :: Token -> Int 
getValue (TokenValue val) = val 

-- Bad function but easiest way to do it for nerdles case
getLength :: Token -> Int
getLength (TokenOperator _) = 1
getLength (TokenValue val)
    | val < 10 = 1
    | val < 100 = 2
    | val < 1000 = 3
    | otherwise = 3 -- See, not good

type TokenEquation = ([Token], [Token])
type TokenExpression = [Token]

getStringExpr :: TokenExpression -> String
getStringExpr expr = concatMap getString expr

-- Returns a string representation of the equation
stringEquation :: TokenEquation -> String
stringEquation (left, right) = (getStringExpr left) ++ "=" ++ (getStringExpr right)

lenExpr :: TokenExpression -> Int
lenExpr expr = foldl (\acc ex -> acc + (getLength ex)) 0 expr

data Tree = Empty | Node 

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

nerdleDivide :: Int -> Int -> Maybe Int
nerdleDivide x y 
    | x < y = Nothing
    | y == 0 = Nothing
    | (rem x y) /= 0 = Nothing 
    | otherwise = Just (div left right)
    where
        left = fromIntegral x 
        right = fromIntegral y 

-- Takes a token and two value operators, applying them and returning a new result token
performCalculation :: Token -> Token -> Token -> Maybe Token
performCalculation (TokenOperator op) (TokenValue left) (TokenValue right)
    | op == '/' = do
        let result = nerdleDivide left right 
        if isNothing result
            then Nothing
            else do
                divisionResult <- result
                Just (TokenValue divisionResult)
    | op == '*' = Just (TokenValue (left * right))
    | op == '+' = Just (TokenValue (left + right))
    | op == '-' = Just (TokenValue (left - right))
    | otherwise = Nothing 
    
-- tests if index is valid for operator in list
isOperatorIndexValid :: [Token] -> Int -> Bool
isOperatorIndexValid tokens index
    | index < 1 || index >= (tLength -1) || tLength < 3 = False
    | otherwise = True
    where tLength = length tokens

-- recurse over operators
evaluateTokens :: [Token] -> Maybe Int
evaluateTokens tokens 
    | (length tokens) == 1 =
        if isValue token
            then Just (getValue token)     
            else Nothing 
    | otherwise = do
        let op = findPriorityOperator 0 Nothing tokens       
    
        if isNothing op then Nothing 
        else do
            operator <- op
            let index = snd operator
            let valid = isOperatorIndexValid tokens index
            if not valid then Nothing
            else do
                let result = performCalculation (tokens !! index) (tokens !! (index - 1)) (tokens !! (index + 1)) 
                resultValue <- result
                if isNothing result then Nothing 
                else 
                    evaluateTokens (concat [take (index - 1) tokens, [resultValue], drop (index + 2) tokens]) 
        
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

-- Compare two maybe ints for equality, got to be a default way of doing this
areIntsEqual :: Maybe Int -> Maybe Int -> Bool
areIntsEqual Nothing b = False
areIntsEqual a Nothing = False
areIntsEqual (Just a) (Just b) = a == b

-- Check if an equation is correct
equalityCheck :: TokenEquation -> Bool
equalityCheck equation = do
    let left = evaluateTokens (fst equation) 
    let right = evaluateTokens (snd equation) 
    if isNothing left || isNothing right
        then False
        else areIntsEqual left right 

-- END EVALUATE STRING EXPRESSION FUNCTIONS --

-- START EXPRESSION GENERATION --

-- Returns all possible TokenValues of specified length
buildNumberTokensLen :: Int -> [Token]
buildNumberTokensLen 0 = []
buildNumberTokensLen 1 = map TokenValue [0..9]
buildNumberTokensLen len = map TokenValue [(10 ^ (len - 1))..(10 ^ len - 1)] 

-- Returns all possible TokenValues of provided lengths
buildNumberTokens :: [Int] -> [Token]
buildNumberTokens digitsList = concatMap buildNumberTokensLen digitsList

-- Returns all possible TokenOperators
buildOperatorTokens :: [Token]
buildOperatorTokens = map TokenOperator allOperators

-- Returns True if a number of len digits would be valid
isLengthValid :: Int -> Int -> Bool
isLengthValid len remaining
    | len > maxDigits = False
    | remaining - len == 1 = False
    | len > remaining = False
    | otherwise = True

-- Build a list of allowed digit count
buildValidNumLengths :: Int -> [Int]
buildValidNumLengths 0 = []
buildValidNumLengths rem = filter (\x -> isLengthValid x rem) [1..maxDigits]

-- Returns a list of all possible next tokens TODO
buildNextTokens :: Maybe Token -> Int -> [Token]
buildNextTokens Nothing len = buildNumberTokens (buildValidNumLengths len) 
    where margin = len - 2
buildNextTokens (Just (TokenValue val)) _ = buildOperatorTokens 
buildNextTokens (Just (TokenOperator op)) len = buildNumberTokens (buildValidNumLengths len) 

-- Builds all possible expressions of a given length, whether they are valid or not
buildExpressions :: Int -> Int -> TokenExpression -> [TokenExpression]
buildExpressions remaining maxLength acc
    | remaining < 0 = [acc]
buildExpressions 0 maxLength acc = [acc] 
buildExpressions remaining maxLength acc = do
    let values = buildNextTokens prev remaining  -- Need to reduce remaining on line below
    concatMap (\x -> buildExpressions (remaining - getLength x) maxLength (acc ++ [x])) values
    where
        prev = if (length acc /= 0) then Just (last acc) else Nothing 
        index = length acc

-- Build all possible equations, whether they are valid or not
buildEquations :: (Int, Int) -> [TokenEquation]
buildEquations (left, right) =
    [(lh, rh) | lh <- (buildExpressions left left []), rh <- (buildExpressions right right [])] 

-- Creates a list of tuples for the number of slots on the left and right side of equals
ofLegalSizes :: [(Int, Int)]
ofLegalSizes =
    foldl (\acc x -> concat [acc, [(wordLength - x, x - 1)]]) [] [2..4] -- hack half word length 

-- Build equations of all legal sizes, whether they are valid or not
buildAllEquations :: [TokenEquation]
buildAllEquations = concatMap buildEquations ofLegalSizes 

    
-- END EXPRESSION GENERATION --
    
main :: IO ()
main = do
    putStrLn "Mr Anderson, Welcome back."
    putStrLn "Building expressions list. Please Standbye..."
    outfile <- openFile "expressions.txt" WriteMode
    let validEquations = filter equalityCheck buildAllEquations
    let printableEquations = map stringEquation validEquations
    mapM_ (\x -> hPutStrLn outfile x) printableEquations 
    
