module Main (main) where
import System.IO
import Data.Maybe (fromJust, isNothing)
import Control.Monad.State
import Data.List
import Data.List.Split
import Data.Text (unpack, pack, Text) 
import Text.Read

wordLength = 8
maxDigits = 3
allOperators = ['/', '*', '+', '-']
allLegal = concat [allOperators, ['='], map (head . show) [0..9]]

-- KnownData knownChars missingPosition excluded 
-- Used to store the information we know, previously validated from user input
data KnownData = KnownData [(Char, Int)] [(Char, Int)] [Char]

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

getLength :: Token -> Int
getLength (TokenOperator _) = 1
getLength (TokenValue val) = length (show val)

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
    | isNothing op = Nothing
    | otherwise = do
        operator <- op
        let index = snd operator
        if not (isOperatorIndexValid tokens index) then Nothing
        else do
            let res = performCalculation (tokens !! index) (tokens !! (index - 1)) (tokens !! (index + 1)) 
            if isNothing res then Nothing 
            else 
                evaluateTokens (concat [take (index - 1) tokens, [fromJust res], drop (index + 2) tokens]) 
    where
        token = head tokens
        op = findPriorityOperator 0 Nothing tokens

-- Compare two maybe ints for equality, got to be a default way of doing this
areIntsEqual :: Maybe Int -> Maybe Int -> Bool
areIntsEqual Nothing b = False
areIntsEqual a Nothing = False
areIntsEqual (Just a) (Just b) = a == b

-- Check if an equation is correct
equalityCheck :: TokenEquation -> Bool
equalityCheck equation = if isNothing left || isNothing right
    then False
    else areIntsEqual left right 
    where
        left = evaluateTokens (fst equation)
        right = evaluateTokens (snd equation)

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
    | remaining < 0 = [acc] -- Should never happen but I'm paranoid
buildExpressions 0 maxLength acc = [acc] 
buildExpressions remaining maxLength acc = do
    let values = buildNextTokens prev remaining 
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

-- Only needed to debug expression generation issues
debugWriteToFile :: IO ()
debugWriteToFile = do
    putStrLn "Building expressions list. Please Standbye..."
    outfile <- openFile "expressions.txt" WriteMode
    let validEquations = filter equalityCheck buildAllEquations
    let printableEquations = map stringEquation validEquations
    mapM_ (\x -> hPutStrLn outfile x) printableEquations 
    -- Super weird, in ghci it's fine
    -- But compiled, the file closes before writing is finished
    mapM_ (\x -> hPutStrLn outfile "ZZZZZZZZ") [1..1000] -- super hack

readInt :: String -> Maybe Int
readInt a = readMaybe a :: Maybe Int

-- START RETRIEVE AND VALIDATE USER INPUT --
validateCharsWithPos :: String -> Maybe [(Char, Int)]
validateCharsWithPos input = do
    let spaceSeparated = splitOn " " input
    let validSizeStrings = filter (\x -> length x == 2) spaceSeparated
    let tuples = map (\x -> (head x, last x)) validSizeStrings
    let positions = map (\z -> readInt [(snd z)] :: Maybe Int) tuples  -- dot notation again?
    if length (filter isNothing positions) > 0
        then Nothing
        else Just (zip (map fst tuples) (map fromJust positions))

validateChars:: String -> Maybe [Char]
validateChars input = do
    let illegalChars = filter (\x -> isNothing (elemIndex x allLegal)) input     
    if length illegalChars > 0
        then Nothing
        else Just input

isSubset :: String -> String -> Bool
isSubset a b = null [x | x <- a, elem x b == False]

allowedAtPos :: [(Char, Int)] -> String -> Bool
allowedAtPos a b = null [x | x <- a, (b !! ((snd x) - 1)) == (fst x)] 

hasNoExcludedChars :: String -> String -> Bool
hasNoExcludedChars a b = null [x | x <- a, elem x b == True]

getUserInputData :: IO (Maybe KnownData) 
getUserInputData = do
    putStrLn "Please enter the positions you've solved: <valPos>"
    putStrLn "EG: You know there is a * in position 2, enter *2 Multiple known values should be separated by spaces"
    knownChars <- getLine
    putStrLn "\nPlease enter all characters you know exist, and the position they are not in <valPos>" 
    missingPosition <- getLine
    putStrLn "\nPlease enter all characters you know don't exist EG: *90/41"
    excluded <- getLine
    putStrLn "\n"
    let vKnownChars = validateCharsWithPos knownChars
    let vMissingPositions = validateCharsWithPos missingPosition
    let vExclude = validateChars excluded

    if isNothing vKnownChars || isNothing vMissingPositions || isNothing vExclude
        then return Nothing
        else 
            return (Just (KnownData (fromJust vKnownChars) (fromJust vMissingPositions) (fromJust vExclude)))
    
-- END RETRIEVE AND VALIDATE USER INPUT --

filterFromKnown :: String -> ((Char, Int) -> Bool)
filterFromKnown s = (\(v, p) -> (s !! (p-1) == v))

-- Filter all equations using provided guess data
stringEquationsFromGuess :: Maybe KnownData -> IO (Maybe [String])
stringEquationsFromGuess Nothing = return Nothing
stringEquationsFromGuess (Just (KnownData known position exclude)) = do    
    let validEquations = filter equalityCheck buildAllEquations
    let sEquations = map stringEquation validEquations
    -- The first thing has to be a filter or we get no where....
    let one = filter (\e -> isNothing (elemIndex False (map (\k -> (filterFromKnown e) k) known))) sEquations
    -- Filter out chars at positions we know cannot exist, and equations missing known cars
    let two = filter (\e -> (isSubset (map fst position) e)) one 
    let three = filter (\e -> (allowedAtPos position e)) two

    -- Filter out equations with chars missing
    let four = filter (\e -> (hasNoExcludedChars exclude e)) three
    return (Just four)

main :: IO ()
main = do
    putStrLn "Mr Anderson, Welcome back.\n"
    -- debugWriteToFile
    guessData <- getUserInputData
    possibleEquations <- stringEquationsFromGuess guessData
    if isNothing possibleEquations 
        then putStrLn "No equations for you chief\n"
        else do
            let equations = fromJust possibleEquations 
            mapM_ putStrLn equations 
    putStrLn "Thank you\n"

