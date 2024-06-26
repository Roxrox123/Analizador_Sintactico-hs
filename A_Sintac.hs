import System.IO (readFile)
import Data.Char (isDigit, isSpace)

data Expression = NumberInt Int
                | NumberFloat Float
                | Operator Char
                | Variable String
                | SpecialSym Char
                | Comment String
                | Programa
                | Principal
                | ErrorToken String
                deriving (Show)

-- Function to analyze and parse numbers in a string
parseNumber :: String -> (Expression, String)
parseNumber s
    | '.' `elem` s = case reads s of
                        [(n :: Float, rest)] -> (NumberFloat n, rest)
                        _ -> (ErrorToken ("Número no válido: " ++ s), "")
    | 'E' `elem` s || 'e' `elem` s = case reads s of
                                        [(n :: Float, rest)] -> (NumberFloat n, rest)
                                        _ -> (ErrorToken ("Número no válido: " ++ s), "")
    | otherwise = case reads s of
                    [(n :: Int, rest)] -> (NumberInt n, rest)
                    _ -> (ErrorToken ("Número no válido: " ++ s), "")

-- Function to analyze and parse variables (identifiers)
parseVariable :: String -> (Expression, String)
parseVariable [] = (ErrorToken "Variable no válida", "")
parseVariable (x:xs)
    | x `elem` ['a'..'z'] || x `elem` ['A'..'Z'] || x `elem` ['0'..'9'] || x == '_' =
        let (var, rest) = span (\x -> x `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")) xs
        in (Variable (x:var), rest)
    | otherwise = (ErrorToken ("Variable no válida: " ++ [x]), xs)

-- Function to divide a line of text
tokenLine :: String -> [Expression]
tokenLine "" = []
tokenLine ('/':'/':rest) = [Comment rest]
tokenLine (x:xs)
    | isSpace x = tokenLine xs
    | isDigit x = case parseNumber (x:xs) of
                    (num, rest) -> num : tokenLine rest
    | x `elem` "=+-*/()^{};" = Operator x : tokenLine xs
    | x `elem` ['a'..'z'] || x `elem` ['A'..'Z'] = case parseVariable (x:xs) of
                                                        (var, rest) -> var : tokenLine rest
    | otherwise = ErrorToken ("El símbolo no existe: " ++ [x]) : tokenLine xs

-- Function to process a file and check for the correct structure
processFile :: String -> IO ()
processFile filename = do
    contents <- readFile filename
    let lines' = lines contents
    let tokens = concatMap tokenLine lines'
    case findError tokens of
        Just err -> putStrLn ("Syntax error: " ++ err)
        Nothing -> case validateProgramStructure tokens of
                      Right _ -> putStrLn "It is a program"
                      Left err -> putStrLn ("Structure error: " ++ err)

-- Function to find the first syntax error
findError :: [Expression] -> Maybe String
findError [] = Nothing
findError (ErrorToken err : _) = Just err
findError (_ : xs) = findError xs

-- Function to validate the program structure
validateProgramStructure :: [Expression] -> Either String ()
validateProgramStructure tokens =
    case tokens of
        (Variable "Programa" : Operator '{' : rest) -> validatePrincipalStructure rest 1
        _ -> Left "Expected 'Programa {' at the beginning."

validatePrincipalStructure :: [Expression] -> Int -> Either String ()
validatePrincipalStructure (Variable "principal" : Operator '{' : rest) depth =
    validateBraces rest (depth + 1)
validatePrincipalStructure _ _ = Left "Expected 'principal {' within 'Programa {'."

validateBraces :: [Expression] -> Int -> Either String ()
validateBraces [] 0 = Right ()
validateBraces (Operator '}' : rest) 1 = validateBraces rest 0
validateBraces (Operator '}' : rest) depth = validateBraces rest (depth - 1)
validateBraces (Operator '{' : rest) depth = validateBraces rest (depth + 1)
validateBraces (_ : rest) depth = validateBraces rest depth
validateBraces [] _ = Left "Mismatched braces."

-- Function to print the tokens of a line
printTokens :: String -> IO ()
printTokens line = do
    let tokens = tokenLine line
    mapM_ printTokenWithType tokens

-- Function to print a token with its type
printTokenWithType :: Expression -> IO ()
printTokenWithType (ErrorToken err) = putStrLn ("Error: " ++ err)
printTokenWithType token = print token

-- Main function
main :: IO ()
main = processFile "Texts.txt"







