{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

--Imports
import System.IO (readFile) --to read file content
import Data.Char (isDigit, isSpace) --for character checks
import Data.List (span) --to split a list
import GHC.Generics (Generic) --typeclass from GHC.Generics module
import Data.Aeson (ToJSON, encode, object, (.=)) --For the tree json format
import Data.Aeson.Encode.Pretty (encodePretty) --To make the tree more stylized
import qualified Data.ByteString.Lazy.Char8 as BL --for handling lazy byte strings
import System.Directory (doesFileExist) --to check file existence

-- Define a data structure for the derivation tree nodes
data TreeNode = TreeNode {
    nodeLabel :: String, -- Label of the node in the tree
    nodeChildren :: [TreeNode] ---- Children nodes of the current node
} deriving (Show, Generic) -- Deriving Generic and Show instances for TreeNode

instance ToJSON TreeNode -- Making TreeNode an instance of ToJSON typeclass for JSON serialization

-- Define the Expression data type
data Expression = NumberInt Int -- Integer number expression
                | NumberFloat Float -- Floating-point number expression
                | Operator Char -- Operator expression
                | Variable String -- Variable expression
                | SpecialSym Char --special symbol 
                | Comment String --comment expression
                | Programa --Programa token
                | Principal --principal token
                | ErrorToken String --error token
                | Type String  -- Type token
                deriving (Show) -- Deriving Show instance for Expression.

-- Function to parse numbers in a string
parseNumber :: String -> (Expression, String)
parseNumber s
    -- Check for decimal point
    | '.' `elem` s = case (reads s :: [(Float, String)]) of
                        -- If parsed as Float, return NumberFloat and remaining string
                        [(n, rest)] -> (NumberFloat n, rest)
                        -- If parsing fails, return ErrorToken
                        _ -> (ErrorToken ("Invalid Number: " ++ s), "")
    -- Check for scientific notation
    | 'E' `elem` s || 'e' `elem` s = case (reads s :: [(Float, String)]) of
                                        -- If parsed as Float, return NumberFloat and remaining string
                                        [(n, rest)] -> (NumberFloat n, rest)
                                        -- If parsing fails, return ErrorToken
                                        _ -> (ErrorToken ("Invalid Number: " ++ s), "")
    -- Try parsing as Int
    | otherwise = case (reads s :: [(Int, String)]) of
                    -- If parsed as Int, return NumberInt and remaining string
                    [(n, rest)] -> (NumberInt n, rest)
                    -- If parsing fails, return ErrorToken
                    _ -> (ErrorToken ("Invalid Number: " ++ s), "")


-- Function to parse variables (identifiers)
parseVariable :: String -> (Expression, String)
parseVariable [] = (ErrorToken "Invalid Variable: ", "")
parseVariable (x:xs)
    -- Check if the first character is a valid variable identifier
    | x `elem` ['a'..'z'] || x `elem` ['A'..'Z'] || x == '_' =
        -- Extract the variable name and remaining string
        let (var, rest) = span (\x -> x `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")) xs
        in (Variable (x:var), rest)
    -- If the first character is not valid, return an ErrorToken
    | otherwise = (ErrorToken ("Invalid Variable: " ++ [x]), xs)

-- TokenLine function to return both the list of expressions and the derivation tree
tokenLine :: String -> ([Expression], TreeNode)
tokenLine "" = ([], TreeNode "" [])  -- Empty line does not contribute to the tree
tokenLine ('/':'/':rest) = ([Comment rest], TreeNode ("Comment: " ++ rest) [])
tokenLine (x:xs)
    -- Ignore whitespace characters
    | isSpace x = tokenLine xs
    -- Parse numbers
    | isDigit x = case parseNumber (x:xs) of
                    (num, rest) -> let (tokens, tree) = tokenLine rest
                                   in (num : tokens, TreeNode (show num) [tree])
    -- Parse operators
    | x `elem` (['=', '+', '-', '*', '/', '(', ')', '^', '{', '}'] :: [Char]) = 
        let (tokens, tree) = tokenLine xs
        in (Operator x : tokens, TreeNode ("Operator: " ++ [x]) [tree])
    -- Parse variables and types
    | x `elem` ['a'..'z'] || x `elem` ['A'..'Z'] = case parseVariable (x:xs) of
                                                        (Variable var, rest) -> let (tokens, tree) = tokenLine rest
                                                                                in (Variable var : tokens, TreeNode ("Variable: " ++ var) [tree])
                                                        (var, rest) -> let (tokens, tree) = tokenLine rest
                                                                       in (var : tokens, TreeNode ("Type: " ++ show var) [tree])
    -- Handle unknown symbols
    | otherwise = let (tokens, tree) = tokenLine xs
                  in (ErrorToken ("El símbolo no existe: " ++ [x]) : tokens, TreeNode ("Error: " ++ [x]) [tree])




-- Function to process a file and check for the correct structure
processFile :: String -> IO ()
processFile filename = do
    fileExists <- doesFileExist filename
    if fileExists
        then do
            contents <- readFile filename
            -- Split the contents into lines
            let lines' = lines contents
            -- Tokenize each line and construct the derivation tree
            let (tokens, trees) = unzip $ map tokenLine lines'
            -- Flatten the list of tokens
            let flatTokens = concat tokens
            -- Construct trees for each line
            let lineTrees = map (\(line, tree) -> TreeNode ("Line " ++ show line) [tree]) (zip [1..] trees)
            -- Construct the flat derivation tree for the entire program
            let flatTree = TreeNode "Programa" lineTrees
            -- Check for syntax errors
            case findError flatTokens of
                Just err -> putStrLn ("Syntax error: " ++ err)
                Nothing -> -- Validate the overall program structure
                            case validateProgramStructure flatTokens of
                                Right _ -> do
                                    putStrLn "It is a program"
                                    putStrLn "Derivation Tree:"
                                    -- Print the pretty-printed derivation tree
                                    BL.putStrLn $ encodePretty flatTree
                                Left err -> putStrLn ("Structure error: " ++ err)
        else putStrLn $ "File not found: " ++ filename


-- Function to find the first syntax error
findError :: [Expression] -> Maybe String
findError [] = Nothing
-- If an ErrorToken is found, return the error message
findError (ErrorToken err : _) = Just err
-- Recursively search for errors
findError (_ : xs) = findError xs

-- Function to validate the program structure
validateProgramStructure :: [Expression] -> Either String ()
validateProgramStructure tokens =
    case tokens of
        -- Check if the program starts with "Programa {"
        (Variable "Programa" : Operator '{' : rest) -> validatePrincipalStructure rest 1
        _ -> Left "Expected 'Programa {' at the beginning."

-- Function to validate the structure within the "Programa {" block
validatePrincipalStructure :: [Expression] -> Int -> Either String ()
validatePrincipalStructure (Variable "principal" : Operator '{' : rest) depth =
    validateBraces rest (depth + 1)
validatePrincipalStructure _ _ = Left "Expected 'principal {' within 'Programa {'."

-- Function to validate the balance of braces
validateBraces :: [Expression] -> Int -> Either String ()
validateBraces [] 0 = Right ()
-- If closing braces match opening braces, validation succeeds
validateBraces (Operator '}' : rest) 1 = validateBraces rest 0
-- Recursively validate braces
validateBraces (Operator '}' : rest) depth = validateBraces rest (depth - 1)
validateBraces (Operator '{' : rest) depth = validateBraces rest (depth + 1)
-- Ignore other tokens
validateBraces (_ : rest) depth = validateBraces rest depth
-- If there are unmatched braces, return an error
validateBraces [] _ = Left "Mismatched braces."


-- Main function
main :: IO ()
main = processFile "Text.txt" --pass the file that contains the program





