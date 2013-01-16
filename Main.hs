{-# LANGUAGE ExistentialQuantification #-} 

{-
Module      : <Haskeme>
Copyright   :
License     :

Maintainer  : Zongheng Yang <zongheng.y@gmail.com>
Stability   : experimental
Portability : portable

A simple Scheme interpreter with a REPL.

It is based on the tutorial `Write Yourself a Scheme in 48 Hours'
(http://jonathan.tang.name/files/scheme_in_48/tutorial/overview.html) written
by Jonathan Tang.
-}

import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T

import System.Environment
import System.IO

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans (liftIO)

import Data.List
import Data.IORef

import System.Console.Haskeline

-- Internal representations for values -----------------------------------------
-- | Internal representations of all types of legal values.
data LispVal = Atom String
             | Number Integer
             | String String
             | Bool Bool
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params    :: [String]
                    , vararg    :: (Maybe String)
                    , body      :: [LispVal]
                    , closure   :: Env
                    }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

-- | Show for LispVal.
showVal :: LispVal -> String
showVal (String x) = "\"" ++ x ++ "\""
showVal (Atom x) = x
showVal (Number x) = show x
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List xs) = "(" ++ unwordsList xs ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func params vararg _ _) =
    "(lambda (" ++ unwords params ++ vararg' ++ ") ...)"
    where vararg' = maybe "" id vararg
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

-- Crucial return types and their operations -----------------------------------
-- | A (ThrowsError V) value is either a LispError or a V value.
type ThrowsError = Either LispError

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

-- | `G' for `generic'.
runIOThrowsG :: IOThrowsError a -> IO a
runIOThrowsG action = runErrorT action >>= return . extractValue

throwUnbErr :: String -> IOThrowsError a
throwUnbErr = throwError . UnboundVar "Unbounded variable"

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- | Environment (mappings from symbol names to their values) representation.
type Env = IORef [(String, IORef LispVal)]
-- Environment representations and operations ----------------------------------

-- TODO: 1. existence of the file  2. other paths
stdLibPath :: String
stdLibPath = "stdlib.scm"

loadIO :: String -> IO [LispVal]
loadIO filename = do
    exps <- readFile filename
    case readExprList exps of
        Left err -> return []
        Right vals -> return vals

initEnv :: IO Env
initEnv = newIORef [] >>=
          flip bindVars (map (\(x, y) -> (x, PrimitiveFunc y)) primitives) >>=
          flip bindVars (map (\(x, y) -> (x, IOFunc y)) ioPrimitives) >>=
          \env -> do
            loadIO stdLibPath >>= runIOThrowsG . evalSequence env
            return env

isBound :: Env -> String -> IO Bool
isBound env symb = readIORef env >>= return . maybe False (const True) . lookup symb

getVar :: Env -> String -> IOThrowsError LispVal
getVar env symb = do
    env' <- liftIO $ readIORef env
    maybe (throwUnbErr symb)
          (liftIO . readIORef)
          (lookup symb env')

-- | Attempts to set a value to a variable. If the variable has not been
-- defined yet, an error occurs.
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar env symb val = do
    env' <- liftIO $ readIORef env
    maybe (throwUnbErr symb)
          (liftIO . flip writeIORef val)
          (lookup symb env')
    return $ Atom symb

-- | Define a variable. If some variable with the same name has already been
-- defined, its value is overriden.
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar env symb val = do
    isDefined <- liftIO $ isBound env symb
    if isDefined
        then setVar env symb val >> return val
        else liftIO $ do
            val' <- newIORef val
            modifyIORef' env ((symb, val'):)
            return $ Atom symb

-- | Bind (extend) variables bindings to an environment.
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars env bindings = readIORef env >>= extendEnv bindings >>= newIORef
    where extendEnv bindings' env' = liftM (++ env') $ mapM toIORef bindings'
          toIORef (symb, val) = do
            ref <- newIORef val
            return (symb, ref)

-- Error handling --------------------------------------------------------------
-- | Errors that can occur during the interpretation.
data LispError = NumArgs Int [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

showError :: LispError -> String
showError (UnboundVar msg var) = msg ++ ": " ++ var
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg func) = msg ++ ": " ++ show func
showError (NumArgs expected found) =
    "Expected " ++ show expected ++ " args, found values: " ++ unwordsList found
showError (TypeMismatch expected found) =
    "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
instance Show LispError where show = showError

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

-- Primitives and primitives-related functions ---------------------------------
-- | Primitives.
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)

             , ("=", numBoolBinop (==))
             , ("<", numBoolBinop (<))
             , (">", numBoolBinop (>))
             --, ("/=", numBoolBinop (/=))
             , (">=", numBoolBinop (>=))
             , ("<=", numBoolBinop (<=))

             , ("and", boolBoolBinop (&&))
             , ("or", boolBoolBinop (||))

             , ("string=?", strBoolBinop (==))
             , ("string?", strBoolBinop (>))
             , ("string<=?", strBoolBinop (<=))
             , ("string>=?", strBoolBinop (>=))

             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)

             -- TODO: not working!
             --, ("sqrt", numericUnary (floor . realToFrac . sqrt))

             , ("boolean?", isBoolean)
             , ("symbol?", isSymbol)
             , ("string?", isString)
             , ("number?", isNumber)
             , ("null?", return . Bool . null)
             
             , ("car", car)
             , ("cdr", cdr)
             , ("cons", cons)

             , ("eqv?", eqv)
             , ("equal?", equal)
             ]
-- list?, pair?

-- TODO: Adds support for other number types.
numericUnary f [Number int] = return $ Number (f int)

numericBinop :: (Integer -> Integer -> Integer) ->
                ([LispVal] -> ThrowsError LispVal)
numericBinop op s@[_] = throwError $ NumArgs 2 s
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op


boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool)
          -> ([LispVal] -> ThrowsError LispVal)
boolBinop unpacker op s@[_] = throwError $ NumArgs 2 s
boolBinop unpacker op params@[x, y] = do
    [x, y] <- mapM unpacker params
    return . Bool $ op x y


numBoolBinop = boolBinop unpackNum 
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

--makeFunc params vararg body closure = Func

-- Unpackers --
data Unpacker = forall a. Eq a => Unpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals x y (Unpacker f) =
    do x' <- f x
       y' <- f y
       return $ x' == y'
    `catchError` (const $ return False)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
--unpackNum (String n) = let parsed = reads n
--                       in if null parsed
--                            then throwError $ TypeMismatch "number" (String n)
--                            else return $ fst $ parsed !! 0
--unpackNum (List [n]) = throwError $ TypeMismatch "number" (List [n])
unpackNum notNum = throwError $ TypeMismatch "number" notNum


unpackStr :: LispVal -> ThrowsError String
--unpackStr (Number n) = throwError $ TypeMismatch "string" (Number n)
unpackStr (String n) = return n
--unpackStr (List [n]) = throwError $ TypeMismatch "string" (List [n])
unpackStr notStr = throwError $ TypeMismatch "string" notStr


unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool n) = return n
unpackBool notBool = throwError $ TypeMismatch "bool" notBool


isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean [Bool _] = return $ Bool True
isBoolean x@_ = throwError $ NumArgs 1 x

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [Number _] = return $ Bool True
isNumber x@_ = throwError $ NumArgs 1 x

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString x@_ = throwError $ NumArgs 1 x

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom _] = return $ Bool True
isSymbol x@_ = throwError $ NumArgs 1 x

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:_) _] = return x
car [bad] = throwError $ TypeMismatch "pair" bad
car badList = throwError $ NumArgs 1 badList    -- a list, more than 1 element

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [bad] = throwError $ TypeMismatch "pair" bad
cdr badList = throwError $ NumArgs 1 badList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x:xs)
cons [List xs, DottedList h t] = return $ DottedList (xs ++ h) t
cons [x, DottedList h t] = return $ DottedList (x:h) t
cons [x, y] = return $ DottedList [x] y
cons badList = throwError $ NumArgs 2 badList

-- TODO: (eqv? '(1) '(1)) in scheme gives #f; this version gives #t
eqv :: [LispVal] -> ThrowsError LispVal
eqv [Atom s1, Atom s2] = return $ Bool (s1 == s2)
eqv [Number x1, Number x2] = return $ Bool (x1 == x2)
eqv [String s1, String s2] = return $ Bool (s1 == s2)
eqv [Bool b1, Bool b2] = return $ Bool (b1 == b2)
eqv [DottedList h1 t1, DottedList h2 t2] =
    eqv $ [List $ h1 ++ [t1], List $ h2 ++ [t2]]
eqv [List xs1, List xs2] =
    if length xs1 /= length xs2 
        then return $ Bool False
        else do
            bools <- mapM eqv (transpose [xs1, xs2])
            return . Bool . and $ map unpackBool' bools
            where unpackBool' (Bool True) = True
                  unpackBool' _ = False
eqv [_, _] = return $ Bool False
eqv bad = throwError $ NumArgs 2 bad

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) unpackers
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
    where unpackers = [ Unpacker unpackBool
                      , Unpacker unpackStr
                      , Unpacker unpackNum
                      ]
equal badList = throwError $ NumArgs 2 badList

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [ ("apply", applyProc)
               , ("open-input-file", makePort ReadMode)
               , ("open-output-file", makePort WriteMode)
               , ("close-input-port", closePort)
               , ("close-output-port", closePort)
               , ("read", readProc)
               , ("write", writeProc)
               , ("read-contents", readContents)
               , ("read-all", readAll)
               ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine stdin) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO (hPrint port obj) >> return (Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [] = return $ Bool False
readAll [String filename] = liftM List $ load filename
readAll files =
    liftM (List . map String) $ liftIO $ sequence $ map (readFile . show) files
 

-- Parsing ---------------------------------------------------------------------
-- | TODO:
--      1. Add support for block comments.
--      2. Add other / fix fields
schemeDef :: LanguageDef st
schemeDef =
    emptyDef { T.commentLine        = ";"
             , T.nestedComments     = True
             , T.caseSensitive      = False

             , T.identStart         = letter <|> specialSym
             , T.identLetter        = alphaNum <|> specialSym

             , T.reservedNames = ["[", "]", "{", "}", "|", "#", "\\"] -- TODO: ?
             , T.reservedOpNames = ["[", "]", "{", "}", "|"]
             }

schemeLexer :: T.TokenParser st
schemeLexer = T.makeTokenParser schemeDef

specialSym = oneOf "#!$%&|*+-/:<=>?@^_~"

--spaces = skipMany1 space

-- Lexeme parsers -------------------------------------------------------------
whiteSpace = T.whiteSpace schemeLexer
integer = T.integer schemeLexer
identifier = T.identifier schemeLexer
parens = T.parens schemeLexer

-- Main parsers ----------------------------------------------------------------
-- | The 'main parser'.
parseScheme :: Parser LispVal
parseScheme = whiteSpace >> parseExpr

-- | Parse an expression.
parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> try parseList   -- use `try' to accomodate subsequent dotted list parse
        <|> parseDottedList

-- | Parse an atom.
parseAtom :: Parser LispVal
parseAtom = do
    val <- identifier
    return $ case val of 
        "#t" -> Bool True
        "#f" -> Bool False
        otherwise -> Atom val


parseString :: Parser LispVal
parseString = do char '"'
                 st <- many (noneOf "\"")
                 char '"'
                 whiteSpace
                 return . String $ st

-- TODO: Support negatives.
parseNumber :: Parser LispVal
parseNumber = integer >>= (return . Number)

parseQuoted :: Parser LispVal
parseQuoted =
    char '\'' >> parseExpr >>= \x -> return $ List [Atom "quote", x]

-- | TODO: comment?
parseList :: Parser LispVal
parseList = parens $ many parseExpr >>= return . List

parseDottedList :: Parser LispVal
parseDottedList = do
    char '('
    f <- manyTill parseExpr (char '.')
    s <- whiteSpace >> parseExpr -- TODO: Allow comments here?
    char ')' -- TODO: changed to reserved?
    whiteSpace
    return $ DottedList f s


-- Misc. wrapper functions -----------------------------------------------------------
--readOrThrow :: Parser LispVal -> String -> ThrowsError LispVal
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser inp =
    case parse parser "lisp" inp of
        Left err -> throwError $ Parser err
        Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseScheme

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow $ many parseScheme


-- Eval and Apply --------------------------------------------------------------
-- | Eval.
-- TODO: 
-- * Don't just simply throw away all unparsed stuff. E.G.:
--      'c 1 23 => c
-- * let
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val                -- strings
eval env val@(Number _) = return val                -- numbers (integers)
eval env val@(Bool _) = return val                  -- bools
eval env (Atom var) = getVar env var                -- variables
eval env (List [Atom "quote", val]) = return val    -- quoted
eval env (List [Atom "if", pred, consq, alt]) = do  -- if
    res <- eval env pred
    case res of
        Bool True -> eval env consq
        _ -> eval env alt
eval env (List [Atom "set!", Atom var, exp]) =      -- assignment
    eval env exp >>= setVar env var
-- definition of variables
eval env (List [Atom "define", Atom var, exp]) =
    eval env exp >>= defineVar env var
-- definition of functions
eval env (List (Atom "define" : List (name:params) : exps)) = 
    defineVar env (show name) (makeNormalFunc params exps env)
-- definition of functions with vararg
eval env (List (Atom "define" : DottedList (name:params) vararg : exps)) =
    defineVar env (show name) (makeVarargFunc vararg params exps env)
-- a lambda that takes any number of arguments
eval env (List (Atom "lambda" : Atom vararg : exps)) =
    return $ makeVarargFunc (Atom vararg) [] exps env
-- a normal lambda
eval env (List (Atom "lambda" : List params : exps)) =
    return $ makeNormalFunc params exps env
-- a lambda with vararg
eval env (List (Atom "lambda" : DottedList params vararg : exps)) =
    return $ makeVarargFunc vararg params exps env
eval env (List [Atom "load", String filename]) =    -- loading a file
    load filename >>= evalSequence env
eval env (List (func:args)) = do                    -- combination
    f <- eval env func
    mapM (eval env) args >>= apply f
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- | Evaluate a sequence of expressions, returns the last evaluated value.
evalSequence :: Env -> [LispVal] -> IOThrowsError LispVal
evalSequence env exps = liftM last $ mapM (eval env) exps

makeFunc vararg params body env = Func (map show params) vararg body env
makeNormalFunc = makeFunc Nothing
makeVarargFunc = makeFunc . Just . show

-- | Apply.
-- TODO: Support for IOFunc.
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc f) args = liftThrows $ f args
apply (IOFunc f) args = f args
apply (Func params vararg body closure) args =
    if (length params /= length args) && (vararg == Nothing)
        then throwError $ NumArgs (length params) args
        else (liftIO $ bindVars closure (zip params args)) >>=
             bindVarArg vararg >>= 
             evalBody body
    where bindVarArg Nothing env = return env
          bindVarArg (Just vararg) env = liftIO $ do
                valRef <- newIORef $ List $ drop (length params) args
                modifyIORef' env ((vararg, valRef):)
                return env 
          evalBody exprs env = liftM last $ sequence $ map (eval env) exprs
apply badFunc badArgs =
    throwError $ NotFunction "Unrecognized function" (show badFunc)


-- REPL, Main, and IO-related Stuff --------------------------------------------
-- | The Read-Eval-Print-Loop.
runRepl :: IO ()
runRepl = do
    env <- initEnv
    runInputT defaultSettings (loop env)
    where
        replPrompt = "Haskeme> "
        loop :: Env -> InputT IO ()
        loop env = do
            inp <- getInputLine replPrompt
            case inp of
                Nothing -> return ()
                Just str -> liftIO (evalAndPrint env str) >> loop env

evalString :: Env -> String -> IO String
evalString env str =
    runIOThrows (liftM show $ liftThrows (readExpr str) >>= eval env)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env str = evalString env str >>= putStrLn

main :: IO ()
main = do
    args <- getArgs
    env <- initEnv
    case length args of
        0 -> runRepl
        1 -> evalAndPrint env $ args !! 0
        otherwise -> putStrLn $ "Please suppy no argument (run the REPL) or"
                             ++ "only one expression."
