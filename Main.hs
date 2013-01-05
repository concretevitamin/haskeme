{-# LANGUAGE ExistentialQuantification #-} 

{- |
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

import System.Environment
import System.IO

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans (liftIO)

import Data.List
import Data.IORef


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
-- (define (x) 10) -> List [Atom "define", List [Atom "x"], Number 10]

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

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal


-- Environment representations and operations ----------------------------------
-- | Environment (mappings from symbol names to their values) representation.
type Env = IORef [(String, IORef LispVal)]

initEnv :: IO Env
initEnv = newIORef [] >>= flip bindVars (map (\(x, y) -> (x, PrimitiveFunc y)) primitives)

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


-- Parsing raw input expressions -----------------------------------------------
readExpr :: String -> ThrowsError LispVal
readExpr inp =
    case parse parseExpr "lisp" inp of
        Left err -> throwError $ Parser err
        Right val -> return val

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> try parseList   -- use `try' to accomodate subsequent
                            -- dotted list parse
        <|> parseDottedList

parseString :: Parser LispVal
parseString = do char '"'
                 --st <- many (noneOf "\"" <|> char '"')
                 st <- many (noneOf "\"")
                 char '"'
                 return . String $ st

-- TODO: will never get #t and #f
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> specialSym
               rest <- many (letter <|> digit <|> specialSym)
               let atom = [first] ++ rest
               return $ case atom of
                            "#t" -> Bool True
                            "#f" -> Bool False
                            otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= return . Number . read

parseList :: Parser LispVal
parseList = do
    char '('
    vals <- parseExpr `sepBy` spaces
    char ')'
    return . List $ vals

parseDottedList :: Parser LispVal
parseDottedList = do
    char '('
    head <- parseExpr `endBy` spaces
    tail <- char '.' >> spaces >> parseExpr
    char ')'
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = char '\'' >> parseExpr >>=
    \x -> return $ List [Atom "quote", x]

specialSym :: Parser Char
specialSym = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space


-- Eval and Apply --------------------------------------------------------------
-- | Eval.
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
eval env (List (func:args)) = do                    -- combination
    f <- eval env func
    mapM (eval env) args >>= apply f
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

makeFunc vararg params body env = Func (map show params) vararg body env
makeNormalFunc = makeFunc Nothing
makeVarargFunc = makeFunc . Just . show

-- | Apply.
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc f) args = liftThrows $ f args
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


-- Crucial return types and their operations -----------------------------------
-- | A (ThrowsError V) value is either a LispError or a V value.
type ThrowsError = Either LispError

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

throwUnbErr :: String -> IOThrowsError a
throwUnbErr = throwError . UnboundVar "Unbounded variable"

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


-- REPL and Main ---------------------------------------------------------------
-- | The Read-Eval-Print-Loop.
-- TODO: can't use arrow keys and delete in REPL.
runRepl :: IO ()
runRepl = do
    env <- initEnv
    until_ (== exitCommand) (readPrompt replPrompt) (evalAndPrint env)

replPrompt = "Haskeme> "
exitCommand = "(exit)"

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env str =
    runIOThrows (liftM show $ liftThrows (readExpr str) >>= eval env)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env str = evalString env str >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    res <- prompt
    if pred res
        then return ()
        else action res >> until_ pred prompt action

main = do
    args <- getArgs
    env <- initEnv
    case length args of
        0 -> runRepl
        1 -> evalAndPrint env $ args !! 0
        otherwise -> putStrLn $ "Please suppy no argument (run the REPL) or"
                             ++ "only one expression."