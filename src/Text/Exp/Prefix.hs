-- | Convert infix exp to prefix exp. The expression's operators to see 'expOperators'
module Text.Exp.Prefix (
    fromInfix
  ) where

import Data.List (intercalate, elemIndex, drop)
import Data.Char (isSpace)
import Text.Regex.PCRE ((=~))

import Debug.Trace (trace)

-- | a operators list which cantains a list in order with precedence.
expOperators :: [[(String, Int)]]
expOperators   = [[("^", 2)], [("*", 2), ("/", 2), ("\\", 2), ("%", 2)], [("+", 2), ("-", 2)], [("=~", 2)], [("&&", 2)], [("||", 2)], [("!", 1)], [("<", 2), ("<=", 2), (">", 2), (">=", 2), ("==", 2), ("/=", 2)], [("(", 0), (")", 0)]]
expOperators'  = foldr (++) [] expOperators
expOperators'' = unzip expOperators'

-- | parse a infix expression to prefix expression in a stack.
fromInfix :: String -> Either String String
fromInfix infixExp =
    if checkInfixExp infixExp'
       then parseExpression' [] [] $ reverse $ splitExp infixExp'
       else Left $ "Error: operators' arguments aren't matched. see :" ++ infixExp'
  where
    infixExp' = doPrefixNegative infixExp
    parseExpression' :: [(String, Int)] -> [String] -> [String] -> Either String String
    parseExpression' opStack stack exps
      | [] <- exps
      , [] <- opStack
      = if checkOpArgs stack
           then Right $ intercalate " " stack
           else Left $ "Error: operators' arguments aren't matched. see infix:" ++ infixExp ++ ". see prefix: " ++ intercalate " " stack
      | [] <- exps
      = let (_op, _) = unzip opStack
        in parseExpression' [] (reverse _op ++ stack) []
      | (op : exp) <- exps
      = let pr = getPrecedence op in
        if | not $ op `elem` fst expOperators'' ->
               parseExpression' opStack (op : stack) exp
           | op == ")" -> parseExpression' ((op, pr) : opStack) stack exp
           | op == "(" ->
               let (opStack'', popedStack) = popBrackets opStack
               in if opStack'' == [] && head popedStack == "Error"
                     then Left $ "Error: " ++ last popedStack ++ " see: " ++ infixExp
                     else parseExpression' opStack'' (popedStack ++ stack) exp
           | [] == opStack && op `elem` fst expOperators'' ->
               parseExpression' ((op, pr) : opStack) stack exp
           | otherwise ->
               let ((_op, _pr) : opStack') = opStack in
               if | pr <= _pr -> parseExpression' ((op, pr) : opStack) stack exp
                  | otherwise -> parseExpression' ((op, pr) : opStack') (_op : stack) exp
    popBrackets = popBrackets' []
      where
        popBrackets' popedStack opStack
            | [] <- opStack
            = ([], ["Error", "mismatch (."])
            | ((op, _) : opStack'') <- opStack
            , op /= ")"
            = popBrackets' (op : popedStack) opStack''
            | ((op, _) : opStack'') <- opStack
            , op == ")"
            = (opStack'', popedStack)

checkInfixExp :: String -> Bool
checkInfixExp exp = not $ exp =~ "((&&|\\|\\||[><=]=|\\^|=~|\\+|-|\\*|/|%|\\\\) *(&&|\\|\\||[><=]=|\\^|=~|\\+|-|\\*|/|%|\\\\))|([0-9A-Za-z]+ +[0-9A-Za-z]+)"

checkOpArgs :: [String] -> Bool
checkOpArgs stack = checkOpArgs' stack "" 0 0 0 == length stack
  where
    checkOpArgs' :: [String] -- ^ prefix stack, without current op
                 -> String   -- ^ current op
                 -> Int      -- ^ current count
                 -> Int      -- ^ needed count
                 -> Int      -- ^ length of current exp
                 -> Int      -- ^ current exp length in stack
    checkOpArgs' stack' op count _count len
        | "" <- op
        , top : stack'' <- stack'
        = let _c = getOpArgumentCount top
          in checkOpArgs' stack'' top 0 _c (len + 1)
        | top : stack'' <- stack'
        , count < _count
        = let count' = getOpArgumentCount top
          in if count' == 0
                then checkOpArgs' stack'' op (count + 1) _count (len + 1)
                else let subLen = checkOpArgs' stack'' top 0 count' 1
                         stack''' = drop (subLen - 1) stack''
                     in checkOpArgs' stack''' op (count + 1) _count (len + subLen)
        | count == _count
        = len
        | [] <- stack'
        , count /= _count
        = 0
        | otherwise
        = len

getOpArgumentCount :: String -> Int
getOpArgumentCount op
    | Just idx <- op `elemIndex` fst expOperators''
    = snd expOperators'' !! idx
    | otherwise
    = 0

getPrecedence ::  String -> Int
getPrecedence = getPrecedence' expOperators 0
  where
    getPrecedence' (pr : prs) idx op =
        if op `elem` (fst $ unzip pr)
           then idx
           else getPrecedence' prs (idx + 1) op

splitExp :: String -> [String]
splitExp = splitExp' []
  where
    splitExp' sp exp
      | "" <- exp = filter (\x -> x /= "") sp
      | otherwise
      = let match = exp =~ " +|'.*'|\".*\"|\\( *[\\+-][0-9A-Za-z]+ *\\)|&&|\\^|\\|\\||=~|!|[><=]=|[><]|\\(|\\)|\\+|-|\\*|/|%|\\\\" :: String
        in if match /= ""
           then let Just (idx, _) = elemSubIndex match exp
                    (arg, exp')   = splitAt idx exp
                    (op, exp'')   = splitAt (length match) exp'
                    op' = if op =~ "\\( *[\\+-][0-9A-Za-z]+ *\\)" :: Bool
                             then filter (not.(`elem`"()")) op
                             else op
                 in splitExp' (sp ++ [filter (not.isSpace) arg, filter (not.isSpace) op']) exp''
           else splitExp' (sp ++ [exp]) ""

doPrefixNegative :: String -> String
doPrefixNegative exp =
    let match = exp =~ "^ *[\\+-][0-9A-Za-z]+" :: String
    in if match /= ""
        then let Just (t, d) = elemSubIndex match exp
            in take t exp ++ "(" ++ match ++ ")" ++ drop d exp
        else exp

-- | get a sub string index
elemSubIndex :: String -- ^ sub string
             -> String -- ^ origin String
             -> Maybe (Int, Int) -- ^ (prefix index, postfix index)
elemSubIndex _sub _str= elemSubIndex' _sub _str
  where
    elemSubIndex' sub str
      | ""  <- str
      , _:_ <- sub
      = Nothing
      | "" <- sub
      = let postfix = length _str - length str
            prefix  = postfix - length _sub
        in Just (prefix, postfix)
      | (c : str') <- str
      , (c' : sub') <- sub
      = if c == c'
           then elemSubIndex' sub' str'
           else elemSubIndex' _sub $ drop (length _str - length str - length _sub + length sub + 1) _str
