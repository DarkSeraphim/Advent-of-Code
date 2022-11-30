module Day24.Part1 (solve) where
    import Text.Printf (printf)
    import Text.ParserCombinators.Parsec (GenParser, endBy1, choice, string, try, char, oneOf)
    import Text.Parsec (endOfLine)
    import Helpers.Parsec (number, parseInput)
    import Data.Map (Map, findWithDefault, insert, empty)
    import Data.List (intercalate)

    data Operator = Inp | Add | Mul | Mod | Div | Eql
    data Expr = Variable Char | Value Int
    data Operands = Unary Expr | Binary Expr Expr
    type Stmt = (Operator, Operands)
    type Input = [Int]
    type State = Map Char Int

    parseCode :: GenParser Char st [Stmt]
    parseCode = parseStmt `endBy1` endOfLine

    parseStmt :: GenParser Char st Stmt
    parseStmt = do
        operator <- choice parseOp <* char ' '
        left <- parseExpr
        operands <- case operator of
                          Inp -> pure $ Unary left
                          _ -> Binary left <$> (char ' ' *> parseExpr)
        return (operator, operands)

    parseOp :: [GenParser Char st Operator]
    parseOp = map (fmap parseOp' . try) [string "inp", string "add", string "mul", string "mod", string "div", string "eql"]

    parseOp' :: String -> Operator
    parseOp' "inp" = Inp
    parseOp' "add" = Add
    parseOp' "mul" = Mul
    parseOp' "mod" = Mod
    parseOp' "div" = Div
    parseOp' "eql" = Eql
    parseOp' _ = error "Invalid operator"

    parseExpr :: GenParser Char st Expr
    parseExpr = choice [Value <$> number, Variable <$> oneOf "wxyz"]

    value :: Expr -> State -> Int
    value (Value v) _ = v
    value (Variable v) state = findWithDefault 0 v state

    name :: Expr -> Char
    name (Variable v) = v
    name _ = error "Not a variable"

    applyOp :: Operator -> Operands -> Input -> State -> (Input, State)
    applyOp Inp (Unary expr) input state = (tail input, insert (name expr) (head input) state)
    applyOp Add (Binary exprL exprR) input state = (input, insert (name exprL) (value exprL state + value exprR state) state)
    applyOp Mul (Binary exprL exprR) input state = (input, insert (name exprL) (value exprL state * value exprR state) state)
    applyOp Mod (Binary exprL exprR) input state = (input, insert (name exprL) (value exprL state `mod` value exprR state) state)
    applyOp Div (Binary exprL exprR) input state = (input, insert (name exprL) (value exprL state `div` value exprR state) state)
    applyOp Eql (Binary exprL exprR) input state = (input, insert (name exprL) (fromEnum $ value exprL state == value exprR state) state)
    applyOp _ _ _ _ = error "Invalid statement"

    compute' :: Map Char Int -> [Stmt] -> [Int] -> Int
    compute' state [] inputs = findWithDefault 0 'z' state
    compute' state ((op, opands):rest) inputs = compute' state' rest inputs'
        where (inputs', state') = applyOp op opands inputs state

    compute :: [Stmt] -> [Int] -> Int
    compute = compute' empty

    solve :: IO ()
    solve = do
        code <- parseInput parseCode
        let res = maximum $ filter ((==0) . compute code) [[0]]
        printf "Error code was %s" (intercalate "" (map show res))
