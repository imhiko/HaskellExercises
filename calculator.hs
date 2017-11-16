main :: IO ()
main = putStrLn "Use function calc :: String -> IO ()\nYou can use operations + - * / and brackets ( ) . Spaces are ignored."

data Expr = Value Double | Expression (Expr, Operation, Expr) | Null
                         | NegExpr Expr 
type Operation = Double -> Double -> Double

solve :: Expr -> Double
solve (Value x) = x
solve (Expression (a, op, b)) = op (solve a) (solve b)
solve (NegExpr ex) = - (solve ex)
solve (Null) = 0

removeSpaces :: [Char] -> [Char]
removeSpaces ([]) = []
removeSpaces (ch:str)
 |ch == ' ' = removeSpaces str
 |otherwise = ch : removeSpaces str

data Part = PartNum Double | PartBrackets [Part] | PartOperator Char | PartNull  | NegPartBrackets [Part]


findOperator :: String -> [Part]
findOperator [] = []
findOperator (ch:str)
  | ch == '(' = (PartOperator '*') : findPosExpr (ch:str)
  | isOperation ch = (PartOperator ch) : findExpr str
  | otherwise = [PartNull]
  
isOperation :: Char -> Bool
isOperation '+' = True
isOperation '-' = True
isOperation '*' = True
isOperation '/' = True
isOperation _ = False

findExpr :: String -> [Part]
findExpr [] = [PartNull]
findExpr str = if (head str) == '-' then negateFirst (findPosExpr (tail str))
                    else findPosExpr str
                    
findPosExpr :: String -> [Part]
findPosExpr [] = [PartNull]
findPosExpr (ch:str)
  | ch == '(' =   let (strInBr, othStr) = (findBrackets str 0)
                  in ((PartBrackets (findExpr strInBr) ) : findOperator othStr)
  | isDigit ch = let (strDoubl, othStr) = findDouble str
                 in (PartNum (read (ch:strDoubl) :: Double) ) : findOperator othStr
  | otherwise = [PartNull]
  


negateFirst :: [Part] -> [Part]
negateFirst [] = []
negateFirst (prt:oth) = (neg prt) : oth

neg :: Part -> Part
neg (PartNum x) = PartNum (- x)
neg (PartBrackets x) = NegPartBrackets x
neg (NegPartBrackets x) = PartBrackets x
neg _ = PartNull

  
findBrackets :: String -> Int -> (String, String)
findBrackets [] _ = ([], [])
findBrackets (ch:str) n = 
      case ch of
        ')' ->  if n == 0 then ([], str)
                else ((ch : brPart1), othPart1)
        '(' ->  ((ch : brPart2), othPart2)
        otherwise -> (ch : brPart3, othPart3)
      where
            (brPart1, othPart1) = findBrackets str (n-1)
            (brPart2, othPart2) = findBrackets str (n+1)
            (brPart3, othPart3) = findBrackets str n

findDouble :: String -> (String, String)
findDouble [] = ([],[])
findDouble (ch:str)
  | isDigit ch = (ch : strDoubl1, othStr1)
  | ch == '.' || ch == ',' = ('.' : strDoubl2, othStr2)
  | otherwise = ([], (ch:str))
  where 
        (strDoubl1, othStr1) = findDouble str
        (strDoubl2, othStr2) = findInt str
                  
findInt :: String -> (String, String)
findInt [] = ([],[])
findInt (ch:str)
  | isDigit ch = (ch : strDoubl, othStr)
  | otherwise = ([], (ch:str))
  where (strDoubl, othStr) = findDouble str
      

isDigit :: Char -> Bool
isDigit '1' = True
isDigit '2' = True
isDigit '3' = True
isDigit '4' = True
isDigit '5' = True
isDigit '6' = True
isDigit '7' = True
isDigit '8' = True
isDigit '9' = True
isDigit '0' = True
isDigit _ = False
  

buildValExpr :: Part -> Expr
buildValExpr (PartNum val) = Value val
buildValExpr (PartBrackets val) = buildExpr val
buildValExpr (NegPartBrackets val) = NegExpr (buildExpr val)
buildValExpr _ = Null

buildFullExpr :: ([Part], Part, [Part]) -> Expr
buildFullExpr (ex1, (PartOperator op), ex2) = 
                              Expression ((buildExpr ex1), (chToOperat op), (buildExpr ex2))
buildFullExpr _ = Null

chToOperat :: Char -> Operation
chToOperat '+' = (+)
chToOperat '-' = (-)
chToOperat '*' = (*)
chToOperat '/' = (/)

buildExpr :: [Part] -> Expr
buildExpr [] = Null
buildExpr prts =
  if length prts == 1 then buildValExpr (head prts)
    else if hasPlusOrMin prts then buildFullExpr (left1, last1, right1)
      else if hasMultOrDiv prts then buildFullExpr (left2, last2, right2)
        else Null
  where 
        (left1, last1, right1) = splitByLast prts isPlusOrMin
        (left2, last2, right2) = splitByLast prts isMultOrDiv

  
  
isPlusOrMin :: Part -> Bool
isPlusOrMin (PartOperator op) = op == '+' || op == '-'
isPlusOrMin _ = False

isMultOrDiv :: Part -> Bool
isMultOrDiv (PartOperator op) =  op == '*' || op == '/'
isMultOrDiv _ = False

hasPlusOrMin :: [Part] -> Bool
hasPlusOrMin [] = False
hasPlusOrMin (prt:prts) = case prt of
  PartOperator op -> op == '+' || op == '-' || hasPlusOrMin prts 
  otherwise -> hasPlusOrMin prts

hasMultOrDiv :: [Part] -> Bool
hasMultOrDiv [] = False
hasMultOrDiv (prt:prts) = case prt of
  PartOperator op -> op == '*' || op == '/' || hasMultOrDiv prts
  otherwise -> hasMultOrDiv prts


splitByFirst :: [Part] -> (Part -> Bool)  -> ([Part], Part, [Part])
splitByFirst [] _ = ([], PartNull, [])
splitByFirst (prt:oth) check
  | check prt = ([], prt, oth)
  | otherwise = ((prt : begining),spl, end)
  where (begining, spl, end) = splitByFirst oth check
  
splitByLast prts check = ((reverse c), b, (reverse a)) 
  where (a, b, c) = splitByFirst  (reverse prts) check
  
printExpr :: Expr -> IO ()
printExpr Null = putStrLn ("Не удалось решить.")
printExpr ex = putStrLn ("Ответ: " ++ (show (solve ex)))
  
calc = putStrLn. show. solve . buildExpr . findExpr . removeSpaces
calc2 = putStrLn. showExpr. buildExpr. findExpr. removeSpaces
calc3 = putStrLn. showParts. findExpr. removeSpaces

showExpr ex = case ex of
  (Value x) -> show $ x
  (Expression (a, b, c)) -> ("( " ++ (showExpr a) ++ " op " ++ (showExpr c) ++ " )")
  (NegExpr x) -> "-" ++ (showExpr x)
  otherwise -> "Null"
  
showParts :: [Part] -> String
showParts [] = ""
showParts (prt:oth) = case prt of
  (PartNum x) -> (show x) ++ ", " ++ (showParts oth)
  (PartOperator x) -> x : ", " ++ (showParts oth)
  (PartBrackets x) -> "( " ++ (showParts x) ++ " ), " ++ (showParts oth)
  (PartNull) -> "Null, " ++ (showParts oth)
  (NegPartBrackets x) -> "-( " ++ (showParts x) ++ " ), " ++ (showParts oth)

