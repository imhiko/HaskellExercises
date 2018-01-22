import Data.Maybe
import Text.Read
main :: IO ()
main = do
  putStrLn "Добро пожаловать в ваш список дел!\nИспользуйте команду help, чтобы просмотреть список всех комманд."
  putStrLn ""
  askUser ["zoo", "bowling", "aquapark", "golf", "meeting", "dinner"]

askUser :: [String] -> IO ()
askUser toDoList = do
  putStrLn "Введите команду:"
  input <- getLine
  input2 <- getLine --костыль

  let (nwList, message) = completeCommand input toDoList in
      case message of
          "0"  -> putStrLn "Удачного дня!"
          "1" -> do
            putStr (listToStr nwList)
            askUser nwList
          otherwise  -> do
            putStr message
            askUser nwList


completeCommand :: String -> [String] -> ([String], String)
completeCommand str lst =
  let (command, args) = split str in
    case command of
      "add" -> ((args : lst), "1")
      "remove" ->   if isJust (readMaybe args :: Maybe Int)
                    then (removeRecord lst (read args), "1")
                    else if args == "all" then ([], "") else (lst, "")
                  
      "sort" -> (sortList lst, "1")
      "show" -> (lst, "1")
      "exit" -> (lst, "0")
      "help" -> (lst, "\nadd <строка> – добавляет запись в начало списка\nremove <i> – удаляет i–тую запись\nremove all – удаляет все записи\nshow – выводит все записи\nexit – завершение работы\nsort – сортирует список в алфавитном порядке\n")
      otherwise -> (lst, "Команду не удалось выполнить.\n")


split :: String -> (String, String)
split "" = ("", "")
split (' ' : str) = ("", str)
split (ch : str) = let (part1, part2) = split str in ((ch : part1), part2)

listToStr :: [String] -> String
listToStr lst = '\n' : (listToStrN lst 1) ++ "\n"
listToStrN :: [String] -> Integer -> String
listToStrN [] _ = ""
listToStrN (str: oth) n = (show n) ++ ".  " ++ str ++ "\n" ++ (listToStrN oth (n+1))

removeRecord :: [String] -> Integer -> [String]
removeRecord [] _ = []
removeRecord (str : oth) n =
  if n <= 1 then oth
  else str : (removeRecord oth (n - 1))

sortList :: [String] -> [String]
sortList [] = []
sortList (str : []) = [str]
sortList (str : oth) = let (minimal, oth2) = minimalAndOth str oth
  in minimal : (sortList oth2)


minimalAndOth  :: String -> [String] -> (String, [String])
minimalAndOth str [] = (str, [])
minimalAndOth str oth = let minimal = getMinimal str oth in (minimal, removeFirst minimal (str:oth))
  
getMinimal :: String -> [String] -> String
getMinimal str [] = str
getMinimal str (str2 : oth) = 
    let (smaller, bigger) = if str <= str2 then (str, str2) else (str2, str) in
    getMinimal smaller oth
    
removeFirst :: String -> [String] -> [String]
removeFirst _ [] = []
removeFirst str (str2 : oth) = if str == str2 then oth else (str2 : (removeFirst str oth))















