module Main where
-- Author: lvwenlong_lambda@qq.com
-- Last Modified:CST 2015-08-17 20:01:53 星期一
import Control.Monad
import Text.ParserCombinators.Parsec
import System.Environment
import System.Posix.Files
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.List
import Data.List.Split hiding(endBy)
import qualified Data.Map as Map

data VimLogTime = VimLogTime {
    vimLogYear   :: Integer
  , vimLogMonth  :: Int
  , vimLogDay    :: Int
  , vimLogHour   :: Int
  , vimLogMinute :: Int
  , vimLogSecond :: Int
} deriving(Eq, Show)
data VimLogAction    = Create | Open | Write deriving(Eq, Show)
type VimLogGitBranch = String
type VimFileType     = String
data VimLog = VimLog {
    time       :: VimLogTime
  , action     :: VimLogAction
  , editedfile :: FilePath
  , filetype   :: VimFileType
  , gitbranch  :: Maybe VimLogGitBranch
} deriving(Eq)
instance Show VimLog where 
    show (VimLog t a e ft g) = "VimLog {\n    " ++ show t ++ "\n    " ++ show a ++ "\n    " ++ e ++ "\n    " ++ ft ++ "\n    " ++ show g ++ "\n}"

secondOfDay :: VimLogTime -> Integer
secondOfDay t = let h = toInteger $ vimLogHour   t
                    m = toInteger $ vimLogMinute t
                    s = toInteger $ vimLogSecond t
                 in h * 3600 + m * 60 + s

dateToday :: IO String -- (year, month, day)
dateToday = liftM (map replace . showGregorian . utctDay) getCurrentTime
    where replace '-' = '/'
          replace  c   = c

logParser :: Parser VimLog
logParser = VimLog <$> (logTimeParser  <* delimiter)
                   <*> (actionParser   <* delimiter)
                   <*> (filePathParser <* delimiter)
                   <*> (filePathParser <* delimiter)
                   <*> branchParser
   where delimiter = char ';'

logTimeParser :: Parser VimLogTime
logTimeParser = VimLogTime <$> (read <$> many1   digit <* char '-')     -- year
                           <*> (read <$> count 2 digit <* char '-')     -- month
                           <*> (read <$> count 2 digit <* spaces)       -- day
                           <*> (read <$> count 2 digit <* char ':')     -- hour
                           <*> (read <$> count 2 digit <* char ':')     -- minute
                           <*> (read <$> count 2 digit)                 -- second

fileTypeParser :: Parser VimFileType 
fileTypeParser = many1 (noneOf ";") 

actionParser :: Parser VimLogAction
actionParser = (Create <$ string "create")
           <|> (Open   <$ string "open")
           <|> (Write  <$ string "write")

filePathParser :: Parser FilePath
filePathParser = try quotedPath <|> many1 (noneOf ";\n\r")

quotedPath :: Parser FilePath
quotedPath = char '"' *> many1 quotedChar <* char '"'

quotedChar :: Parser Char
quotedChar = try ('"' <$ string "\\\"") <|>  noneOf "\"" <?> "fuck"

branchParser :: Parser (Maybe VimLogGitBranch)
branchParser = (Just <$> filePathParser) <|> (Nothing <$ string "")

eol :: Parser String 
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\r"
  <|> string "\n"
  <?> "end of line"

logFileParser :: Parser [VimLog]
logFileParser = (logParser `endBy` eol) 


logMap :: [VimLog] -> Map.Map FilePath [(Integer, VimLogAction)]
logMap logs = logMap' (Map.empty) logs
    where logMap' m [] = m
          logMap' m (x:xs) = logMap' m' xs
              where m' = let t = (secondOfDay.time) x
                             a = action x
                             f = editedfile x
                             pair = (t,a)
                          in case (Map.lookup f m) of
                               Nothing  -> Map.insert f (pair:[]) m
                               Just rec -> Map.update (\_ -> Just (pair:rec)) f m

-- vim allow you to change the file type,
-- so it is possible that even with same file path
-- you get different file type
-- we use the last file type recorded
filetypeMap :: [VimLog] -> Map.Map FilePath VimFileType
filetypeMap logs = let names = map editedfile logs
                       types = map filetype logs
                    in Map.fromList $ zip names types


trans2 :: (a->a->b)->(c->a)->(c->c->b)
trans2 func cvt c1 c2 = func (cvt c1) (cvt c2)




duration :: Integer->[(Integer,VimLogAction)] -> Integer
duration maxInterval logs = let sorted   = sortBy (compare `trans2` fst) logs
                               splited   = map (map fst) $ split (keepDelimsL (whenElt ((/= Write) . snd))) sorted
                               durations = join $ map delta splited
                              in sum $ filter (<= maxInterval) durations
                                  where delta [] = []
                                        delta xs = zipWith (-) (tail xs) xs

main :: IO ()
main = do
    args   <- getArgs
    today  <- dateToday
    let path       = foldr (++) "" [head args, "/", today, ".log"]
        maxInterval = if (length args >= 2) then read (args !! 1) else (24 * 60 * 60)
    exist  <- fileExist path
    if not exist
       then putStrLn "No vim action today"
       else do parsed  <- parseFromFile logFileParser path 
               dateStr <- dateToday
               case parsed of
                  Left errMsg -> putStrLn $ show errMsg
                  Right val   -> let lm  = logMap $ reverse val
                                     ftm = filetypeMap val
                                     dm  = Map.map (duration maxInterval) lm
                                     vimTotalTime = timeToTimeOfDay $ secondsToDiffTime $ foldr (+) 0 $  map snd (Map.toList dm)
                                     filetypeTime = sortBy (flip  compare `trans2` snd) $ Map.toList $ Map.map (timeToTimeOfDay.secondsToDiffTime) $ getFileTypeTime ftm dm
                                  in do putStrLn $ "Time you spent on VIM today(" ++ dateStr ++ "): " ++ show vimTotalTime
                                        mapM_ (putStrLn.showLog) filetypeTime
                                        putStrLn "============================="

showLog :: (VimFileType,TimeOfDay) -> String
showLog (ft, t) = ft ++ ": " ++ (show t)

getFileTypeTime :: Map.Map FilePath VimFileType -> Map.Map FilePath Integer -> Map.Map VimFileType Integer
getFileTypeTime typeLog fileLog = let fileLogList = Map.toList fileLog
                                      typeLogList = map convertType fileLogList
                                   in Map.fromListWith (+) typeLogList
                                       where convertType :: (FilePath, Integer) -> (VimFileType, Integer)
                                             convertType (name,t) =  case (Map.lookup name typeLog) of
                                                                          Nothing -> error $ "fail to find file type for " ++ name
                                                                          Just v  -> (v,t)
