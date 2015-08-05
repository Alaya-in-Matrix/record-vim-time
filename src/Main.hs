{-#language TypeSynonymInstances #-}
{-#language FlexibleInstances #-}
module Main where

import Control.Monad
import Control.Applicative hiding((<|>),many,optional)
import Text.ParserCombinators.Parsec
import System.Environment
import Data.Default

newtype Email   = Email String deriving(Eq, Show)
newtype LogPath = LogPath String deriving(Eq, Show)
instance Default Email where
    def = Email ""
instance Default LogPath where 
    def = LogPath "~/activity"
data Flags = Flags {
    email :: Email
  , logPath :: LogPath 
} deriving(Eq, Show)


cvt :: [a] -> [(a,a)]
cvt []  = []
cvt [x] = []
cvt (x:y:xs) = (x,y) : cvt xs

defMaybe :: (Default a) => (Maybe a) -> a
defMaybe Nothing  = def
defMaybe (Just v) = v

getFlags :: [(String,String)] -> Flags
getFlags args = let email = defMaybe $ lookup "-e" args
                    path  = defMaybe $ lookup "-d" args
                 in Flags (Email email) (LogPath path)
main = do
    args  <- (liftM cvt) getArgs
    flags <- return $ getFlags args
    print flags
