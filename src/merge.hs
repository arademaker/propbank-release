
module Merge where
  
import Data.List ( intercalate )
import System.FilePath.Posix ( takeBaseName, takeDirectory, takeExtension )
import System.IO ()
import System.Environment ( getArgs )

-- read the files into a list of strings (assuming they are an ordered list of pathnames)
-- convert the list of pathnames into a list of objs
-- merge the list of objs
-- produce the output

split :: Char -> String -> [String]
split c xs = case break (==c) xs of 
  (ls, "") -> [ls]
  (ls, x:rs) -> ls : split c rs

-- >>> split ',' "a,b"
-- ["a","b"]

data Obj = Obj { path :: String,
                 name :: String,
                 extension :: String } deriving (Show)

instance Eq Obj where
  x == y = name x == name y

instance Ord Obj where
  (<) x y     = name x < name y
  compare x y = compare (name x) (name y)
  (<=) x y    = name x <= name y

path2obj :: String -> Obj
path2obj s = Obj (takeDirectory s) (takeBaseName s) (takeExtension s)

join :: [Obj] -> [Obj] -> [(Maybe Obj,Maybe Obj)]
join [] [] = []
join [] (y:ys) = (Nothing, Just y) : join [] ys
join (x:xs) [] = (Just x, Nothing) : join xs []
join (x:xs) (y:ys) 
  | x == y = (Just x, Just y)  : join xs ys
  | x < y  = (Just x, Nothing) : join xs (y:ys)
  | x > y  = (Nothing, Just y) : join (x:xs) ys


print_pair :: [Char] -> (Maybe Obj, Maybe Obj) -> [Char]
print_pair sep (Just x,  Just y) = intercalate sep [name x, path x, path y]
print_pair sep (Nothing, Just x) = intercalate sep [name x, "-", path x]
print_pair sep (Just x, Nothing) = intercalate sep [name x, path x, "-"]
   
main :: IO [()]
main = do
  args <- getArgs
  s1 <- readFile $ args!!0
  s2 <- readFile $ args!!1
  let r1 = map path2obj (lines s1)
  let r2 = map path2obj (lines s2)
  mapM (\o -> putStr $ print_pair " " o ++ "\n") (join r1 r2)

