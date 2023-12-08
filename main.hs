import Numeric
import Data.Char
import System.Random
import System.Random.Shuffle
import System.IO
import System.Console.ANSI
import Control.Monad

-- Settings:
startingLifes, tableSize :: Int
startingLifes = 4
tableSize     = 32

-- Static data
targetWords :: [String]
targetWords =
  [ "mutate"
  , "dubbed"
  , "repair"
  , "rugrat"
  , "system"
  , "human"
  , "defend"
  , "stream"
  , "wagons"
  , "subtle"
  , "carved"
  , "banana"
  , "guitar"
  , "temple"
  , "frozen"
  , "candle"
  , "rocket"
  , "spring"
  , "sunset"
  , "rhythm"
  , "yellow"
  , "zephyr"
  , "jigsaw"
  , "quasar"
  , "fluffy"
  , "whale" 
  , "glitch"
  , "pickle"
  , "planet"
  , "rabbit"
  , "silent"
  , "thirst"
  , "violet"
  , "waffle"
  , "summer"
  , "breeze"
  , "flame" 
  , "rocket"
  , "dragon"
  , "bucket"
  , "palace"
  , "shadow"
  , "thorny"
  , "window"
  , "fierce"
  , "ocean" 
  , "whisky"
  , "forest"
  , "python"
  , "garden"
  , "humble"
  , "jungle"
  , "kettle"
  , "mellow"
  , "noodle"
  , "pickle"
  , "quartz"
  , "rattle"
  , "sizzle"
  , "tender"
  , "unique"
  , "velvet"
  , "wombat"
  , "yogurt"
  ]

garbage :: String
garbage = "$@#`|:=;-<>()[]{}%+*^/\\!?'\""

--
entryWidth :: Int
entryWidth = 12

--
data GameHand = GameHand
  { start            :: Int
  , entries          :: [String]
  , litteredEntries :: [String]
  , solution         :: String
  } deriving Show

likeness :: Eq a => [a] -> [a] -> Int
likeness [] _ = 0
likeness _ [] = 0
likeness (g:gx) (l:lx) = (if g == l then 1 else 0) + (likeness gx lx)

truncatedHex' :: String -> String
truncatedHex' s
  | len <  4 = truncatedHex' ("0" ++ s)
  | len == 4 = "0x" ++ s
  where len = length s
truncatedHex :: Int -> String
truncatedHex i
  | len <  4 = truncatedHex' $ "0" ++ h
  | len >  4 = truncatedHex  $ i' `mod` (16^4 - tableSize)
  | len == 4 = truncatedHex' $ h
  where
    { i'  = abs i
    ; h   = showHex i' ""
    ; len = length h
    }

garbageChar :: IO Char
garbageChar = do
  rindex <- randomIO :: IO Int
  return $ garbage !! (mod rindex (length garbage))

garbageEntry' :: Int -> [IO Char] -> [IO Char]
garbageEntry' i s
  | i > 0 = [garbageChar] ++ (garbageEntry' (i - 1) s)
  | otherwise = s

garbageEntry :: [IO Char]
garbageEntry = garbageEntry' entryWidth []

litterRight :: String -> IO String
litterRight e
  | (length e) < entryWidth = do
    { a <- garbageChar
    ; litterRight (e ++ [a])
    }
  | otherwise = return e

litterEntry' :: String -> Int -> IO String
litterEntry' e i
  | i > 0 = do
    { a <- garbageChar
    ; litterEntry' ([a] ++ e) (i - 1)
    }
  | otherwise = litterRight e

litterEntry :: String -> IO String
litterEntry e = do
  r <- randomIO :: IO Int
  let i = (r `mod` entryWidth - (length e))
  litterEntry' e i
 
litterEntries :: [String] -> IO [String]
litterEntries l
  | (length l) < tableSize = do
    { a <- sequence garbageEntry
    ; litterEntries (l ++ [a])
    }
  | otherwise = do
    { a <- sequence garbageEntry
    ; return l
    }

renderEntry :: Int -> String -> String
renderEntry i w =
  (truncatedHex i) ++ " " ++ w

renderOdd, renderEven, renderEntries :: Int -> [String] -> String
renderOdd i [] = ""
renderOdd i (l:ls) =
  (renderEntry i l) ++ " " ++ (renderEven (i + entryWidth) ls)
renderEven i [] = ""
renderEven i (l:ls) =
  (renderEntry i l) ++ "\n" ++ (renderOdd (i + entryWidth) ls)
renderEntries i l =
  renderOdd i l

printConsole :: GameHand -> IO ()
printConsole s = do
  putStr $ renderEntries (start s) (litteredEntries s)

printLifes, printLifes' :: Int -> IO ()
printLifes' i
  | i > 0 = do 
    { putStr " #"
    ; printLifes' (i - 1)
    }
  | otherwise = putStrLn ""
printLifes i = do
  putStr "Attempts remaining:"
  printLifes' i

gameCycle :: Int -> GameHand -> IO ()
gameCycle i s = do
  putStrLn "Password Required"
  putStrLn ""
  printLifes i
  putStrLn ""
  printConsole s
  putStr ">"
  hFlush stdout
  guess <- getLine
  putStrLn ""
  putStrLn $ ">" ++ (map toUpper guess)
  if guess == (solution s) then do
    putStrLn ""
    putStrLn "> Password Accepted."
  else do
    putStrLn ">Entry denied."
    let l = likeness guess (solution s)
    when (elem guess (entries s)) $ putStrLn $ ">Likeness=" ++ show l
    putStrLn ""
    putStrLn ""
    when (i > 1) $ gameCycle (i - 1) s

main :: IO ()
main = do
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Yellow]
  seed <- randomIO :: IO Int
  i    <- randomIO :: IO Int
  let ronnie   = mkStdGen seed
  let e        = take 12 (shuffle' targetWords (length targetWords) ronnie)
  e' <- litterEntries e
  e' <- mapM litterEntry e'
  let e''           = shuffle' e' (length e') ronnie
  let solutionIndex = i `mod` (length targetWords)
  gameCycle startingLifes (GameHand
    { entries          = e
    , litteredEntries  = e''
    , start            = seed
    , solution         = targetWords !! solutionIndex
    }
    )
