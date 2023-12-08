import System.IO
import Data.Char
import System.Console.ANSI
import System.Random
import Control.Monad.IO.Class
import System.Random.Shuffle

target_words =
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
  ]

garbage = "$@#`|:=;_-<>()[]{}%+*^/\\!?'\""

solution_index = 3
table_size = 32
entry_width = 12
starting_lifes = 4


likeness :: Eq a => [a] -> [a] -> Int
likeness [] _ = 0
likeness _ [] = 0
likeness (g:gx) (l:lx) = (if g == l then 1 else 0) + (likeness gx lx)

fake_hex' :: String -> String
fake_hex' s
  | len < 4 = fake_hex' ("0" ++ s)
  | len == 4 = "0x" ++ s
  where len = length s
fake_hex :: Int -> String
fake_hex i
  | len < 4 = fake_hex' ("0" ++ (show i))
  | len > 4 = fake_hex (mod i 10000)
  | len == 4 = fake_hex' (show i)
  where len = length (show i)

garbage_char :: IO Char
garbage_char = do
  rindex <- randomIO :: IO Int
  return $ garbage !! (mod rindex (length garbage))

garbage_entry' :: Int -> [IO Char] -> [IO Char]
garbage_entry' i s
  | i > 0 = [garbage_char] ++ (garbage_entry' (i - 1) s)
  | otherwise = s

garbage_entry :: [IO Char]
garbage_entry = garbage_entry' entry_width []

litter_entry :: String -> IO String
litter_entry e
  | (length e) < entry_width = do a <- garbage_char
	  litter_entry (e ++ [a])
  | otherwise = return e
 
litter_entries :: [String] -> IO [String]
litter_entries l
  | (length l) < table_size = do a <- sequence garbage_entry; litter_entries (l ++ [a])
  | otherwise = do a <- sequence garbage_entry; return l

render_entry :: Int -> String -> String
render_entry i w =
  (fake_hex i) ++ " " ++ w

render_odd, render_even, render_entries :: Int -> [String] -> String
render_odd i [] = ""
render_odd i (l:ls) =
  (render_entry i l) ++ " " ++ (render_even (i + 1) ls)
render_even i [] = ""
render_even i (l:ls) =
  (render_entry i l) ++ "\n" ++ (render_odd (i + 1) ls)
render_entries i l =
  render_odd i l

print_console :: [String] -> IO ()
print_console l = do
  rng <- newStdGen
  start <- randomIO :: IO Int
  entries <- litter_entries target_words
  entries <- (mapM litter_entry entries)
  putStr $ render_entries start (shuffle' entries (length entries) rng)

print_lifes' :: Int -> IO ()
print_lifes' i
  | i > 0 = do putStr " #"; print_lifes' (i - 1)
  | otherwise = putStrLn ""
  

print_lifes :: Int -> IO ()
print_lifes i = do
  putStr "Attempts remaining:"
  print_lifes' i

game_cycle :: Int -> IO ()
game_cycle i = do
  putStrLn "Password Required"
  putStrLn ""
  print_lifes i
  putStrLn ""
  print_console target_words
  putStr "$:"
  hFlush stdout
  guess_index <- readLn :: IO Int
  let guess = target_words !! (guess_index - 1)
  let solution = target_words !! solution_index
  print $ ">" ++ (map toUpper guess)
  if guess == solution then
    print "I'm in!"
  else do
    print ">Entry denied."
    print $ ">Likeness=" ++ show (likeness guess solution)
    game_cycle (i - 1)

main :: IO ()
main = do
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull Yellow]
  game_cycle starting_lifes
