unit = ()

getStringListFromString :: String -> [String]
getStringListFromString = words

getIntListFromString :: String -> [Int]
getIntListFromString line = map read (getStringListFromString line)

getnm :: String -> (Int, Int)
getnm nm = do
  let nm' = getIntListFromString nm
  let n = nm' !! 0
  let m = nm' !! 1
  (n, m)

solve :: Int -> IO ()
solve 0 = return unit
solve t = do
  nm <- getLine
  let (n, m) = getnm nm
  let solution = max n m + 1
  print solution
  solve (t - 1)
  return unit

main :: IO ()
main = do
  t <- readLn
  solve t
  return unit
