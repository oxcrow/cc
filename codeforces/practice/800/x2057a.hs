unit = ()

getStringListFromString :: String -> [String]
getStringListFromString = words

getIntListFromString :: String -> [Int]
getIntListFromString line = map read (getStringListFromString line)

solve :: Int -> IO ()
solve 0 = return unit
solve t = do
  line <- getLine
  let nm = getIntListFromString line
  let n = nm !! 0
  let m = nm !! 1
  let solution = max n m + 1
  print solution
  solve (t - 1)
  return unit

main :: IO ()
main = do
  t <- readLn
  solve t
  return unit
