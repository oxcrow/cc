unit = ()

getStringListFromString :: String -> [String]
getStringListFromString = words

getIntListFromString :: String -> [Int]
getIntListFromString line = map read (getStringListFromString line)

readIntList :: IO [Int]
readIntList = do
  line <- getLine
  let list = getIntListFromString line
  return list

readInt :: IO Int
readInt = do readLn :: IO Int

getnm :: IO (Int, Int)
getnm = do
  nm <- readIntList
  let n = nm !! 0
  let m = nm !! 1
  return (n, m)

maxDifferenceSum :: [Int] -> [Int] -> Int -> Int
maxDifferenceSum [a] [_] acc = acc + a
maxDifferenceSum a b acc =
  let a' = head a
      b' = head b
      d = a' - b'
      d' = if d >= 0 then d else 0
      sum = acc + d'
      maxSum = maxDifferenceSum (tail a) (tail b) sum
   in maxSum

solve :: Int -> IO ()
solve 0 = return unit
solve t = do
  n <- readInt
  a' <- readIntList
  b' <- readIntList
  -- The only way we can increase a's score is to select days
  -- after which b has a low score. To maximize the difference
  -- we need to select the days that cause the highest difference.
  -- If b has a higher score every day then only train on the last day;
  let a = a'
  let b = tail (b' ++ [0])
  let maxSum = maxDifferenceSum a b 0
  let solution = maxSum
  print solution
  solve (t - 1)
  return unit

main :: IO ()
main = do
  t <- readLn
  solve t
  return unit
