unit = ()

getStringListFromString :: String -> [String]
getStringListFromString = words

getIntListFromString :: String -> [Int]
getIntListFromString line = map read (getStringListFromString line)

readInt :: IO Int
readInt = do readLn :: IO Int

readIntList :: IO [Int]
readIntList = do
  line <- getLine
  let list = getIntListFromString line
  return list

readStringLines :: Int -> [String] -> IO [String]
readStringLines 0 acc = return acc
readStringLines n acc = do
  line <- getLine
  let acc' = acc ++ [line]
  result <- readStringLines (n - 1) acc'
  return result

readNM :: IO (Int, Int)
readNM = do
  nm <- readIntList
  let n = nm !! 0
  let m = nm !! 1
  return (n, m)

solve :: Int -> IO ()
solve 0 = return unit
solve t = do
  (n, m) <- readNM
  s <- readStringLines n []
  let l = map length s
  let solution = maxWords l m 0 0
  print solution
  solve (t - 1)
  return unit
  where
    maxWords :: [Int] -> Int -> Int -> Int -> Int
    maxWords [] m sum x = x
    maxWords l m sum x =
      let h = head l
          t = tail l
          sum' = sum + h
          result =
            if sum' <= m
              then maxWords t m sum' (x + 1)
              else maxWords t m sum' x
       in result

main :: IO ()
main = do
  t <- readLn
  solve t
  return unit
