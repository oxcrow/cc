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

window :: Int -> [a] -> [[a]]
window size list =
  ( case list of
      [] -> []
      x : xs ->
        if length list >= size
          then (take size list) : window size xs
          else window size xs
  )

isStable [x, y]
  | xxy && xyy = True
  | otherwise = False
  where
    xxy = x + x > y && x + y > x
    xyy = x + y > y && y + y > x

solve :: Int -> IO ()
solve 0 = return unit
solve t = do
  n <- readInt
  a <- readIntList
  -- In set (x, y),
  --  I. (x,x,x) and (y,y,y) are stable
  -- II. (x,x,y) and (x,y,y) could be unstable
  -- Thus we should investigate case II. for our answer
  let w = window 2 a
  let s = map isStable w
  let ss = any (== True) s
  let solution = if ss then "YES" else "NO"
  putStrLn $ id solution
  solve (t - 1)
  return unit

main :: IO ()
main = do
  t <- readLn
  solve t
  return unit
