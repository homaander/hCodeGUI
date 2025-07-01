import Code.HomaCode



-- main :: IO ()
-- main = do
  -- runApp

  -- print $ getTapeIdParallel (getHCodeText 37 "ANDREW") [1 .. 50]
  -- print $ getTapeIdFromChank (getHCodeText 37 "LOL") 1

   -- print $ Point2 1 1

  -- let
  --   base = 37
  --   rank = 6 
  --   dat  = HC.getHCodeText base "ANDREW"
  -- print $ HC.trapFinderLength base rank dat

  -- let
  --   preset  = HC.getPreset @HNum base rank 100000
  --   results = iterate (HC.runPreset preset) dat !! 1425
  -- print $ showHCode $ HC.codeN 40356 results

main :: IO ()
main = do
  let 
    v = hn 16 [12, 2, 13, 3, 14, 4]
    preset_decode66 = getPreset 16 6 (-66)
    preset_code66   = getPreset 16 6 66

  putStrLn "-- HData"
  test "showHCode " (showHCode v) "C2D3E4"

  -- Получение данных из числа
  test "toHData 10" (showHCode $ toHData 10 1234) "1234"
  test "toHData 16" (showHCode $ toHData 16 1234) "4D2"


  putStrLn "-- Code"
  -- Кодирование 1 раз
  test "code      " (showHCode $ code v) "6B6B6C"
  -- Кодирование N раз
  test "codeN     " (showHCode $ codeN 66 v) "584C32"
  test "-^>       " (showHCode $ v -^> 66) "584C32"

  -- Кодирование через пресет
  test "preset cod" (showHCode $ runPreset preset_code66 v) "584C32"


  putStrLn "-- Decode"
  -- Декодировать 1 раз
  test "decode    " (showHCode $ decode v) "425240"
  -- Декодировать N раз
  test "decodeN   " (showHCode $ decodeN 66 v) "323BB5"
  test "<^-       " (showHCode $ v <^- 66) "323BB5"

  -- Декодирование через пресет
  test "preset dec" (showHCode $ runPreset preset_decode66 v) "323BB5"

  putStrLn "-- Tape"
  -- Основные данные заданного кода в ленте
  -- getHCodeText 16 "007454"
  test "toTape    " (show $ toTape v) (show $ HTape {tapeId = hn 16 [0,0,7,4,5,4], tapeOffset = 275, tapeAntiOffset = 229, tapeLength = 504})

  putStrLn "-- TapeInfo"
  -- Узнать какие ленты можно получить из суммы двух, смещая 2 ленту до 500
  -- test "getSumsList" 
  let a = getSumsList @HNum 200 (hn 10 [0,0,0,0,1]) (hn 10 [0,0,0,0,3])
  print $ map showHCode a

  -- Узнать как из ленты a и b получить ленту c
  --  Результат: (требуемое смещение b, полученное смещение c)
  -- test (take 10 $ getOfsetsSums @[Int] [0,0,1] [0,0,7] [0,0,3]) [(1,198),(2,305),(3,110),(5,370),(8,233),(10,285),(11,253),(13,332),(15,240),(19,20)]




test :: (Eq a, Show a) => String -> a -> a -> IO ()
test str a b = if a == b
           then putStrLn ("OK: " <> "(" <> str <> ")  " <> show a)
           else error    ("FL: " <> "(" <> str <> ")  " <> show a)





-- pattern Point2 :: a -> b -> (a, b)
-- pattern Point2{x, y} = (x, y)


-- >>> toHData @HNum 10 123
-- [HN {hBase = 10, hVal = 1},HN {hBase = 10, hVal = 2},HN {hBase = 10, hVal = 3}]

-- >>> map fromHData $ getPreset @HNum 10 5 33
-- >>> map fromHData $ getPreset @HNum 10 5 66
-- [61086,16964,9254,86505,64451]
-- [74346,40890,38643,49407,60374]