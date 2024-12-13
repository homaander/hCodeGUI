import Code.HomaCode
import Code.HomaCode.Data


main :: IO ()
main = do
  putStrLn "Test start"

  let v = [HN 16 12,HN 16 2,HN 16 13,HN 16 3,HN 16 14,HN 16 4]

  test "showHCode" (showHCode v) "C2D3E4"

  -- HData
  --   Получение данных из числа
  test "toHData 10" (showHCode $ toHData 10 1234) "1234"
  test "toHData 16" (showHCode $ toHData 16 1234) "4D2"


  -- Code
  --   Кодирование 1 раз
  test "code" (showHCode $ code v) "6B6B6C"
  --   Кодирование N раз
  test "codeN" (showHCode $ codeN 66 v) "584C32"
  test "codeN arrow" (showHCode $ v -^> 66) "584C32"

  --   Кодирование через пресет
  let preset66 = getPreset 16 6 66
  test "runPreset code" (showHCode $ runPreset preset66 v) "584C32"


  --   Декодировать 1 раз
  test "decode" (showHCode $ decode v) "425240"
  --   Декодировать N раз
  test "decodeN" (showHCode $ decodeN 66 v) "323BB5"
  test "decodeN arrow" (showHCode $ v <^- 66) "323BB5"

  --   Декодирование через пресет
  let preset_66 = getPreset 16 6 (-66)
  test "runPreset decode" (showHCode $ runPreset preset_66 v) "323BB5"

  -- Tape
  --   Основные данные заданного кода в ленте
  --   [HN 16 0,HN 16 0,HN 16 7,HN 16 4,HN 16 5,HN 16 4]
  test "toTape" (show $ toTape v) (show $ HTape {tapeId = getHCodeText 16 "007454", tapeOffset = 275, tapeAntiOffset = 229, tapeLength = 504})

  -- -- TapeInfo
  -- --   Узнать какие ленты можно получить из суммы двух, смещая 2 ленту до 500
  -- test (getSumsList @[Int] [0,0,1] [0,0,3]) [[0,0,4],[0,0,1],[0,0,3],[0,0,2]]

  -- --   Узнать как из ленты a и b получить ленту c
  -- --     Результат: (требуемое смещение b, полученное смещение c)
  -- test (take 10 $ getOfsetsSums @[Int] [0,0,1] [0,0,7] [0,0,3]) [(1,198),(2,305),(3,110),(5,370),(8,233),(10,285),(11,253),(13,332),(15,240),(19,20)]

  putStrLn "-----------"
  putStrLn "Test Ok"
  putStrLn "Test Ok"
  putStrLn "Test Ok"
  putStrLn "-----------"
  putStrLn ""
  putStrLn ""
  putStrLn ""
  putStrLn ""
  putStrLn ""




test :: (Eq a, Show a) => String -> a -> a -> IO ()
test str a b = if a == b
           then putStrLn (str <> ", ok: " <> show a <> " , " <> show b)
           else error (str <> ", failed: " <> show a <> " , " <> show b)
