module Lang.HomaLang (
    hlangCompExp
  , hlangCompExp2
  , hlangRun
) where


type HLType = Int
type HLCell = Int
type HLVal = Int

type HLCom = (HLType, HLCell, HLVal)

type HLPag = [Int]
type HLIO = [Int]

hlanginit :: [Int]
hlanginit = [1, 50, 1, 0, 0, 0, 0, 0, 0, 1]

hlangRead :: [HLCom] -> HLPag
hlangRead d = hlanginit <> replicate 40 0 <> concat (map conc d)
  where
    conc (a, b, c) = [a, b, c]

hlangRun :: HLIO -> HLPag -> HLIO
hlangRun _ [] = []
hlangRun _ [_] = []
hlangRun _ [_, _] = []
hlangRun _in pag'@(_ : com : ifc : _)  =
    if length pag <= com then
      _in
    else
      hlangRun _out hlres
  where
    pag = take 2 pag' <> [1] <> drop 3 pag'

    hlccell = pag !! (com + 1)

    hlctype = pag !! com
    hlcval' = pag !! (com + 2)
    hlcval  = valRender _in pag hlctype hlcval'

    hlres' =  if ifc == 1 then commandRender hlccell hlcval pag else pag

    
    -- hlres  = error $ show (hlctype, hlcval')
    hlres  = [hlres' !! 0, (hlres' !! 1) + 3] <> drop 2 hlres'

    _out  = if ifc == 1 then getOut _in pag hlctype hlccell hlcval' else _in

valRender :: HLIO -> HLPag -> HLType -> HLVal -> HLVal
valRender _ _   0 v  = v
valRender h _   1 3  = head h
valRender _ pag 1 v  = pag !! v

valRender _ _ _ _  = error "HLANG Parsing error: uncorrect writing type"


getOut ::  HLIO -> HLPag ->  HLType -> HLCell -> HLVal -> HLIO
getOut hlio _ 1 _ 3   = tail hlio
getOut hlio pag t 4 v = hlio <> [valRender hlio pag t v]
getOut hlio _ _ _ _   = hlio


-- 0 Pag
-- 1 Com
-- 2 If
-- 3 In
-- 4 Out
-- 5 A
-- 6 B
-- 7 Sum
-- 8 Dif
-- 9 Eq

-- * в Out можно считать последнюю запись

commandRender :: HLCell -> HLVal -> HLPag -> HLPag
commandRender {- Pag -} 0 0 _ = error "Programm end"
commandRender {- Pag -} 0 _ _ = error "in process"
commandRender {- In  -} 3 _ _ = error "Overwrite in"
commandRender {- Sum -} 7 _ _ = error "Overwrite sum"
commandRender {- Dif -} 8 _ _ = error "Overwrite dif"
commandRender {- Eq  -} 9 _ _ = error "Overwrite eq"
commandRender {- A   -} 5 a pag = take 5 pag <> [a, b, a + b, a - b, if a == b then 1 else 0] <> drop 10 pag
  where b = pag !! 6
-- B
commandRender {- B   -} 6 b pag = take 6 pag <> [b, a + b, a - b, if a == b then 1 else 0] <> drop 10 pag
  where a = pag !! 5

-- Default
commandRender cell val pag = take (cell) pag <> [val] <> drop (cell + 1) pag



{--
A : 8
B : 1

21 <- Com
Out <- Dif
A <- Dif
If <- Eq
If : 0
Com <- 21
--}

hlangCompExp2 :: HLPag
hlangCompExp2 = hlangRead [
    (0, 5, 8)
  , (0, 6, 1)
  , (1, 21, 1)
  , (1, 4, 8)
  , (1, 5, 8)
  , (1, 2, 9)
  , (0, 2, 0)
  , (1, 1, 21)
  ]

hlangCompExp :: HLPag
hlangCompExp = hlangRead hlangExamnple

hlangEx :: [String]
hlangEx = [
    "Out : 1"

  , "A <- In"
  , "B : 7"

  , "Out <- Sum"
  ]

hlangExamnple :: [(Int, Int, Int)]
hlangExamnple = [
    (0, 4, 1)
  , (1, 5, 3)
  , (0, 6, 7)
  , (1, 4, 7)
  ]



-- >>> hlangCompExp
-- >>> length hlangCompExp
-- >>> commandRender 1 21 hlangCompExp
-- >>> valRender 1 1 [] hlangCompExp
-- Couldn't match type `[Int]' with `Int'
-- Expected: HLVal
--   Actual: HLPag
-- In the fourth argument of `valRender', namely `hlangCompExp'
-- In the expression: valRender 1 1 [] hlangCompExp
-- In an equation for `it_a5ZCd':
--     it_a5ZCd = valRender 1 1 [] hlangCompExp
-- Couldn't match type `[a0_a5ZDQ[tau:1]]' with `Int'
-- Expected: HLType
--   Actual: [a0_a5ZDQ[tau:1]]
-- In the third argument of `valRender', namely `[]'
-- In the expression: valRender 1 1 [] hlangCompExp
-- In an equation for `it_a5ZCd':
--     it_a5ZCd = valRender 1 1 [] hlangCompExp

