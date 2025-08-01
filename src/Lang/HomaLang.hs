module Lang.HomaLang (
    hlangEx1H
  , hlangEx2H
  , hlangEx3H

  , pagGen
  , readHLComs
  , toHLPag

  , runHLPag
) where


data HDat = Pag | Com | If | Buf | A | B | Sum | Dif | Eq | Def Int
data HCom = S HDat HDat | R HDat HDat

type HLType = Int
type HLCell = Int
type HLVal = Int

type HLCom = (HLType, HLCell, HLVal)

type HLPag = [Int]
type HLIO = [Int]


pagGen :: Int -> [HCom] -> HLPag
pagGen i = toHLPag i . readHLComs

readHLComs :: [HCom] -> [HLCom]
readHLComs = map readHLCom

readHLCom :: HCom -> HLCom
readHLCom (S a b) = (0, fromHDat a, fromHDat b)
readHLCom (R a b) = (1, fromHDat a, fromHDat b)

fromHDat :: HDat -> Int
fromHDat Pag = 0
fromHDat Com = 1
fromHDat If  = 2
fromHDat Buf = 3
fromHDat A   = 4
fromHDat B   = 5
fromHDat Sum = 6
fromHDat Dif = 7
fromHDat Eq  = 8
fromHDat (Def x) = x


toHLPag :: Int -> [HLCom] -> HLPag
toHLPag pag_id d = hlanginit <> replicate 40 0 <> concat (map conc d)
  where
    conc (a, b, c) = [a, b, c]
    hlanginit      = [pag_id, 50, 1, 0, 0, 0, 0, 0, 0, 228]





runHLPag :: HLIO -> HLPag -> (Int, HLIO)
runHLPag _ [] = (0, [])
runHLPag io pag@(pag_id :_) = runHLPag' (pag_id, io) pag


runHLPag' :: (Int, HLIO) -> HLPag -> (Int, HLIO)
runHLPag' _ [] = (0, [])
runHLPag' _ [_] = (0, [])
runHLPag' _ [_, _] = (0, [])
runHLPag' (_pag_id, _in) pag'@(pag_id : com : ifc : _)  =
    if length pag <= com || _pag_id /= pag_id
      then (pag_id, _in)
      else runHLPag' (pag_id, _out) hlres
  where
    pag = take 2 pag' <> [1] <> drop 3 pag'

    (hlctype : hlccell : hlcval' : _) = drop com pag

    (hlcval, _out) = if ifc == 0
                     then (hlcval', _in) 
                     else valRender _in pag hlctype hlccell hlcval'

    hlres' = if ifc == 0
             then pag 
             else commandRender pag hlccell hlcval

    
    -- hlres  = error $ show (hlctype, hlcval')
    hlres  = [hlres' !! 0, (hlres' !! 1) + 3] <> drop 2 hlres'



valRender :: HLIO -> HLPag -> HLType -> HLCell -> HLVal -> (HLVal, HLIO)
-- Write to buffer
valRender io     _   0 3 v = (v, io <> [v])
valRender io     pag 1 3 v = (v, io <> [pag !! v])
-- Read buffer
valRender (x:xs) _   1 _ 3 = (x, xs)

valRender io     _   0 _ v = (v, io)
valRender io     pag 1 _ v = (pag !! v, io)

valRender _      _   _ _ _ = error "HLANG Parsing error: uncorrect writing type"



commandRender :: HLPag -> HLCell -> HLVal -> HLPag
commandRender {- Sum -} _ 6 _ = error "Overwrite sum"
commandRender {- Dif -} _ 7 _ = error "Overwrite dif"
commandRender {- Eq  -} _ 8 _ = error "Overwrite  eq"

commandRender {- A   -} pag 4 a = take 4 pag <> [a, b, a + b, a - b, if a == b then 1 else 0, 228] <> drop 10 pag
  where b = pag !! 5
commandRender {- B   -} pag 5 b = take 5 pag <> [   b, a + b, a - b, if a == b then 1 else 0, 228] <> drop 10 pag
  where a = pag !! 4

commandRender pag cell val = take (cell) pag <> [val] <> drop (cell + 1) pag



hlangEx1H :: [HCom]
hlangEx1H = [
    S Buf (Def 1)

  , R A Buf
  , S B (Def 7)

  , R Buf Sum
  ]


hlangEx2H :: [HCom]
hlangEx2H = [
    S Buf (Def 2)
  , R A Buf
  , S B (Def 1)
  
  , R (Def 21) Com
  , R Buf Dif
  , R A Dif
  , R If Eq
  , S If (Def 0)
  , R Com (Def 21)
  ]


hlangEx3H :: [HCom]
hlangEx3H = [
    S Buf (Def 3)
  , R A Buf
  , S B (Def 17)
  , R Buf Sum
  , S Pag (Def 0)
  ]

-- >>> readHLComs hlangEx2H
-- >>> pagGen 2 hlangEx2H
-- [(0,3,2),(1,4,3),(0,5,1),(1,21,1),(1,3,7),(1,4,7),(1,2,8),(0,2,0),(1,1,21)]
-- [2,50,1,0,0,0,0,0,0,228,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,2,1,4,3,0,5,1,1,21,1,1,3,7,1,4,7,1,2,8,0,2,0,1,1,21]


-- >>> hlangCompExp
-- >>> length hlangCompExp
-- >>> commandRender 1 21 hlangCompExp
-- >>> valRender [] hlangCompExp 1 1
-- [1,50,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,1,1,5,3,0,6,7,1,4,7]
-- 62
-- [1,21,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,1,1,5,3,0,6,7,1,4,7]
-- 50

