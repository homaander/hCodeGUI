{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Main(main) where

import Data.List (transpose)

import Monomer
import Control.Lens
import TextShow ( TextShow(showt) )
-- import qualified Data.ByteString as B


import Cfg
import View.Blocks
import Code.HomaCode


main :: IO ()
main = do
    startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "H Code App",
      appWindowIcon  "./assets/icon.png",
      appWindowState (MainWindowNormal (800, 800)),
      appTheme       darkTheme,
      appFontDef     "Regular" "./assets/fonts/FiraCode-Light.ttf",
      appInitEvent   AppInit
      ]

    model = AppModel "12345" [] 1 10 1 "" 0 0 0 ""


buildUI :: WidgetEnv' -> AppModel -> WidgetNode'
buildUI _ model = widgetTree
  where
    widgetTree = vstack [
      label "H_Code" `styleBasic` [ textSize 32, paddingB 10 ],
      hgrid [
        label (model ^. appTestText) `styleBasic` [ textSize 32, paddingB 10 ],
        button "GO" AppTest
        ],
      label "Default code:",

      hgrid [
        vstack [
          hgrid [
            label "Count: ",
            numericField codeNC
              `styleBasic` [textCenter]
            ],

          dropdown selectDataBase [2, 10, 16, 37]
            (\sRow -> hstack [ label "Base: ", label $ showt sRow ])
            (label . showt)
            `styleBasic` [paddingB 10],

          hgrid [
            label "Data: ",
            textField codeText
              `styleBasic` [textCenter]
            ],

          hgrid [
            button "<----" AppDecode,
            button "---->"   AppCode
            ]
              `styleBasic` [width 300]
          ]
            `styleBasic` [paddingH 10, width 300],

        blockInfo model
        ],

      label "Table:" `styleBasic` [paddingB 10],

      -- animShake_ [ shakeV, autoStart_ True, duration (Millisecond 500) ] $ blockMatrix model,
      blockMatrix model
      ]
        `styleBasic` [ padding 10 ]


handleEvent :: WidgetEnv' -> WidgetNode' -> AppModel -> AppEvent -> [AppEventResp']
handleEvent _ _ model evt =
  case evt of
    AppInit      -> []
    AppDecode    -> [ Model $ model & codeText .~ fst dataV ]
    AppCode      -> [ Model $ model & codeText .~ snd dataV ]
    AppTapeInfo  -> [ Model 
                        $ model
                          & tapeInfoId         .~ tapeId         tapeV
                          & tapeInfoOffset     .~ tapeOffset     tapeV
                          & tapeInfoAntiOffset .~ tapeAntiOffset tapeV
                          & tapeInfoLength     .~ tapeLength     tapeV
                      ]
    AppTDown     -> [ Model $ model & codeTable .~ fst dataTR ]
    AppTUp       -> [ Model $ model & codeTable .~ snd dataTR ]
    AppTRight    -> [ Model $ model & codeTable .~ fst dataT  ]
    AppTLeft     -> [ Model $ model & codeTable .~ snd dataT  ]

    AppToTable   -> [ Model
                        $ model
                          & codeTable .~ incodeTUpdate
                      ]

    AppFromTable -> [ Model
                        $ model
                          & codeText  .~ showHCodeText (incodeT !! (rowNum - 1))
                      ]
    AppTest -> [ Model $ model & appTestText .~ "LOL" ]
  where
    -- Code / Decode
    baseVal  = model ^. selectDataBase
    codeVal  = model ^. codeText
    codeNVal = model ^. codeNC

    rowNum = model ^. selectRowNum

    codeHN = getHCodeText baseVal codeVal

    dataV = dataText codeNVal    codeHN
    tapeV = tapeText $ toTape codeHN

    -- Table Code / Decode
    incodeT  = map (resetBase baseVal) $ model ^. codeTable
    incodeTR = transpose incodeT

    incodeTUpdate = take (rowNum - 1) incodeT <> [codeHN] <> drop rowNum incodeT

    dataT  = (map code incodeT, map decode incodeT)
    dataTR = (transpose $ map code incodeTR, transpose $ map decode incodeTR)
