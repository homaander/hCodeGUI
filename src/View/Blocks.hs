module View.Blocks(blockInfo, blockMatrix) where

import Data.List (transpose)
import qualified Data.Text as T

import Monomer
import TextShow
import Control.Lens

import View.Cfg
import Code.HomaCode

blockInfo :: AppModel -> WidgetNode AppModel AppEvent
blockInfo model = vstack [
  button "Get Info" AppTapeInfo,

  spacer,

  hstack_ [childSpacing_ 50] [
    vstack $ map (`styleBasic` [width 100, textSize 14]) [
      label "Rank:",
      label "Base:",
      label "------------",
      label "Data HN:",
      label "Data Val:",
      label "------------",
      label "Tape ID HN:",
      label "Tape ID Val:",
      label "------------",
      label "Offset:",
      label "Anti Offset:",
      label "Length:"
        `styleBasic` [paddingB 10]
      ],

    vstack $ map (`styleBasic` [width 100, textSize 14]) [
      label $ showt $ length $ T.unpack $ model ^. codeText,
      label $ showt baseVal,
      label "------------",
      label $ "HN " <> codeVal,
      label $ showt $ fromHData codeHN,
      label "------------",
      label $ "HN " <> tapeInfoIdT,
      label $ showt $ fromHData tapeInfoIdHN,
      label "------------",
      label $ showt $ model ^. tapeInfoOffset,
      label $ showt $ model ^. tapeInfoAntiOffset,
      label $ showt $ model ^. tapeInfoLength
      ]
    ]
  ]
    `styleBasic` [border 1 black, paddingT 10, paddingH 40, width 200]
  where
    codeVal  = model ^. codeText
    baseVal  = model ^. selectDataBase

    tapeInfoIdT = model ^. tapeInfoId
    tapeInfoIdHN = getHCodeText baseVal tapeInfoIdT

    codeHN = getHCodeText baseVal codeVal


labelMatrix :: [HNum] -> Color -> WidgetNode AppModel AppEvent
labelMatrix b color = label 
  (T.pack $ mconcat [[toLetter a] <> " " | a <- b])
    `styleBasic` [textColor color]


blockMatrix :: AppModel -> WidgetNode AppModel AppEvent
blockMatrix model = hgrid [
  vstack [
    hgrid [
      dropdown selectRowNum [1 .. 5]
        (\sRow -> hstack [ label "Row: ", label $ showt sRow ]) (label . showt),

      button "Write row" AppToTable,
      button "Read row"  AppFromTable
      ]
      `styleBasic` [ paddingB 30 ],

    box_ [alignCenter] $ vgrid_ [childSpacing_ 10] [
      hgrid_ [childSpacing_ 10] [
        spacer,
        vgrid [ labelMatrix b gray | b <- snd dataTR ],
        spacer
        ],

      hgrid_ [childSpacing_ 10] [
        vgrid [ labelMatrix b gray  | b <- snd dataT ],
        vgrid [ labelMatrix b white | b <-   incodeT ],
        vgrid [ labelMatrix b gray  | b <- fst dataT ]
        ],

      hgrid_ [childSpacing_ 10] [
        spacer,
        vgrid [ labelMatrix b gray | b <- fst dataTR ],
        spacer
        ]
      ]
    ],

  vstack [
    button "Up" AppTUp,
    hgrid [
      button "<-"  AppTLeft,
      button "->" AppTRight
      ],
    button "Down" AppTDown
    ]
      `styleBasic` [paddingH 40, width 200]
  ]
  where
    incodeT  = model ^. codeTable
    incodeTR = transpose incodeT

    dataT  = (map code incodeT, map decode incodeT)
    dataTR = (transpose $ map code incodeTR, transpose $ map decode incodeTR)