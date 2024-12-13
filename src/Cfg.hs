{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Cfg where

import Data.Text (Text)

import Monomer
import Control.Lens
import Code.HomaCode.Data

data AppModel = AppModel {
  _codeText           :: Text,
  _codeTable          :: [[HNum]],

  _codeNC             :: Int,

  _selectDataBase     :: HBase,
  _selectRowNum       :: Int,

  _tapeInfoId         :: Text,
  _tapeInfoOffset     :: Int,
  _tapeInfoAntiOffset :: Int,
  _tapeInfoLength     :: Int
  } deriving (Eq, Show)

makeLenses 'AppModel

data AppEvent = AppInit

              | AppDecode
              | AppCode
              | AppTapeInfo

              | AppToTable
              | AppFromTable

              | AppTUp
              | AppTDown
              | AppTLeft
              | AppTRight
  deriving (Eq, Show)

type WidgetEnv'    = WidgetEnv AppModel AppEvent
type WidgetNode'   = WidgetNode AppModel AppEvent
type AppEventResp' = AppEventResponse AppModel AppEvent
