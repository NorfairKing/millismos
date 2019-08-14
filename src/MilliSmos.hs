{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module MilliSmos
  ( milliSmos
  ) where

import Data.Maybe

import qualified Data.ByteString as SB
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Directory
import System.Environment
import System.Exit

import Cursor.Brick.Forest
import Cursor.Brick.Text
import Cursor.Brick.Tree
import Cursor.Forest
import Cursor.List.NonEmpty
import Cursor.Text
import Cursor.Tree
import Cursor.Types

import Data.Tree
import Data.Yaml as Yaml

import Path.IO

import Brick as Brick
import Brick.Main as Brick
import Brick.Widgets.Border as Brick
import Brick.Widgets.Center as Brick
import Brick.Widgets.Core as Brick

import Graphics.Vty.Attributes as Vty
import Graphics.Vty.Input.Events as Vty

milliSmos :: IO ()
milliSmos = do
  (file:_) <- getArgs
  errOrContents <- forgivingAbsence $ SB.readFile file
  fc <-
    case errOrContents of
      Nothing -> pure $ makeForestCursor toTextCursor (cTree True (Node "" []) :| [])
      Just contents ->
        case Yaml.decodeEither' contents of
          Left err -> die $ "Failed to read tree file: " <> prettyPrintParseException err
          Right tree ->
            pure $
            makeForestCursor toTextCursor (NE.map (cTree True) (tree :: NonEmpty (Tree Text)))
  let s = State {stateCursor = fc, stateMode = EditForest}
  s' <- Brick.defaultMain milliSmosApp s
  SB.writeFile file $
    Yaml.encode $ NE.map rebuildCTree $ rebuildForestCursor toText $ stateCursor s'

toTextCursor :: Text -> TextCursor
toTextCursor = fromMaybe (error "Wasn't a single line") . makeTextCursor

toText :: TextCursor -> Text
toText = rebuildTextCursor

data State =
  State
    { stateCursor :: ForestCursor TextCursor Text
    , stateMode :: Mode
    }
  deriving (Show, Eq)

data Mode
  = EditForest
  | EditText
  deriving (Show, Eq)

milliSmosApp :: App State e ResourceName
milliSmosApp =
  App
    { appDraw = draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure
    , appAttrMap =
        const $
        attrMap
          Vty.defAttr
          [ (nodeAttr, fg yellow)
          , (selectedAttr, fg brightWhite)
          , (modeAttr, fg green)
          , (infoAttr, fg blue)
          , (warningAttr, fg red)
          ]
    }

draw :: State -> [Widget ResourceName]
draw s =
  [ padAll 1 $
    vBox
      [ withAttr nodeAttr $
        padBottom Max $
        verticalForestCursorWidget drawTextCTree (treeCursorWidget wrap cur) drawTextCTree $
        stateCursor s
      , hBorder
      , withAttr modeAttr drawMode
      , hBorder
      , withAttr infoAttr drawInfo
      , hBorder
      , hCenterLayer $
        withAttr warningAttr $
        strWrap
          "Note that this is a tree editor, not a forest editor, so there is no way to add a sibling to the root node."
      ]
  ]
  where
    drawMode :: Widget n
    drawMode =
      str $
      case stateMode s of
        EditText -> "Editing selected node's text"
        EditForest -> "Editing tree"
    drawInfo :: Widget n
    drawInfo =
      let explanations =
            map (\(k, v) -> (str k, str v)) $
            case stateMode s of
              EditText ->
                [ ("<char>", "insert character")
                , ("Backspace", "remove character")
                , ("Delete", "delete character")
                , ("Left", "move left within node")
                , ("Right", "move right within node")
                , ("Enter, Esc", "Switch mode")
                ]
              EditForest ->
                [ ("i, I, a, A", "switch mode")
                , ("j", "Add a sibling node")
                , ("J", "Add a child node")
                , ("d", "Delete selected node")
                , ("D", "Delete selected subtree")
                , ("h", "Promote the selected node")
                , ("H", "Promote the selected subtree")
                , ("l", "Demote the selected node")
                , ("L", "Demote the selected subtree")
                , ("Up", "move to previous node")
                , ("Down", "move to next node")
                , ("Esc", "Exit")
                ]
       in (\(ks, vs) -> hBox $ [vBox ks, padLeft (Pad 2) $ vBox vs]) $ unzip explanations
    cur :: TextCursor -> CForest Text -> Widget ResourceName
    cur tc cf =
      let ecw = withAttr selectedAttr $ (str "> " <+>) $ drawTextCursor tc
          rest = padLeft defaultPadding $ drawCForest cf
       in vBox [ecw, rest]
    wrap :: [CTree Text] -> Text -> [CTree Text] -> Widget n -> Widget n
    wrap tsl e tsr w =
      let befores = map drawTextCTree tsl
          ew = textWidget e
          afters = map drawTextCTree tsr
       in (str "- " <+> ew) <=> padLeft defaultPadding (vBox $ concat [befores, [w], afters])
    drawTextCursor :: TextCursor -> Widget ResourceName
    drawTextCursor =
      visible .
      (case stateMode s of
         EditText -> selectedTextCursorWidget TextResource
         EditForest -> textCursorWidget)

drawCForest :: CForest Text -> Widget n
drawCForest cf =
  case cf of
    EmptyCForest -> emptyWidget
    ClosedForest _ -> emptyWidget
    OpenForest ts ->
      let etws = map drawTextCTree $ NE.toList ts
       in vBox etws

drawTextCTree :: CTree Text -> Widget n
drawTextCTree (CNode t cf) =
  vBox [hBox [str "- ", textWidget t], padLeft defaultPadding (drawCForest cf)]

nodeAttr :: AttrName
nodeAttr = "node"

selectedAttr :: AttrName
selectedAttr = "selected"

modeAttr :: AttrName
modeAttr = "mode"

infoAttr :: AttrName
infoAttr = "info"

warningAttr :: AttrName
warningAttr = "warning"

defaultPadding :: Padding
defaultPadding = Pad 4

data ResourceName =
  TextResource
  deriving (Show, Eq, Ord)

handleEvent :: State -> BrickEvent n e -> EventM n (Next State)
handleEvent s e =
  case e of
    VtyEvent ve ->
      case ve of
        EvKey key mods ->
          let textDOUo ::
                   (TextCursor -> Maybe (DeleteOrUpdate TextCursor)) -> (EventM n (Next State))
              textDOUo func = textDo $ dullMDelete . func
              textDo :: (TextCursor -> Maybe TextCursor) -> (EventM n (Next State))
              textDo func = mDo $ (forestCursorSelectedTreeL . treeCursorCurrentL) func
              mDo ::
                   (ForestCursor TextCursor Text -> Maybe (ForestCursor TextCursor Text))
                -> (EventM n (Next State))
              mDo func =
                let fc = stateCursor s
                    fc' = fromMaybe fc $ func fc
                 in continue $ s {stateCursor = fc'}
           in case stateMode s of
                EditText ->
                  let switchMode = continue $ s {stateMode = EditForest}
                   in case key of
                        KChar c -> textDo $ textCursorInsert c
                        KBS -> textDOUo textCursorRemove
                        KDel -> textDOUo textCursorDelete
                        KLeft -> textDo textCursorSelectPrev
                        KRight -> textDo textCursorSelectNext
                        KEnter -> switchMode
                        KEsc -> switchMode
                        _ -> continue s
                EditForest ->
                  let switchMode = continue $ s {stateMode = EditText}
                   in case key of
                        KChar 'i' -> switchMode
                        KChar 'I' -> switchMode
                        KChar 'a' -> switchMode
                        KChar 'A' -> switchMode
                        KChar 'h' -> mDo $ forestCursorPromoteElem toText toTextCursor
                        KChar 'H' -> mDo $ forestCursorPromoteSubTree toText toTextCursor
                        KChar 'l' -> mDo $ forestCursorDemoteElem toText toTextCursor
                        KChar 'L' -> mDo $ forestCursorDemoteSubTree toText toTextCursor
                        KChar 'j' -> mDo $ Just . forestCursorAppend ""
                        KChar 'J' -> mDo $ Just . forestCursorAddChildToNodeAtStart ""
                        KChar 'd' ->
                          mDo $ \tc ->
                            case forestCursorRemoveElem toTextCursor tc of
                              Deleted -> Nothing
                              Updated tc' -> Just tc'
                        KChar 'D' ->
                          mDo $ \tc ->
                            case forestCursorRemoveSubTree toTextCursor tc of
                              Deleted -> Nothing
                              Updated tc' -> Just tc'
                        KUp -> mDo $ forestCursorSelectPrev toText toTextCursor
                        KDown -> mDo $ forestCursorSelectNext toText toTextCursor
                        KEsc -> halt s
                        _ -> continue s
        _ -> continue s
    _ -> continue s
