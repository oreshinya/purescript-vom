module Test.Element where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Types (htmlElementToNode)
import DOM.HTML.Window (document)
import DOM.Node.Node (childNodes, firstChild, textContent)
import DOM.Node.NodeList (item)
import Data.Array (head, length, range, (!!))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Test.Assert (ASSERT, assert)
import Test.TestHelper (clearBody)
import VOM (VNode(..), h, patch, t, (:=))



assertDiffAlgorithmForElement :: forall e. Eff (dom :: DOM, assert :: ASSERT | e) Unit
assertDiffAlgorithmForElement = do
  assertFor base
  assertFor reversed
  assertFor unshifted
  assertFor divided
  assertFor pushed
  assertFor shifted
  assertFor removed
  assertFor popped
  assertFor unshiftedReversed
  assertFor dividedReversed
  assertFor pushedReversed
  assertFor shiftedReversed
  assertFor removedReversed
  assertFor poppedReversed
  assertFor chaotic
  where
    base =
      [ h "div" [ "key" := "1" ] [ t "text 1" ]
      , h "div" [ "key" := "2" ] [ t "text 2" ]
      , h "div" [ "key" := "3" ] [ t "text 3" ]
      , h "div" [ "key" := "4" ] [ t "text 4" ]
      , h "div" [ "key" := "5" ] [ t "text 5" ]
      ]

    reversed =
      [ h "div" [ "key" := "5" ] [ t "text 5" ]
      , h "div" [ "key" := "4" ] [ t "text 4" ]
      , h "div" [ "key" := "3" ] [ t "text 3" ]
      , h "div" [ "key" := "2" ] [ t "text 2" ]
      , h "div" [ "key" := "1" ] [ t "text 1" ]
      ]

    unshifted =
      [ h "div" [ "key" := "8" ] [ t "text 8" ]
      , h "div" [ "key" := "0" ] [ t "text 0" ]
      , h "div" [ "key" := "1" ] [ t "text 1" ]
      , h "div" [ "key" := "2" ] [ t "text 2" ]
      , h "div" [ "key" := "3" ] [ t "text 3" ]
      , h "div" [ "key" := "4" ] [ t "text 4" ]
      , h "div" [ "key" := "5" ] [ t "text 5" ]
      ]

    divided =
      [ h "div" [ "key" := "1" ] [ t "text 1" ]
      , h "div" [ "key" := "2" ] [ t "text 2" ]
      , h "div" [ "key" := "8" ] [ t "text 8" ]
      , h "div" [ "key" := "0" ] [ t "text 0" ]
      , h "div" [ "key" := "3" ] [ t "text 3" ]
      , h "div" [ "key" := "4" ] [ t "text 4" ]
      , h "div" [ "key" := "5" ] [ t "text 5" ]
      ]

    pushed =
      [ h "div" [ "key" := "1" ] [ t "text 1" ]
      , h "div" [ "key" := "2" ] [ t "text 2" ]
      , h "div" [ "key" := "3" ] [ t "text 3" ]
      , h "div" [ "key" := "4" ] [ t "text 4" ]
      , h "div" [ "key" := "5" ] [ t "text 5" ]
      , h "div" [ "key" := "6" ] [ t "text 6" ]
      , h "div" [ "key" := "7" ] [ t "text 7" ]
      ]

    shifted =
      [ h "div" [ "key" := "2" ] [ t "text 2" ]
      , h "div" [ "key" := "3" ] [ t "text 3" ]
      , h "div" [ "key" := "4" ] [ t "text 4" ]
      , h "div" [ "key" := "5" ] [ t "text 5" ]
      ]

    removed =
      [ h "div" [ "key" := "1" ] [ t "text 1" ]
      , h "div" [ "key" := "2" ] [ t "text 2" ]
      , h "div" [ "key" := "4" ] [ t "text 4" ]
      , h "div" [ "key" := "5" ] [ t "text 5" ]
      ]

    popped =
      [ h "div" [ "key" := "1" ] [ t "text 1" ]
      , h "div" [ "key" := "2" ] [ t "text 2" ]
      , h "div" [ "key" := "3" ] [ t "text 3" ]
      , h "div" [ "key" := "4" ] [ t "text 4" ]
      ]

    unshiftedReversed =
      [ h "div" [ "key" := "8" ] [ t "text 8" ]
      , h "div" [ "key" := "0" ] [ t "text 0" ]
      , h "div" [ "key" := "5" ] [ t "text 5" ]
      , h "div" [ "key" := "4" ] [ t "text 4" ]
      , h "div" [ "key" := "3" ] [ t "text 3" ]
      , h "div" [ "key" := "2" ] [ t "text 2" ]
      , h "div" [ "key" := "1" ] [ t "text 1" ]
      ]

    dividedReversed =
      [ h "div" [ "key" := "5" ] [ t "text 5" ]
      , h "div" [ "key" := "4" ] [ t "text 4" ]
      , h "div" [ "key" := "3" ] [ t "text 3" ]
      , h "div" [ "key" := "8" ] [ t "text 8" ]
      , h "div" [ "key" := "0" ] [ t "text 0" ]
      , h "div" [ "key" := "2" ] [ t "text 2" ]
      , h "div" [ "key" := "1" ] [ t "text 1" ]
      ]

    pushedReversed =
      [ h "div" [ "key" := "5" ] [ t "text 5" ]
      , h "div" [ "key" := "4" ] [ t "text 4" ]
      , h "div" [ "key" := "3" ] [ t "text 3" ]
      , h "div" [ "key" := "2" ] [ t "text 2" ]
      , h "div" [ "key" := "1" ] [ t "text 1" ]
      , h "div" [ "key" := "0" ] [ t "text 0" ]
      , h "div" [ "key" := "8" ] [ t "text 8" ]
      ]

    shiftedReversed =
      [ h "div" [ "key" := "4" ] [ t "text 4" ]
      , h "div" [ "key" := "3" ] [ t "text 3" ]
      , h "div" [ "key" := "2" ] [ t "text 2" ]
      , h "div" [ "key" := "1" ] [ t "text 1" ]
      ]

    removedReversed =
      [ h "div" [ "key" := "5" ] [ t "text 5" ]
      , h "div" [ "key" := "4" ] [ t "text 4" ]
      , h "div" [ "key" := "2" ] [ t "text 2" ]
      , h "div" [ "key" := "1" ] [ t "text 1" ]
      ]

    poppedReversed =
      [ h "div" [ "key" := "5" ] [ t "text 5" ]
      , h "div" [ "key" := "4" ] [ t "text 4" ]
      , h "div" [ "key" := "3" ] [ t "text 3" ]
      , h "div" [ "key" := "2" ] [ t "text 2" ]
      ]

    chaotic =
      [ h "div" [ "key" := "1" ] [ t "text 1" ]
      , h "div" [ "key" := "2" ] [ t "text 2" ]
      , h "div" [ "key" := "5" ] [ t "text 5" ]
      , h "div" [ "key" := "100" ] [ t "text 100" ]
      , h "div" [ "key" := "3" ] [ t "text 3" ]
      , h "div" [ "key" := "99" ] [ t "text 99" ]
      , h "div" [ "key" := "4" ] [ t "text 4" ]
      ]

    assertFor vnodes = do
      clearBody
      mBody <- window >>= document >>= body
      case mBody of
        Nothing -> assert false
        Just body' -> do
          patch Nothing (Just $ root base) $ htmlElementToNode body'
          (firstChild $ htmlElementToNode body') >>= assertElement base
          patch (Just $ root base) (Just $ root vnodes) $ htmlElementToNode body'
          (firstChild $ htmlElementToNode body') >>= assertElement vnodes
      where
        root children = h "div" [] children

        assertElement _ Nothing = assert false
        assertElement vnodes' (Just parent) = for_ (range 0 $ length vnodes' - 1) \i -> do
          nodes <- childNodes parent
          node <- item i nodes
          case node of
            Nothing -> assert false
            Just node' -> do
              mChild <- firstChild node'
              case mChild of
                Nothing -> assert false
                Just child' -> do
                   text <- textContent child'
                   case vnodes' !! i of
                     Just (Element el) ->
                       case head el.children of
                         Just (Text _ text') -> assert $ text == text'
                         _ -> assert false
                     _ -> assert false
