module Test.Text where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Types (htmlElementToNode)
import DOM.HTML.Window (document)
import DOM.Node.Node (childNodes, firstChild, textContent)
import DOM.Node.NodeList (item)
import Data.Array (length, range, (!!))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Test.Assert (ASSERT, assert)
import Test.TestHelper (clearBody)
import VOM (VNode(..), h, patch, t)



type TextList e = Array (VNode e)



assertDiffAlgorithmForText :: forall e. Eff (dom :: DOM, assert :: ASSERT | e) Unit
assertDiffAlgorithmForText = do
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
  assertFor
    [ t "text 1"
    , t "text 2"
    , t "text 5"
    , t "text 100"
    , t "text 3"
    , t "text 99"
    , t "text 4"
    ]
  where
    base =
      [ t "text 1"
      , t "text 2"
      , t "text 3"
      , t "text 4"
      , t "text 5"
      ]

    reversed =
      [ t "text 5"
      , t "text 4"
      , t "text 3"
      , t "text 2"
      , t "text 1"
      ]

    unshifted =
      [ t "text 8"
      , t "text 0"
      , t "text 1"
      , t "text 2"
      , t "text 3"
      , t "text 4"
      , t "text 5"
      ]

    divided =
      [ t "text 1"
      , t "text 2"
      , t "text 8"
      , t "text 0"
      , t "text 3"
      , t "text 4"
      , t "text 5"
      ]

    pushed =
      [ t "text 1"
      , t "text 2"
      , t "text 3"
      , t "text 4"
      , t "text 5"
      , t "text 6"
      , t "text 7"
      ]

    shifted =
      [ t "text 2"
      , t "text 3"
      , t "text 4"
      , t "text 5"
      ]

    removed =
      [ t "text 1"
      , t "text 2"
      , t "text 4"
      , t "text 5"
      ]

    popped =
      [ t "text 1"
      , t "text 2"
      , t "text 3"
      , t "text 4"
      ]

    unshiftedReversed =
      [ t "text 8"
      , t "text 0"
      , t "text 5"
      , t "text 4"
      , t "text 3"
      , t "text 2"
      , t "text 1"
      ]

    dividedReversed =
      [ t "text 5"
      , t "text 4"
      , t "text 3"
      , t "text 8"
      , t "text 0"
      , t "text 2"
      , t "text 1"
      ]

    pushedReversed =
      [ t "text 5"
      , t "text 4"
      , t "text 3"
      , t "text 2"
      , t "text 1"
      , t "text 0"
      , t "text 8"
      ]

    shiftedReversed =
      [ t "text 4"
      , t "text 3"
      , t "text 2"
      , t "text 1"
      ]

    removedReversed =
      [ t "text 5"
      , t "text 4"
      , t "text 2"
      , t "text 1"
      ]

    poppedReversed =
      [ t "text 5"
      , t "text 4"
      , t "text 3"
      , t "text 2"
      ]

    assertFor vnodes = do
      clearBody
      mBody <- window >>= document >>= body
      case mBody of
        Nothing -> assert false
        Just body' -> do
          patch Nothing (Just $ root base) $ htmlElementToNode body'
          (firstChild $ htmlElementToNode body') >>= assertText base
          patch (Just $ root base) (Just $ root vnodes) $ htmlElementToNode body'
          (firstChild $ htmlElementToNode body') >>= assertText vnodes
      where
        root children = h "div" [] children

        assertText _ Nothing = assert false
        assertText vnodes' (Just parent) = for_ (range 0 $ length vnodes' - 1) \i -> do
          nodes <- childNodes parent
          node <- item i nodes
          case node of
            Nothing -> assert false
            Just n -> do
               text <- textContent n
               case vnodes' !! i of
                 Just (Text _ text') -> assert $ text == text'
                 _ -> assert false
