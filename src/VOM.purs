module VOM
  ( VProp
  , VNode(..)
  , h
  , t
  , attribute
  , style
  , handler
  , stringTo
  , noneTo
  , patch
  , (:=)
  , (:|)
  , (~>)
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Rec.Class (Step(..), tailRecM2)
import Control.Safely as Safe
import DOM (DOM)
import DOM.Event.Event (target)
import DOM.Event.EventTarget (eventListener)
import DOM.Event.Types (Event)
import DOM.HTML (window)
import DOM.HTML.HTMLInputElement (value)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Document (createTextNode, createElement, createElementNS)
import DOM.Node.Element (removeAttribute, setAttribute)
import DOM.Node.Node (appendChild, childNodes, insertBefore, removeChild, replaceChild, setTextContent)
import DOM.Node.NodeList (length, item)
import DOM.Node.Types (Document, Element, Node, NodeList, elementToNode, textToNode)
import Data.Array (union, (:), (!!), mapWithIndex, snoc, singleton)
import Data.Foldable (foldl, for_)
import Data.Foreign (Foreign, toForeign)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Tuple (Tuple(..), fst, lookup, curry)
import KeyBasedDiff (class HasKey, Operation(..), operateDiff)
import Unsafe.Coerce (unsafeCoerce)



data VProp e
  = Attribute String
  | Handler (Event -> Eff e Unit)

data VNode e
  = Element
    { tag :: String
    , props :: Array (Tuple String (VProp e))
    , children :: Array (VNode e)
    }
  | Text (Maybe String) String


foreign import setForeign :: forall e. String -> Foreign -> Element -> Eff (dom :: DOM | e) Unit

foreign import removeForeign :: forall e. String -> Element -> Eff (dom :: DOM | e) Unit



h :: forall e
   . String
  -> Array (Tuple String (VProp e))
  -> Array (VNode e)
  -> VNode e
h tag props children = Element { tag, props, children: children' }
  where
    setKey i (Text Nothing s) = Text (Just $ keyValue i) s
    setKey i (Element el) =
      case lookup "key" el.props of
        Just _ -> Element el
        Nothing -> Element { tag: el.tag, children: el.children, props: ("key" := keyValue i) : el.props }
    setKey _ vnode = vnode

    children' = mapWithIndex setKey children



t :: forall e. String -> VNode e
t = Text Nothing



keyValue :: Int -> String
keyValue i = "_vom_key_" <> show i



attribute :: forall e. String -> String -> Tuple String (VProp e)
attribute key value' = Tuple key (Attribute value')

infixr 1 attribute as :=



style :: forall e. String -> Array (Tuple String String) -> Tuple String (VProp e)
style key styles = attribute key value'
  where
    joint acc (Tuple k v) = acc <> k <> ":" <> v <> ";"
    value' = foldl joint "" styles

infixr 1 style as :|



handler :: forall e. String -> (Event -> Eff e Unit) -> Tuple String (VProp e)
handler key fn = Tuple key (Handler fn)

infixr 1 handler as ~>



stringTo :: forall e. (String -> Eff (dom :: DOM | e) Unit) -> (Event -> Eff (dom :: DOM | e) Unit)
stringTo fn = (\e -> (value $ unsafeCoerce $ target e) >>= fn)



noneTo :: forall e. Eff (dom :: DOM | e) Unit -> (Event -> Eff (dom :: DOM | e) Unit)
noneTo fn = \_ -> fn



doc :: forall e. Eff (dom :: DOM | e) Document
doc = window >>= document >>= htmlDocumentToDocument >>> pure



svgNameSpace :: Maybe String
svgNameSpace = Just "http://www.w3.org/2000/svg"



changed :: forall e. VNode e -> VNode e -> Boolean
changed (Element prev) (Element next) = prev.tag /= next.tag
changed (Text _ prev) (Text _ next) = prev /= next
changed _ _ = true



setProp
  :: forall e
   . Element
  -> Tuple String (VProp (dom :: DOM | e))
  -> Eff (dom :: DOM | e) Unit
setProp _ (Tuple "key" _) = pure unit
setProp el (Tuple k v) =
  case v of
    Attribute val -> do
      setForeign k (toForeign val) el
      setAttribute k val el
    Handler val ->
      setForeign k (toForeign $ eventListener val) el



removeProp :: forall e. Element -> String -> Eff (dom :: DOM | e) Unit
removeProp _ "key" = pure unit
removeProp el k = do
  removeForeign k el
  removeAttribute k el



createNode :: forall e. VNode (dom :: DOM | e) -> Eff (dom :: DOM | e) Node
createNode (Text _ text) =
  doc >>= createTextNode text >>= textToNode >>> pure

createNode (Element { tag, props, children }) = do
  el <- case tag of
    "svg" ->
      doc >>= createElementNS svgNameSpace tag
    _ ->
      doc >>= createElement tag
  for_ props $ setProp el
  let node = elementToNode el
  Safe.for_ children
    \vc -> do
      child <- createNode vc
      void $ appendChild child node
  pure node



updateProps
  :: forall e
   . Array (Tuple String (VProp (dom :: DOM | e)))
  -> Array (Tuple String (VProp (dom :: DOM | e)))
  -> Element
  -> Eff (dom :: DOM | e) Unit
updateProps prevs nexts element =
  for_ keys update
  where
    keys = union (map fst prevs) (map fst nexts)
    update key =
      case lookup key prevs, lookup key nexts of
        Nothing, Nothing -> pure unit
        Just _, Nothing -> removeProp element key
        _, Just next -> curry (setProp element) key next



nodeListToArray
  :: forall e
   . NodeList
  -> Int
  -> Int
  -> Array Node
  -> Eff (dom :: DOM | e) (Array Node)
nodeListToArray nodeList from to accum = tailRecM2 go { nodeList, from, to } accum
  where
    go acc acc2
      | acc.from > acc.to = pure $ Done acc2
      | otherwise = do
          mNode <- item acc.from acc.nodeList
          pure $ Loop { a: acc { from = acc.from + 1 }, b: maybe acc2 (snoc acc2) mNode }



memoizedChildNodes :: forall e. Node -> Eff (dom :: DOM | e) (Array Node)
memoizedChildNodes node = do
  nodeList <- childNodes node
  l <- length nodeList
  nodeListToArray nodeList 0 (l - 1) []



patch'
  :: forall e
   . Array (VNode (dom :: DOM | e))
  -> Array (VNode (dom :: DOM | e))
  -> Array Node
  -> Node
  -> Eff (dom :: DOM | e) Unit
patch' prevs nexts children parent = operateDiff prevs nexts effectBy
  where
    effectBy (Create vnode nextIdx) = do
      node <- createNode vnode
      insertChildAt node nextIdx

    effectBy (Remove prevIdx) =
      maybe (pure unit) (void <<< flip removeChild parent) $ children !! prevIdx

    effectBy (Move prev next prevIdx nextIdx) =
      flip (maybe $ pure unit) (children !! prevIdx) \node -> do
        void $ removeChild node parent
        insertChildAt node nextIdx
        effectBy (Update prev next prevIdx)

    effectBy (Update prev next prevIdx) =
      if changed prev next then
        case prev, next of
          Text _ _, Text _ nextText ->
            maybe (pure unit) (void <<< setTextContent nextText) $ children !! prevIdx

          _, _ ->
            flip (maybe $ pure unit) (children !! prevIdx) \node -> do
              nextNode <- createNode next
              void $ replaceChild nextNode node parent
      else
        case prev, next of
          Element pEl, Element nEl ->
            flip (maybe $ pure unit) (children !! prevIdx) \node -> do
              updateProps pEl.props nEl.props $ unsafeCoerce node
              children' <- memoizedChildNodes node
              patch' pEl.children nEl.children children' node

          _, _ -> pure unit

    insertChildAt child idx = do
      nodeList' <- childNodes parent
      targetIdxNode <- item idx nodeList'
      void $ case targetIdxNode of
        Nothing -> appendChild child parent
        Just node -> insertBefore child node parent



patch
  :: forall e
   . Maybe (VNode (dom :: DOM | e))
  -> Maybe (VNode (dom :: DOM | e))
  -> Node
  -> Eff (dom :: DOM | e) Unit
patch prev next parent = do
  children <- memoizedChildNodes parent
  patch' prevs nexts children parent
  where
    prevs = maybe [] singleton prev
    nexts = maybe [] singleton next



instance hasKeyVNode :: HasKey (VNode e) where
  getKey (Text key _) = fromMaybe "" key
  getKey (Element el) =
    case lookup "key" el.props of
      (Just (Attribute key)) -> key
      _ -> ""
