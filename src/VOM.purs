module VOM
  ( VProp
  , VNode
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
import Control.Monad.Eff.Exception (EXCEPTION, catchException)
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
import DOM.Node.Node (childNodes, appendChild, removeChild, replaceChild, setTextContent)
import DOM.Node.NodeList (item)
import DOM.Node.Types (Node, Document, Element, textToNode, elementToNode)
import Data.Array (union, (:), (!!), (..), length, mapWithIndex)
import Data.Foldable (foldl, for_, traverse_)
import Data.Foreign (Foreign, toForeign)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Tuple (Tuple(..), fst, lookup, curry)
import KeyBasedDiff (class HasKey)
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



foreign import setForeign :: forall e. String -> Foreign -> Element -> Eff (dom :: DOM, exception :: EXCEPTION | e) Unit

foreign import removeForeign :: forall e. String -> Element -> Eff (dom :: DOM, exception :: EXCEPTION | e) Unit



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



childAt :: forall e. Int -> Node -> Eff (dom :: DOM | e) (Maybe Node)
childAt index node = childNodes node >>= item index


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
setProp _ (Tuple "key" v) = pure unit
setProp el (Tuple k v) =
  case v of
    Attribute val -> do
      catchException (const $ pure unit) $ setForeign k (toForeign val) el
      setAttribute k val el
    Handler val ->
      catchException (const $ pure unit) $ setForeign k (toForeign $ eventListener val) el



removeProp :: forall e. Element -> String -> Eff (dom :: DOM | e) Unit
removeProp _ "key" = pure unit
removeProp el k = do
  catchException (const $ pure unit) $ removeForeign k el
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
  for_ children
    \vc -> do
      child <- createNode vc
      appendChild child node
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



patch
  :: forall e
   . Maybe (VNode (dom :: DOM | e))
  -> Maybe (VNode (dom :: DOM | e))
  -> Node
  -> Eff (dom :: DOM | e) Unit
patch old new target = patch' old new target 0
  where
    patch' Nothing Nothing _ _ = pure unit

    patch' Nothing (Just next) parent _ = do
      node <- createNode next
      void $ appendChild node parent

    patch' (Just _) Nothing parent index = do
      mNode <- childAt index parent
      maybe (pure unit) (void <<< flip removeChild parent) mNode

    patch' (Just (Text _ prev)) (Just (Text _ next)) parent index =
      when (prev /= next) do
        mNode <- childAt index parent
        maybe (pure unit) (void <<< setTextContent next) mNode

    patch' (Just prev) (Just next) parent index = do
      mNode <- childAt index parent
      case mNode of
        Nothing -> pure unit
        Just node ->
          if (changed prev next) then do
            nextNode <- createNode next
            void $ replaceChild nextNode node parent
          else do
            case prev, next of
              Element { props: prevProps }, Element { props: nextProps } ->
                updateProps prevProps nextProps $ unsafeCoerce node
              _, _ -> pure unit
            startWalk prev next node

    startWalk (Element prev) (Element next) parent = do
      if (prevLength > nextLength)
        then do
          walk (0 .. (nextLength - 1))
          walk ((prevLength - 1) .. nextLength)
        else
          walk (0 .. (nextLength - 1))
      where
        walk = traverse_ (\i -> patch' (prev.children !! i) (next.children !! i) parent i)
        prevLength = length prev.children
        nextLength = length next.children

    startWalk _ _ _ = pure unit



instance hasKeyVNode :: HasKey (VNode e) where
  getKey (Text key _) = fromMaybe "" key
  getKey (Element el) =
    case lookup "key" el.props of
      (Just (Attribute key)) -> key
      _ -> ""
