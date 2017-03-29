module VOM
  ( VProp
  , VNode
  , h
  , t
  , attr
  , handler
  , stringTo
  , noneTo
  , style
  , patch
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Array (union, (!!), (..), length)
import Data.Foldable (foldl, for_, traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Tuple (Tuple(..), fst, lookup, curry)
import DOM (DOM)
import DOM.Event.Event (target)
import DOM.Event.EventTarget (EventListener, eventListener)
import DOM.Event.Types (Event)
import DOM.HTML (window)
import DOM.HTML.HTMLInputElement (value)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Document (createTextNode, createElement, createElementNS)
import DOM.Node.Element (setAttribute, removeAttribute)
import DOM.Node.Node (childNodes, appendChild, removeChild, replaceChild, setTextContent)
import DOM.Node.NodeList (item)
import DOM.Node.Types (Node, Document, Element, textToNode, elementToNode)
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
  | Text String



foreign import setAttribute' :: forall e. String ->
                                EventListener (dom :: DOM | e) ->
                                Element ->
                                Eff (dom :: DOM | e) Unit

foreign import removeAttribute' :: forall e. String ->
                                  Element ->
                                  Eff (dom :: DOM | e) Unit



h :: forall e. String ->
     Array (Tuple String (VProp e)) ->
     Array (VNode e) ->
     VNode e
h tag props children = Element
  { tag
  , props
  , children
  }



t :: forall e. String -> VNode e
t = Text



attr :: forall e. String -> VProp e
attr = Attribute



handler :: forall e. (Event -> Eff e Unit) -> VProp e
handler = Handler



stringTo :: forall e. (String -> Eff (dom :: DOM | e) Unit) -> VProp (dom :: DOM | e)
stringTo f = handler (\e -> (value $ unsafeCoerce $ target e) >>= f)



noneTo :: forall e. Eff (dom :: DOM | e) Unit -> VProp (dom :: DOM | e)
noneTo f = handler (\_ -> f)



style :: forall e. Array (Tuple String String) -> VProp e
style = attr <<< foldl joint ""
  where joint acc (Tuple k v) = acc <> k <> ":" <> v <> ";"



doc :: forall e. Eff (dom :: DOM | e) Document
doc = window >>= document >>= htmlDocumentToDocument >>> pure



childAt :: forall e. Int -> Node -> Eff (dom :: DOM | e) (Maybe Node)
childAt index node = childNodes node >>= item index >>= toMaybe >>> pure


svgNameSpace :: Nullable String
svgNameSpace = toNullable $ Just "http://www.w3.org/2000/svg"



changed :: forall e. VNode e -> VNode e -> Boolean
changed (Element prev) (Element next) = prev.tag /= next.tag
changed (Text prev) (Text next) = prev /= next
changed _ _ = true



setProp :: forall e. Element ->
           Tuple String (VProp (dom :: DOM | e)) ->
           Eff (dom :: DOM | e) Unit
setProp el (Tuple k v) =
  case v of
    (Attribute val) ->
      setAttribute k val el
    (Handler val) ->
      setAttribute' k (eventListener val) el



removeProp :: forall e. Element -> String -> Eff (dom :: DOM | e) Unit
removeProp el k = do
  removeAttribute' k el
  removeAttribute k el



createNode :: forall e. VNode (dom :: DOM | e) -> Eff (dom :: DOM | e) Node
createNode (Text text) =
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



updateProps :: forall e. Array (Tuple String (VProp (dom :: DOM | e))) ->
              Array (Tuple String (VProp (dom :: DOM | e))) ->
              Element ->
              Eff (dom :: DOM | e) Unit
updateProps prevs nexts element =
  for_ keys update
  where
    keys = union (map fst prevs) (map fst nexts)
    update key =
      case lookup key prevs, lookup key nexts of
        Nothing, Nothing -> pure unit
        Just _, Nothing -> removeProp element key
        _, Just next -> curry (setProp element) key next



patch :: forall e. Maybe (VNode (dom :: DOM | e)) ->
         Maybe (VNode (dom :: DOM | e)) ->
         Node ->
         Eff (dom :: DOM | e) Unit
patch old new target = patch' old new target 0
  where
    patch' Nothing Nothing _ _ = pure unit

    patch' Nothing (Just next) parent _ = do
      node <- createNode next
      void $ appendChild node parent

    patch' (Just _) Nothing parent index = do
      mNode <- childAt index parent
      maybe (pure unit) (void <<< flip removeChild parent) mNode

    patch' (Just (Text prev)) (Just (Text next)) parent index =
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
