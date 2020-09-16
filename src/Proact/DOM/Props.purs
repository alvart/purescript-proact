{-
  @license MIT
  Props.purs
-}

module Proact.DOM.Props
where

import CSS.Render (collect)
import CSS.Stylesheet (CSS, Rule(..), runS)
import Data.Array (mapMaybe, concatMap, singleton)
import Data.Foldable (foldMap)
import Data.Function.Uncurried
  ( mkFn1
  , mkFn2
  , mkFn3
  , mkFn4
  , mkFn5
  , mkFn6
  , mkFn7
  , mkFn8
  , mkFn9
  , mkFn10
  )
import Data.Functor.Contravariant (cmap)
import Data.Maybe(Maybe(..))
import Data.Options (Option, opt)
import Data.Tuple (Tuple(..))
import Prelude (($), (<<<), Unit, bind, pure)
import Proact (EventHandler)
import React.SyntheticEvent
  ( SyntheticEvent
  , SyntheticAnimationEvent
  , SyntheticClipboardEvent
  , SyntheticCompositionEvent
  , SyntheticInputEvent
  , SyntheticKeyboardEvent
  , SyntheticFocusEvent
  , SyntheticMouseEvent
  , SyntheticTouchEvent
  , SyntheticTransitionEvent
  , SyntheticUIEvent
  , SyntheticWheelEvent
  )

-- | Represents the properties of a React Element.
data Properties s (e :: # Type)

-- | Builds an option from an Event Handler that requires one argument.
mkEventOpt
  :: forall arg s e
   . String -> Option (Properties s e) (arg -> EventHandler s e Unit)
mkEventOpt = cmap mkFn1 <<< opt

-- | Builds an option from an Event Handler that requires two arguments.
mkEventOpt2
  :: forall arg1 arg2 s e
   . String -> Option (Properties s e) (arg1 -> arg2 -> EventHandler s e Unit)
mkEventOpt2 = cmap mkFn2 <<< opt

-- | Builds an option from an Event Handler that requires three arguments.
mkEventOpt3
  :: forall arg1 arg2 arg3 s e
   . String
  -> Option (Properties s e) (arg1 -> arg2 -> arg3 -> EventHandler s e Unit)
mkEventOpt3 = cmap mkFn3 <<< opt

-- | Builds an option from an Event Handler that requires four arguments.
mkEventOpt4
  :: forall arg1 arg2 arg3 arg4 s e
   . String
  -> Option
      (Properties s e)
      (arg1 -> arg2 -> arg3 -> arg4 -> EventHandler s e Unit)
mkEventOpt4 = cmap mkFn4 <<< opt

-- | Builds an option from an Event Handler that requires five arguments.
mkEventOpt5
  :: forall arg1 arg2 arg3 arg4 arg5 s e
   . String
  -> Option
      (Properties s e)
      (arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> EventHandler s e Unit)
mkEventOpt5 = cmap mkFn5 <<< opt

-- | Builds an option from an Event Handler that requires six arguments.
mkEventOpt6
  :: forall arg1 arg2 arg3 arg4 arg5 arg6 s e
   . String
  -> Option
      (Properties s e)
      (arg1 -> arg2 -> arg3 -> arg4 -> arg5 -> arg6 -> EventHandler s e Unit)
mkEventOpt6 = cmap mkFn6 <<< opt

-- | Builds an option from an Event Handler that requires seven arguments.
mkEventOpt7
  :: forall arg1 arg2 arg3 arg4 arg5 arg6 arg7 s e
   . String
  -> Option
      (Properties s e)
       ( arg1
      -> arg2
      -> arg3
      -> arg4
      -> arg5
      -> arg6
      -> arg7
      -> EventHandler s e Unit
       )
mkEventOpt7 = cmap mkFn7 <<< opt

-- | Builds an option from an Event Handler that requires eight arguments.
mkEventOpt8
  :: forall arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 s e
   . String
  -> Option
      (Properties s e)
       ( arg1
      -> arg2
      -> arg3
      -> arg4
      -> arg5
      -> arg6
      -> arg7
      -> arg8
      -> EventHandler s e Unit
       )
mkEventOpt8 = cmap mkFn8 <<< opt

-- | Builds an option from an Event Handler that requires nine arguments.
mkEventOpt9
  :: forall arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 s e
   . String
  -> Option
      (Properties s e)
       ( arg1
      -> arg2
      -> arg3
      -> arg4
      -> arg5
      -> arg6
      -> arg7
      -> arg8
      -> arg9
      -> EventHandler s e Unit
       )
mkEventOpt9 = cmap mkFn9 <<< opt

-- | Builds an option from an Event Handler that requires ten arguments.
mkEventOpt10
  :: forall arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 s e
   . String
  -> Option
      (Properties s e)
       ( arg1
      -> arg2
      -> arg3
      -> arg4
      -> arg5
      -> arg6
      -> arg7
      -> arg8
      -> arg9
      -> arg10
      -> EventHandler s e Unit
       )
mkEventOpt10 = cmap mkFn10 <<< opt

-- | Creates an option for the `style` property.
style :: forall s e . Option (Properties s e) CSS
style = cmap renderInline $ opt "_style"
  where
  prop (Property k v) = Just $ Tuple k v
  prop _ = Nothing

  renderInline :: CSS -> Array (Array String)
  renderInline css =
    do
    rule <- mapMaybe prop $ runS css
    Tuple k v <- concatMap (foldMap singleton) $ collect rule
    pure [ k, v ]

-- | Creates an option for the `accept` property.
accept :: forall s e . Option (Properties s e) String
accept = opt "accept"

-- | Creates an option for the `acceptCharset` property.
acceptCharset :: forall s e . Option (Properties s e) String
acceptCharset = opt "acceptCharset"

-- | Creates an option for the `accessKey` property.
accessKey :: forall s e . Option (Properties s e) String
accessKey = opt "accessKey"

-- | Creates an option for the `action` property.
action :: forall s e . Option (Properties s e) String
action = opt "action"

-- | Creates an option for the `allowFullScreen` property.
allowFullScreen :: forall s e . Option (Properties s e) Boolean
allowFullScreen = opt "allowFullScreen"

-- | Creates an option for the `allowTransparency` property.
allowTransparency :: forall s e . Option (Properties s e) Boolean
allowTransparency = opt "allowTransparency"

-- | Creates an option for the `alt` property.
alt :: forall s e . Option (Properties s e) String
alt = opt "alt"

-- | Creates an option for the `async` property.
async :: forall s e . Option (Properties s e) Boolean
async = opt "async"

-- | Creates an option for the `autoComplete` property.
autoComplete :: forall s e . Option (Properties s e) String
autoComplete = opt "autoComplete"

-- | Creates an option for the `autoFocus` property.
autoFocus :: forall s e . Option (Properties s e) Boolean
autoFocus = opt "autoFocus"

-- | Creates an option for the `autoPlay` property.
autoPlay :: forall s e . Option (Properties s e) Boolean
autoPlay = opt "autoPlay"

-- | Creates an option for the `capture` property.
capture :: forall s e . Option (Properties s e) Boolean
capture = opt "capture"

-- | Creates an option for the `cellPadding` property.
cellPadding :: forall s e . Option (Properties s e) String
cellPadding = opt "cellPadding"

-- | Creates an option for the `cellSpacing` property.
cellSpacing :: forall s e . Option (Properties s e) String
cellSpacing = opt "cellSpacing"

-- | Creates an option for the `charSet` property.
charSet :: forall s e . Option (Properties s e) String
charSet = opt "charSet"

-- | Creates an option for the `challenge` property.
challenge :: forall s e . Option (Properties s e) String
challenge = opt "checked"

-- | Creates an option for the `checked` property.
checked :: forall s e . Option (Properties s e) Boolean
checked = opt "checked"

-- | Creates an option for the `cite` property.
cite :: forall s e . Option (Properties s e) String
cite = opt "cite"

-- | Creates an option for the `classID` property.
classID :: forall s e . Option (Properties s e) String
classID = opt "classID"

-- | Creates an option for the `className` property.
className :: forall s e . Option (Properties s e) String
className = opt "className"

-- | Creates an option for the `cols` property.
cols :: forall s e . Option (Properties s e) Int
cols = opt "cols"

-- | Creates an option for the `colSpan` property.
colSpan :: forall s e . Option (Properties s e) Int
colSpan = opt "colSpan"

-- | Creates an option for the `content` property.
content :: forall s e . Option (Properties s e) String
content = opt "content"

-- | Creates an option for the `contentEditable` property.
contentEditable :: forall s e . Option (Properties s e) Boolean
contentEditable = opt "contentEditable"

-- | Creates an option for the `contextMenu` property.
contextMenu :: forall s e . Option (Properties s e) String
contextMenu = opt "contextMenu"

-- | Creates an option for the `controls` property.
controls :: forall s e . Option (Properties s e) Boolean
controls = opt "controls"

-- | Creates an option for the `coords` property.
coords :: forall s e . Option (Properties s e) String
coords = opt "coords"

-- | Creates an option for the `crossOrigin` property.
crossOrigin :: forall s e . Option (Properties s e) String
crossOrigin = opt "crossOrigin"

-- | Creates an option for the `dateTime` property.
dateTime :: forall s e . Option (Properties s e) String
dateTime = opt "dateTime"

-- | Creates an option for the `default` property.
default :: forall s e . Option (Properties s e) Boolean
default = opt "default"

-- | Creates an option for the `defaultChecked` property.
defaultChecked :: forall s e . Option (Properties s e) Boolean
defaultChecked = opt "defaultChecked"

-- | Creates an option for the `defaultValue` property.
defaultValue :: forall s e . Option (Properties s e) String
defaultValue = opt "defaultValue"

-- | Creates an option for the `defer` property.
defer :: forall s e . Option (Properties s e) Boolean
defer = opt "defer"

-- | Creates an option for the `dir` property.
dir :: forall s e . Option (Properties s e) String
dir = opt "dir"

-- | Creates an option for the `disabled` property.
disabled :: forall s e . Option (Properties s e) Boolean
disabled = opt "disabled"

-- | Creates an option for the `download` property.
download :: forall s e . Option (Properties s e) String
download = opt "download"

-- | Creates an option for the `draggable` property.
draggable :: forall s e . Option (Properties s e) Boolean
draggable = opt "draggable"

-- | Creates an option for the `encType` property.
encType :: forall s e . Option (Properties s e) String
encType = opt "encType"

-- | Creates an option for the `form` property.
form :: forall s e . Option (Properties s e) String
form = opt "form"

-- | Creates an option for the `formAction` property.
formAction :: forall s e . Option (Properties s e) String
formAction = opt "formAction"

-- | Creates an option for the `formEncType` property.
formEncType :: forall s e . Option (Properties s e) String
formEncType = opt "formEncType"

-- | Creates an option for the `formMethod` property.
formMethod :: forall s e . Option (Properties s e) String
formMethod = opt "formMethod"

-- | Creates an option for the `formNoValidate` property.
formNoValidate :: forall s e . Option (Properties s e) Boolean
formNoValidate = opt "formNoValidate"

-- | Creates an option for the `formTarget` property.
formTarget :: forall s e . Option (Properties s e) String
formTarget = opt "formTarget"

-- | Creates an option for the `frameBorder` property.
frameBorder :: forall s e . Option (Properties s e) String
frameBorder = opt "frameBorder"

-- | Creates an option for the `headers` property.
headers :: forall s e . Option (Properties s e) String
headers = opt "headers"

-- | Creates an option for the `height` property.
height :: forall s e . Option (Properties s e) String
height = opt "height"

-- | Creates an option for the `hidden` property.
hidden :: forall s e . Option (Properties s e) Boolean
hidden = opt "hidden"

-- | Creates an option for the `high` property.
high :: forall s e . Option (Properties s e) String
high = opt "high"

-- | Creates an option for the `href` property.
href :: forall s e . Option (Properties s e) String
href = opt "href"

-- | Creates an option for the `hrefLang` property.
hrefLang :: forall s e . Option (Properties s e) String
hrefLang = opt "hrefLang"

-- | Creates an option for the `htmlFor` property.
htmlFor :: forall s e . Option (Properties s e) String
htmlFor = opt "htmlFor"

-- | Creates an option for the `httpEquiv` property.
httpEquiv :: forall s e . Option (Properties s e) String
httpEquiv = opt "httpEquiv"

-- | Creates an option for the `icon` property.
icon :: forall s e . Option (Properties s e) String
icon = opt "icon"

-- | Creates an option for the `_id` property.
_id :: forall s e . Option (Properties s e) String
_id = opt "id"

-- | Creates an option for the `inputMode` property.
inputMode :: forall s e . Option (Properties s e) String
inputMode = opt "inputMode"

-- | Creates an option for the `integrity` property.
integrity :: forall s e . Option (Properties s e) String
integrity = opt "integrity"

-- | Creates an option for the `is` property.
is :: forall s e . Option (Properties s e) String
is = opt "is"

-- | Creates an option for the `key` property.
key :: forall s e . Option (Properties s e) String
key = opt "key"

-- | Creates an option for the `keyParams` property.
keyParams :: forall s e . Option (Properties s e) String
keyParams = opt "keyParams"

-- | Creates an option for the `keyType` property.
keyType :: forall s e . Option (Properties s e) String
keyType = opt "keyType"

-- | Creates an option for the `kind` property.
kind :: forall s e . Option (Properties s e) String
kind = opt "kind"

-- | Creates an option for the `label` property.
label :: forall s e . Option (Properties s e) String
label = opt "label"

-- | Creates an option for the `lang` property.
lang :: forall s e . Option (Properties s e) String
lang = opt "lang"

-- | Creates an option for the `list` property.
list :: forall s e . Option (Properties s e) String
list = opt "list"

-- | Creates an option for the `loop` property.
loop :: forall s e . Option (Properties s e) Boolean
loop = opt "loop"

-- | Creates an option for the `low` property.
low :: forall s e . Option (Properties s e) String
low = opt "low"

-- | Creates an option for the `manifest` property.
manifest :: forall s e . Option (Properties s e) String
manifest = opt "manifest"

-- | Creates an option for the `marginHeight` property.
marginHeight :: forall s e . Option (Properties s e) String
marginHeight = opt "marginHeight"

-- | Creates an option for the `marginWidth` property.
marginWidth :: forall s e . Option (Properties s e) String
marginWidth = opt "marginWidth"

-- | Creates an option for the `max` property.
max :: forall s e . Option (Properties s e) String
max = opt "max"

-- | Creates an option for the `maxLength` property.
maxLength :: forall s e . Option (Properties s e) String
maxLength = opt "maxLength"

-- | Creates an option for the `media` property.
media :: forall s e . Option (Properties s e) String
media = opt "media"

-- | Creates an option for the `mediaGroup` property.
mediaGroup :: forall s e . Option (Properties s e) String
mediaGroup = opt "mediaGroup"

-- | Creates an option for the `method` property.
method :: forall s e . Option (Properties s e) String
method = opt "method"

-- | Creates an option for the `min` property.
min :: forall s e . Option (Properties s e) String
min = opt "min"

-- | Creates an option for the `minLength` property.
minLength :: forall s e . Option (Properties s e) String
minLength = opt "minLength"

-- | Creates an option for the `multiple` property.
multiple :: forall s e . Option (Properties s e) Boolean
multiple = opt "multiple"

-- | Creates an option for the `muted` property.
muted :: forall s e . Option (Properties s e) Boolean
muted = opt "muted"

-- | Creates an option for the `name` property.
name :: forall s e . Option (Properties s e) String
name = opt "name"

-- | Creates an option for the `nonce` property.
nonce :: forall s e . Option (Properties s e) String
nonce = opt "nonce"

-- | Creates an option for the `noValidate` property.
noValidate :: forall s e . Option (Properties s e) Boolean
noValidate = opt "noValidate"

-- | Creates an option for the `open` property.
open :: forall s e . Option (Properties s e) Boolean
open = opt "open"

-- | Creates an option for the `optimum` property.
optimum :: forall s e . Option (Properties s e) String
optimum = opt "optimum"

-- | Creates an option for the `pattern` property.
pattern :: forall s e . Option (Properties s e) String
pattern = opt "pattern"

-- | Creates an option for the `placeholder` property.
placeholder :: forall s e . Option (Properties s e) String
placeholder = opt "placeholder"

-- | Creates an option for the `poster` property.
poster :: forall s e . Option (Properties s e) String
poster = opt "poster"

-- | Creates an option for the `preload` property.
preload :: forall s e . Option (Properties s e) String
preload = opt "preload"

-- | Creates an option for the `profile` property.
profile :: forall s e . Option (Properties s e) String
profile = opt "profile"

-- | Creates an option for the `radioGroup` property.
radioGroup :: forall s e . Option (Properties s e) String
radioGroup = opt "radioGroup"

-- | Creates an option for the `readOnly` property.
readOnly :: forall s e . Option (Properties s e) Boolean
readOnly = opt "readOnly"

-- | Creates an option for the `rel` property.
rel :: forall s e . Option (Properties s e) String
rel = opt "rel"

-- | Creates an option for the `required` property.
required :: forall s e . Option (Properties s e) Boolean
required = opt "required"

-- | Creates an option for the `reversed` property.
reversed :: forall s e . Option (Properties s e) Boolean
reversed = opt "reversed"

-- | Creates an option for the `role` property.
role :: forall s e . Option (Properties s e) String
role = opt "role"

-- | Creates an option for the `rows` property.
rows :: forall s e . Option (Properties s e) Int
rows = opt "rows"

-- | Creates an option for the `rowSpan` property.
rowSpan :: forall s e . Option (Properties s e) Int
rowSpan = opt "rowSpan"

-- | Creates an option for the `sandbox` property.
sandbox :: forall s e . Option (Properties s e) String
sandbox = opt "sandbox"

-- | Creates an option for the `scope` property.
scope :: forall s e . Option (Properties s e) String
scope = opt "scope"

-- | Creates an option for the `scoped` property.
scoped :: forall s e . Option (Properties s e) Boolean
scoped = opt "scoped"

-- | Creates an option for the `scrolling` property.
scrolling :: forall s e . Option (Properties s e) String
scrolling = opt "scrolling"

-- | Creates an option for the `seamless` property.
seamless :: forall s e . Option (Properties s e) Boolean
seamless = opt "seamless"

-- | Creates an option for the `selected` property.
selected :: forall s e . Option (Properties s e) Boolean
selected = opt "selected"

-- | Creates an option for the `shape` property.
shape :: forall s e . Option (Properties s e) String
shape = opt "shape"

-- | Creates an option for the `size` property.
size :: forall s e . Option (Properties s e) Int
size = opt "size"

-- | Creates an option for the `sizes` property.
sizes :: forall s e . Option (Properties s e) String
sizes = opt "sizes"

-- | Creates an option for the `span` property.
span :: forall s e . Option (Properties s e) Int
span = opt "span"

-- | Creates an option for the `spellCheck` property.
spellCheck :: forall s e . Option (Properties s e) Boolean
spellCheck = opt "spellCheck"

-- | Creates an option for the `src` property.
src :: forall s e . Option (Properties s e) String
src = opt "src"

-- | Creates an option for the `srcDoc` property.
srcDoc :: forall s e . Option (Properties s e) String
srcDoc = opt "srcDoc"

-- | Creates an option for the `srcLang` property.
srcLang :: forall s e . Option (Properties s e) String
srcLang = opt "srcLang"

-- | Creates an option for the `srcSet` property.
srcSet :: forall s e . Option (Properties s e) String
srcSet = opt "srcSet"

-- | Creates an option for the `start` property.
start :: forall s e . Option (Properties s e) Int
start = opt "start"

-- | Creates an option for the `step` property.
step :: forall s e . Option (Properties s e) String
step = opt "step"

-- | Creates an option for the `summary` property.
summary :: forall s e . Option (Properties s e) String
summary = opt "summary"

-- | Creates an option for the `tabIndex` property.
tabIndex :: forall s e . Option (Properties s e) Int
tabIndex = opt "tabIndex"

-- | Creates an option for the `target` property.
target :: forall s e . Option (Properties s e) String
target = opt "target"

-- | Creates an option for the `title` property.
title :: forall s e . Option (Properties s e) String
title = opt "title"

-- | Creates an option for the `_type` property.
_type :: forall s e . Option (Properties s e) String
_type = opt "type"

-- | Creates an option for the `useMap` property.
useMap :: forall s e . Option (Properties s e) String
useMap = opt "useMap"

-- | Creates an option for the `value` property.
value :: forall s e . Option (Properties s e) String
value = opt "value"

-- | Creates an option for the `valueArray` property.
valueArray :: forall s e . Option (Properties s e) (Array String)
valueArray = opt "value"

-- | Creates an option for the `width` property.
width :: forall s e . Option (Properties s e) String
width = opt "width"

-- | Creates an option for the `wmode` property.
wmode :: forall s e . Option (Properties s e) String
wmode = opt "wmode"

-- | Creates an option for the `wrap` property.
wrap :: forall s e . Option (Properties s e) String
wrap = opt "wrap"

-- RDFa Attributes

-- | Creates an option for the `about` property.
about :: forall s e . Option (Properties s e) String
about = opt "about"

-- | Creates an option for the `datatype` property.
datatype :: forall s e . Option (Properties s e) String
datatype = opt "datatype"

-- | Creates an option for the `inlist` property.
inlist :: forall s e . Option (Properties s e) String
inlist = opt "inlist"

-- | Creates an option for the `prefix` property.
prefix :: forall s e . Option (Properties s e) String
prefix = opt "prefix"

-- | Creates an option for the `property` property.
property :: forall s e . Option (Properties s e) String
property = opt "property"

-- | Creates an option for the `resource` property.
resource :: forall s e . Option (Properties s e) String
resource = opt "resource"

-- | Creates an option for the `typeof` property.
typeof :: forall s e . Option (Properties s e) String
typeof = opt "typeof"

-- | Creates an option for the `vocab` property.
vocab :: forall s e . Option (Properties s e) String
vocab = opt "vocab"

-- Non-standard Attributes

-- | Creates an option for the `autoCapitalize` property.
autoCapitalize :: forall s e . Option (Properties s e) String
autoCapitalize = opt "autoCapitalize"

-- | Creates an option for the `autoCorrect` property.
autoCorrect :: forall s e . Option (Properties s e) String
autoCorrect = opt "autoCorrect"

-- | Creates an option for the `autoSave` property.
autoSave :: forall s e . Option (Properties s e) String
autoSave = opt "autoSave"

-- | Creates an option for the `color` property.
color :: forall s e . Option (Properties s e) String
color = opt "color"

-- | Creates an option for the `itemProp` property.
itemProp :: forall s e . Option (Properties s e) String
itemProp = opt "itemProp"

-- | Creates an option for the `itemScope` property.
itemScope :: forall s e . Option (Properties s e) Boolean
itemScope = opt "itemScope"

-- | Creates an option for the `itemType` property.
itemType :: forall s e . Option (Properties s e) String
itemType = opt "itemType"

-- | Creates an option for the `itemID` property.
itemID :: forall s e . Option (Properties s e) String
itemID = opt "itemID"

-- | Creates an option for the `itemRef` property.
itemRef :: forall s e . Option (Properties s e) String
itemRef = opt "itemRef"

-- | Creates an option for the `results` property.
results :: forall s e . Option (Properties s e) Int
results = opt "results"

-- | Creates an option for the `security` property.
security :: forall s e . Option (Properties s e) String
security = opt "security"

-- | Creates an option for the `unselectable` property.
unselectable :: forall s e . Option (Properties s e) Boolean
unselectable = opt "unselectable"

-- Events

-- | Creates an option for the `onAnimationStart` event.
onAnimationStart
  :: forall s e
   . Option (Properties s e) (SyntheticAnimationEvent -> EventHandler s e Unit)
onAnimationStart = mkEventOpt "_onAnimationStart"

-- | Creates an option for the `onAnimationEnd` event.
onAnimationEnd
  :: forall s e
   . Option (Properties s e) (SyntheticAnimationEvent -> EventHandler s e Unit)
onAnimationEnd = mkEventOpt "_onAnimationEnd"

-- | Creates an option for the `onAnimationIteration` event.
onAnimationIteration
  :: forall s e
   . Option (Properties s e) (SyntheticAnimationEvent -> EventHandler s e Unit)
onAnimationIteration = mkEventOpt "_onAnimationIteration"

-- | Creates an option for the `onTransitionEnd` event.
onTransitionEnd
  :: forall s e
   . Option
      (Properties s e)
      (SyntheticTransitionEvent -> EventHandler s e Unit)
onTransitionEnd = mkEventOpt "_onTransitionEnd"

-- | Creates an option for the `onToggle` event.
onToggle
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onToggle = mkEventOpt "_onToggle"

-- | Creates an option for the `onError` event.
onError
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onError = mkEventOpt "_onError"

-- | Creates an option for the `onLoad` event.
onLoad
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onLoad = mkEventOpt "_onLoad"

-- | Creates an option for the `onAbort` event.
onAbort
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onAbort = mkEventOpt "_onAbort"

-- | Creates an option for the `onCanPlay` event.
onCanPlay
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onCanPlay = mkEventOpt "_onCanPlay"

-- | Creates an option for the `onCanPlayThrough` event.
onCanPlayThrough
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onCanPlayThrough = mkEventOpt "_onCanPlayThrough"

-- | Creates an option for the `onDurationChange` event.
onDurationChange
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onDurationChange = mkEventOpt "_onDurationChange"

-- | Creates an option for the `onEmptied` event.
onEmptied
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onEmptied = mkEventOpt "_onEmptied"

-- | Creates an option for the `onEncrypted` event.
onEncrypted
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onEncrypted = mkEventOpt "_onEncrypted"

-- | Creates an option for the `onEnded` event.
onEnded
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onEnded = mkEventOpt "_onEnded"

-- | Creates an option for the `onLoadedData` event.
onLoadedData
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onLoadedData = mkEventOpt "_onLoadedData"

-- | Creates an option for the `onLoadedMetadata` event.
onLoadedMetadata
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onLoadedMetadata = mkEventOpt "_onLoadedMetadata"

-- | Creates an option for the `onLoadStart` event.
onLoadStart
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onLoadStart = mkEventOpt "_onLoadStart"

-- | Creates an option for the `onPause` event.
onPause
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onPause = mkEventOpt "_onPause"

-- | Creates an option for the `onPlay` event.
onPlay
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onPlay = mkEventOpt "_onPlay"

-- | Creates an option for the `onPlaying` event.
onPlaying
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onPlaying = mkEventOpt "_onPlaying"

-- | Creates an option for the `onProgress` event.
onProgress
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onProgress = mkEventOpt "_onProgress"

-- | Creates an option for the `onRateChange` event.
onRateChange
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onRateChange = mkEventOpt "_onRateChange"

-- | Creates an option for the `onSeeked` event.
onSeeked
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onSeeked = mkEventOpt "_onSeeked"

-- | Creates an option for the `onSeeking` event.
onSeeking
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onSeeking = mkEventOpt "_onSeeking"

-- | Creates an option for the `onStalled` event.
onStalled
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onStalled = mkEventOpt "_onStalled"

-- | Creates an option for the `onSuspend` event.
onSuspend
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onSuspend = mkEventOpt "_onSuspend"

-- | Creates an option for the `onTimeUpdate` event.
onTimeUpdate
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onTimeUpdate = mkEventOpt "_onTimeUpdate"

-- | Creates an option for the `onVolumeChange` event.
onVolumeChange
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onVolumeChange = mkEventOpt "_onVolumeChange"

-- | Creates an option for the `onWaiting` event.
onWaiting
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onWaiting = mkEventOpt "_onWaiting"

-- | Creates an option for the `onCopy` event.
onCopy
  :: forall s e
   . Option (Properties s e) (SyntheticClipboardEvent -> EventHandler s e Unit)
onCopy = mkEventOpt "_onCopy"

-- | Creates an option for the `onCut` event.
onCut
  :: forall s e
   . Option (Properties s e) (SyntheticClipboardEvent -> EventHandler s e Unit)
onCut = mkEventOpt "_onCut"

-- | Creates an option for the `onPaste` event.
onPaste
  :: forall s e
   . Option (Properties s e) (SyntheticClipboardEvent -> EventHandler s e Unit)
onPaste = mkEventOpt "_onPaste"

-- | Creates an option for the `onCompositionEnd` event.
onCompositionEnd
  :: forall s e
   . Option
      (Properties s e)
      (SyntheticCompositionEvent -> EventHandler s e Unit)
onCompositionEnd = mkEventOpt "_onCompositionEnd"

-- | Creates an option for the `onCompositionStart` event.
onCompositionStart
  :: forall s e
   . Option
      (Properties s e)
      (SyntheticCompositionEvent -> EventHandler s e Unit)
onCompositionStart = mkEventOpt "_onCompositionStart"

-- | Creates an option for the `onCompositionUpdate` event.
onCompositionUpdate
  :: forall s e
   . Option
      (Properties s e)
      (SyntheticCompositionEvent -> EventHandler s e Unit)
onCompositionUpdate = mkEventOpt "_onCompositionUpdate"

-- | Creates an option for the `onKeyDown` event.
onKeyDown
  :: forall s e
   . Option (Properties s e) (SyntheticKeyboardEvent -> EventHandler s e Unit)
onKeyDown = mkEventOpt "_onKeyDown"

-- | Creates an option for the `onKeyPress` event.
onKeyPress
  :: forall s e
   . Option (Properties s e) (SyntheticKeyboardEvent -> EventHandler s e Unit)
onKeyPress = mkEventOpt "_onKeyPress"

-- | Creates an option for the `onKeyUp` event.
onKeyUp
  :: forall s e
   . Option (Properties s e) (SyntheticKeyboardEvent -> EventHandler s e Unit)
onKeyUp = mkEventOpt "_onKeyUp"

-- | Creates an option for the `onFocus` event.
onFocus
  :: forall s e
   . Option (Properties s e) (SyntheticFocusEvent -> EventHandler s e Unit)
onFocus = mkEventOpt "_onFocus"

-- | Creates an option for the `onBlur` event.
onBlur
  :: forall s e
   . Option (Properties s e) (SyntheticFocusEvent -> EventHandler s e Unit)
onBlur = mkEventOpt "_onBlur"

-- | Creates an option for the `onChange` event.
onChange
  :: forall s e
   . Option (Properties s e) (SyntheticInputEvent -> EventHandler s e Unit)
onChange = mkEventOpt "_onChange"

-- | Creates an option for the `onInput` event.
onInput
  :: forall s e
   . Option (Properties s e) (SyntheticInputEvent -> EventHandler s e Unit)
onInput = mkEventOpt "_onInput"

-- | Creates an option for the `onInvalid` event.
onInvalid
  :: forall s e
   . Option (Properties s e) (SyntheticInputEvent -> EventHandler s e Unit)
onInvalid = mkEventOpt "_onInvalid"

-- | Creates an option for the `onSubmit` event.
onSubmit
  :: forall s e
   . Option (Properties s e) (SyntheticInputEvent -> EventHandler s e Unit)
onSubmit = mkEventOpt "_onSubmit"

-- | Creates an option for the `onClick` event.
onClick
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onClick = mkEventOpt "_onClick"

-- | Creates an option for the `onContextMenu` event.
onContextMenu
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onContextMenu = mkEventOpt "_onContextMenu"

-- | Creates an option for the `onDoubleClick` event.
onDoubleClick
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onDoubleClick = mkEventOpt "_onDoubleClick"

-- | Creates an option for the `onDrag` event.
onDrag
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onDrag = mkEventOpt "_onDrag"

-- | Creates an option for the `onDragEnd` event.
onDragEnd
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onDragEnd = mkEventOpt "_onDragEnd"

-- | Creates an option for the `onDragEnter` event.
onDragEnter
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onDragEnter = mkEventOpt "_onDragEnter"

-- | Creates an option for the `onDragExit` event.
onDragExit
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onDragExit = mkEventOpt "_onDragExit"

-- | Creates an option for the `onDragLeave` event.
onDragLeave
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onDragLeave = mkEventOpt "_onDragLeave"

-- | Creates an option for the `onDragOver` event.
onDragOver
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onDragOver = mkEventOpt "_onDragOver"

-- | Creates an option for the `onDragStart` event.
onDragStart
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onDragStart = mkEventOpt "_onDragStart"

-- | Creates an option for the `onDrop` event.
onDrop
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onDrop = mkEventOpt "_onDrop"

-- | Creates an option for the `onMouseDown` event.
onMouseDown
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseDown = mkEventOpt "_onMouseDown"

-- | Creates an option for the `onMouseEnter` event.
onMouseEnter
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseEnter = mkEventOpt "_onMouseEnter"

-- | Creates an option for the `onMouseLeave` event.
onMouseLeave
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseLeave = mkEventOpt "_onMouseLeave"

-- | Creates an option for the `onMouseMove` event.
onMouseMove
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseMove = mkEventOpt "_onMouseMove"

-- | Creates an option for the `onMouseOut` event.
onMouseOut
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseOut = mkEventOpt "_onMouseOut"

-- | Creates an option for the `onMouseOver` event.
onMouseOver
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseOver = mkEventOpt "_onMouseOver"

-- | Creates an option for the `onMouseUp` event.
onMouseUp
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseUp = mkEventOpt "_onMouseUp"

-- | Creates an option for the `onSelect` event.
onSelect
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onSelect = mkEventOpt "_onSelect"

-- | Creates an option for the `onTouchCancel` event.
onTouchCancel
  :: forall s e
   . Option (Properties s e) (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchCancel = mkEventOpt "_onTouchCancel"

-- | Creates an option for the `onTouchEnd` event.
onTouchEnd
  :: forall s e
   . Option (Properties s e) (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchEnd = mkEventOpt "_onTouchEnd"

-- | Creates an option for the `onTouchMove` event.
onTouchMove
  :: forall s e
   . Option (Properties s e) (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchMove = mkEventOpt "_onTouchMove"

-- | Creates an option for the `onTouchStart` event.
onTouchStart
  :: forall s e
   . Option (Properties s e) (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchStart = mkEventOpt "_onTouchStart"

-- | Creates an option for the `onScroll` event.
onScroll
  :: forall s e
   . Option (Properties s e) (SyntheticUIEvent -> EventHandler s e Unit)
onScroll = mkEventOpt "_onScroll"

-- | Creates an option for the `onWheel` event.
onWheel
  :: forall s e
   . Option (Properties s e) (SyntheticWheelEvent -> EventHandler s e Unit)
onWheel = mkEventOpt "_onWheel"

-- | Creates an option for the `onAnimationStartCapture` event.
onAnimationStartCapture
  :: forall s e
   . Option (Properties s e) (SyntheticAnimationEvent -> EventHandler s e Unit)
onAnimationStartCapture = mkEventOpt "_onAnimationStartCapture"

-- | Creates an option for the `onAnimationEndCapture` event.
onAnimationEndCapture
  :: forall s e
   . Option (Properties s e) (SyntheticAnimationEvent -> EventHandler s e Unit)
onAnimationEndCapture = mkEventOpt "_onAnimationEndCapture"

-- | Creates an option for the `onAnimationIterationCapture` event.
onAnimationIterationCapture
  :: forall s e
   . Option (Properties s e) (SyntheticAnimationEvent -> EventHandler s e Unit)
onAnimationIterationCapture = mkEventOpt "_onAnimationIterationCapture"

-- | Creates an option for the `onTransitionEndCapture` event.
onTransitionEndCapture
  :: forall s e
   . Option (Properties s e) (SyntheticTransitionEvent -> EventHandler s e Unit)
onTransitionEndCapture = mkEventOpt "_onTransitionEndCapture"

-- | Creates an option for the `onToggleCapture` event.
onToggleCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onToggleCapture = mkEventOpt "_onToggleCapture"

-- | Creates an option for the `onErrorCapture` event.
onErrorCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onErrorCapture = mkEventOpt "_onErrorCapture"

-- | Creates an option for the `onLoadCapture` event.
onLoadCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onLoadCapture = mkEventOpt "_onLoadCapture"

-- | Creates an option for the `onAbortCapture` event.
onAbortCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onAbortCapture = mkEventOpt "_onAbortCapture"

-- | Creates an option for the `onCanPlayCapture` event.
onCanPlayCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onCanPlayCapture = mkEventOpt "_onCanPlayCapture"

-- | Creates an option for the `onCanPlayThroughCapture` event.
onCanPlayThroughCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onCanPlayThroughCapture = mkEventOpt "_onCanPlayThroughCapture"

-- | Creates an option for the `onDurationChangeCapture` event.
onDurationChangeCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onDurationChangeCapture = mkEventOpt "_onDurationChangeCapture"

-- | Creates an option for the `onEmptiedCapture` event.
onEmptiedCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onEmptiedCapture = mkEventOpt "_onEmptiedCapture"

-- | Creates an option for the `onEncryptedCapture` event.
onEncryptedCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onEncryptedCapture = mkEventOpt "_onEncryptedCapture"

-- | Creates an option for the `onEndedCapture` event.
onEndedCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onEndedCapture = mkEventOpt "_onEndedCapture"

-- | Creates an option for the `onLoadedDataCapture` event.
onLoadedDataCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onLoadedDataCapture = mkEventOpt "_onLoadedDataCapture"

-- | Creates an option for the `onLoadedMetadataCapture` event.
onLoadedMetadataCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onLoadedMetadataCapture = mkEventOpt "_onLoadedMetadataCapture"

-- | Creates an option for the `onLoadStartCapture` event.
onLoadStartCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onLoadStartCapture = mkEventOpt "_onLoadStartCapture"

-- | Creates an option for the `onPauseCapture` event.
onPauseCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onPauseCapture = mkEventOpt "_onPauseCapture"

-- | Creates an option for the `onPlayCapture` event.
onPlayCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onPlayCapture = mkEventOpt "_onPlayCapture"

-- | Creates an option for the `onPlayingCapture` event.
onPlayingCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onPlayingCapture = mkEventOpt "_onPlayingCapture"

-- | Creates an option for the `onProgressCapture` event.
onProgressCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onProgressCapture = mkEventOpt "_onProgressCapture"

-- | Creates an option for the `onRateChangeCapture` event.
onRateChangeCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onRateChangeCapture = mkEventOpt "_onRateChangeCapture"

-- | Creates an option for the `onSeekedCapture` event.
onSeekedCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onSeekedCapture = mkEventOpt "_onSeekedCapture"

-- | Creates an option for the `onSeekingCapture` event.
onSeekingCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onSeekingCapture = mkEventOpt "_onSeekingCapture"

-- | Creates an option for the `onStalledCapture` event.
onStalledCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onStalledCapture = mkEventOpt "_onStalledCapture"

-- | Creates an option for the `onSuspendCapture` event.
onSuspendCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onSuspendCapture = mkEventOpt "_onSuspendCapture"

-- | Creates an option for the `onTimeUpdateCapture` event.
onTimeUpdateCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onTimeUpdateCapture = mkEventOpt "_onTimeUpdateCapture"

-- | Creates an option for the `onVolumeChangeCapture` event.
onVolumeChangeCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onVolumeChangeCapture = mkEventOpt "_onVolumeChangeCapture"

-- | Creates an option for the `onWaitingCapture` event.
onWaitingCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onWaitingCapture = mkEventOpt "_onWaitingCapture"

-- | Creates an option for the `onCopyCapture` event.
onCopyCapture
  :: forall s e
   . Option (Properties s e) (SyntheticClipboardEvent -> EventHandler s e Unit)
onCopyCapture = mkEventOpt "_onCopyCapture"

-- | Creates an option for the `onCutCapture` event.
onCutCapture
  :: forall s e
   . Option (Properties s e) (SyntheticClipboardEvent -> EventHandler s e Unit)
onCutCapture = mkEventOpt "_onCutCapture"

-- | Creates an option for the `onPasteCapture` event.
onPasteCapture
  :: forall s e
   . Option (Properties s e) (SyntheticClipboardEvent -> EventHandler s e Unit)
onPasteCapture = mkEventOpt "_onPasteCapture"

-- | Creates an option for the `onCompositionEndCapture` event.
onCompositionEndCapture
  :: forall s e
   . Option
      (Properties s e)
      (SyntheticCompositionEvent -> EventHandler s e Unit)
onCompositionEndCapture = mkEventOpt "_onCompositionEndCapture"

-- | Creates an option for the `onCompositionStartCapture` event.
onCompositionStartCapture
  :: forall s e
   . Option
      (Properties s e)
      (SyntheticCompositionEvent -> EventHandler s e Unit)
onCompositionStartCapture = mkEventOpt "_onCompositionStartCapture"

-- | Creates an option for the `onCompositionUpdateCapture` event.
onCompositionUpdateCapture
  :: forall s e
   . Option
      (Properties s e)
      (SyntheticCompositionEvent -> EventHandler s e Unit)
onCompositionUpdateCapture = mkEventOpt "_onCompositionUpdateCapture"

-- | Creates an option for the `onKeyDownCapture` event.
onKeyDownCapture
  :: forall s e
   . Option (Properties s e) (SyntheticKeyboardEvent -> EventHandler s e Unit)
onKeyDownCapture = mkEventOpt "_onKeyDownCapture"

-- | Creates an option for the `onKeyPressCapture` event.
onKeyPressCapture
  :: forall s e
   . Option (Properties s e) (SyntheticKeyboardEvent -> EventHandler s e Unit)
onKeyPressCapture = mkEventOpt "_onKeyPressCapture"

-- | Creates an option for the `onKeyUpCapture` event.
onKeyUpCapture
  :: forall s e
   . Option (Properties s e) (SyntheticKeyboardEvent -> EventHandler s e Unit)
onKeyUpCapture = mkEventOpt "_onKeyUpCapture"

-- | Creates an option for the `onFocusCapture` event.
onFocusCapture
  :: forall s e
   . Option (Properties s e) (SyntheticFocusEvent -> EventHandler s e Unit)
onFocusCapture = mkEventOpt "_onFocusCapture"

-- | Creates an option for the `onBlurCapture` event.
onBlurCapture
  :: forall s e
   . Option (Properties s e) (SyntheticFocusEvent -> EventHandler s e Unit)
onBlurCapture = mkEventOpt "_onBlurCapture"

-- | Creates an option for the `onChangeCapture` event.
onChangeCapture
  :: forall s e
   . Option (Properties s e) (SyntheticInputEvent -> EventHandler s e Unit)
onChangeCapture = mkEventOpt "_onChangeCapture"

-- | Creates an option for the `onInputCapture` event.
onInputCapture
  :: forall s e
   . Option (Properties s e) (SyntheticInputEvent -> EventHandler s e Unit)
onInputCapture = mkEventOpt "_onInputCapture"

-- | Creates an option for the `onInvalidCapture` event.
onInvalidCapture
  :: forall s e
   . Option (Properties s e) (SyntheticInputEvent -> EventHandler s e Unit)
onInvalidCapture = mkEventOpt "_onInvalidCapture"

-- | Creates an option for the `onSubmitCapture` event.
onSubmitCapture
  :: forall s e
   . Option (Properties s e) (SyntheticInputEvent -> EventHandler s e Unit)
onSubmitCapture = mkEventOpt "_onSubmitCapture"

-- | Creates an option for the `onClickCapture` event.
onClickCapture
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onClickCapture = mkEventOpt "_onClickCapture"

-- | Creates an option for the `onContextMenuCapture` event.
onContextMenuCapture
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onContextMenuCapture = mkEventOpt "_onContextMenuCapture"

-- | Creates an option for the `onDoubleClickCapture` event.
onDoubleClickCapture
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onDoubleClickCapture = mkEventOpt "_onDoubleClickCapture"

-- | Creates an option for the `onDragCapture` event.
onDragCapture
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onDragCapture = mkEventOpt "_onDragCapture"

-- | Creates an option for the `onDragEndCapture` event.
onDragEndCapture
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onDragEndCapture = mkEventOpt "_onDragEndCapture"

-- | Creates an option for the `onDragEnterCapture` event.
onDragEnterCapture
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onDragEnterCapture = mkEventOpt "_onDragEnterCapture"

-- | Creates an option for the `onDragExitCapture` event.
onDragExitCapture
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onDragExitCapture = mkEventOpt "_onDragExitCapture"

-- | Creates an option for the `onDragLeaveCapture` event.
onDragLeaveCapture
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onDragLeaveCapture = mkEventOpt "_onDragLeaveCapture"

-- | Creates an option for the `onDragOverCapture` event.
onDragOverCapture
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onDragOverCapture = mkEventOpt "_onDragOverCapture"

-- | Creates an option for the `onDragStartCapture` event.
onDragStartCapture
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onDragStartCapture = mkEventOpt "_onDragStartCapture"

-- | Creates an option for the `onDropCapture` event.
onDropCapture
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onDropCapture = mkEventOpt "_onDropCapture"

-- | Creates an option for the `onMouseDownCapture` event.
onMouseDownCapture
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseDownCapture = mkEventOpt "_onMouseDownCapture"

-- | Creates an option for the `onMouseEnterCapture` event.
onMouseEnterCapture
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseEnterCapture = mkEventOpt "_onMouseEnterCapture"

-- | Creates an option for the `onMouseLeaveCapture` event.
onMouseLeaveCapture
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseLeaveCapture = mkEventOpt "_onMouseLeaveCapture"

-- | Creates an option for the `onMouseMoveCapture` event.
onMouseMoveCapture
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseMoveCapture = mkEventOpt "_onMouseMoveCapture"

-- | Creates an option for the `onMouseOutCapture` event.
onMouseOutCapture
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseOutCapture = mkEventOpt "_onMouseOutCapture"

-- | Creates an option for the `onMouseOverCapture` event.
onMouseOverCapture
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseOverCapture = mkEventOpt "_onMouseOverCapture"

-- | Creates an option for the `onMouseUpCapture` event.
onMouseUpCapture
  :: forall s e
   . Option (Properties s e) (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseUpCapture = mkEventOpt "_onMouseUpCapture"

-- | Creates an option for the `onSelectCapture` event.
onSelectCapture
  :: forall s e
   . Option (Properties s e) (SyntheticEvent -> EventHandler s e Unit)
onSelectCapture = mkEventOpt "_onSelectCapture"

-- | Creates an option for the `onTouchCancelCapture` event.
onTouchCancelCapture
  :: forall s e
   . Option (Properties s e) (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchCancelCapture = mkEventOpt "_onTouchCancelCapture"

-- | Creates an option for the `onTouchEndCapture` event.
onTouchEndCapture
  :: forall s e
   . Option (Properties s e) (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchEndCapture = mkEventOpt "_onTouchEndCapture"

-- | Creates an option for the `onTouchMoveCapture` event.
onTouchMoveCapture
  :: forall s e
   . Option (Properties s e) (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchMoveCapture = mkEventOpt "_onTouchMoveCapture"

-- | Creates an option for the `onTouchStartCapture` event.
onTouchStartCapture
  :: forall s e
   . Option (Properties s e) (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchStartCapture = mkEventOpt "_onTouchStartCapture"

-- | Creates an option for the `onScrollCapture` event.
onScrollCapture
  :: forall s e
   . Option (Properties s e) (SyntheticUIEvent -> EventHandler s e Unit)
onScrollCapture = mkEventOpt "_onScrollCapture"

-- | Creates an option for the `onWheelCapture` event.
onWheelCapture
  :: forall s e
   . Option (Properties s e) (SyntheticWheelEvent -> EventHandler s e Unit)
onWheelCapture = mkEventOpt "_onWheelCapture"
