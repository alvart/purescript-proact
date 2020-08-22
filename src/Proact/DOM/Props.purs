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
import Data.Functor.Contravariant (cmap)
import Data.Maybe(Maybe(..))
import Data.Options (Option, opt)
import Data.Tuple (Tuple(..))
import Prelude (Unit, ($), bind, pure)
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
data Properties

-- | Creates an option for the `style` property.
style :: Option Properties CSS
style = cmap renderInline $ opt "style"
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
accept :: Option Properties String
accept = opt "accept"

-- | Creates an option for the `acceptCharset` property.
acceptCharset :: Option Properties String
acceptCharset = opt "acceptCharset"

-- | Creates an option for the `accessKey` property.
accessKey :: Option Properties String
accessKey = opt "accessKey"

-- | Creates an option for the `action` property.
action :: Option Properties String
action = opt "action"

-- | Creates an option for the `allowFullScreen` property.
allowFullScreen :: Option Properties Boolean
allowFullScreen = opt "allowFullScreen"

-- | Creates an option for the `allowTransparency` property.
allowTransparency :: Option Properties Boolean
allowTransparency = opt "allowTransparency"

-- | Creates an option for the `alt` property.
alt :: Option Properties String
alt = opt "alt"

-- | Creates an option for the `async` property.
async :: Option Properties Boolean
async = opt "async"

-- | Creates an option for the `autoComplete` property.
autoComplete :: Option Properties String
autoComplete = opt "autoComplete"

-- | Creates an option for the `autoFocus` property.
autoFocus :: Option Properties Boolean
autoFocus = opt "autoFocus"

-- | Creates an option for the `autoPlay` property.
autoPlay :: Option Properties Boolean
autoPlay = opt "autoPlay"

-- | Creates an option for the `capture` property.
capture :: Option Properties Boolean
capture = opt "capture"

-- | Creates an option for the `cellPadding` property.
cellPadding :: Option Properties String
cellPadding = opt "cellPadding"

-- | Creates an option for the `cellSpacing` property.
cellSpacing :: Option Properties String
cellSpacing = opt "cellSpacing"

-- | Creates an option for the `charSet` property.
charSet :: Option Properties String
charSet = opt "charSet"

-- | Creates an option for the `challenge` property.
challenge :: Option Properties String
challenge = opt "checked"

-- | Creates an option for the `checked` property.
checked :: Option Properties Boolean
checked = opt "checked"

-- | Creates an option for the `cite` property.
cite :: Option Properties String
cite = opt "cite"

-- | Creates an option for the `classID` property.
classID :: Option Properties String
classID = opt "classID"

-- | Creates an option for the `className` property.
className :: Option Properties String
className = opt "className"

-- | Creates an option for the `cols` property.
cols :: Option Properties Int
cols = opt "cols"

-- | Creates an option for the `colSpan` property.
colSpan :: Option Properties Int
colSpan = opt "colSpan"

-- | Creates an option for the `content` property.
content :: Option Properties String
content = opt "content"

-- | Creates an option for the `contentEditable` property.
contentEditable :: Option Properties Boolean
contentEditable = opt "contentEditable"

-- | Creates an option for the `contextMenu` property.
contextMenu :: Option Properties String
contextMenu = opt "contextMenu"

-- | Creates an option for the `controls` property.
controls :: Option Properties Boolean
controls = opt "controls"

-- | Creates an option for the `coords` property.
coords :: Option Properties String
coords = opt "coords"

-- | Creates an option for the `crossOrigin` property.
crossOrigin :: Option Properties String
crossOrigin = opt "crossOrigin"

-- | Creates an option for the `dateTime` property.
dateTime :: Option Properties String
dateTime = opt "dateTime"

-- | Creates an option for the `default` property.
default :: Option Properties Boolean
default = opt "default"

-- | Creates an option for the `defaultChecked` property.
defaultChecked :: Option Properties Boolean
defaultChecked = opt "defaultChecked"

-- | Creates an option for the `defaultValue` property.
defaultValue :: Option Properties String
defaultValue = opt "defaultValue"

-- | Creates an option for the `defer` property.
defer :: Option Properties Boolean
defer = opt "defer"

-- | Creates an option for the `dir` property.
dir :: Option Properties String
dir = opt "dir"

-- | Creates an option for the `disabled` property.
disabled :: Option Properties Boolean
disabled = opt "disabled"

-- | Creates an option for the `download` property.
download :: Option Properties String
download = opt "download"

-- | Creates an option for the `draggable` property.
draggable :: Option Properties Boolean
draggable = opt "draggable"

-- | Creates an option for the `encType` property.
encType :: Option Properties String
encType = opt "encType"

-- | Creates an option for the `form` property.
form :: Option Properties String
form = opt "form"

-- | Creates an option for the `formAction` property.
formAction :: Option Properties String
formAction = opt "formAction"

-- | Creates an option for the `formEncType` property.
formEncType :: Option Properties String
formEncType = opt "formEncType"

-- | Creates an option for the `formMethod` property.
formMethod :: Option Properties String
formMethod = opt "formMethod"

-- | Creates an option for the `formNoValidate` property.
formNoValidate :: Option Properties Boolean
formNoValidate = opt "formNoValidate"

-- | Creates an option for the `formTarget` property.
formTarget :: Option Properties String
formTarget = opt "formTarget"

-- | Creates an option for the `frameBorder` property.
frameBorder :: Option Properties String
frameBorder = opt "frameBorder"

-- | Creates an option for the `headers` property.
headers :: Option Properties String
headers = opt "headers"

-- | Creates an option for the `height` property.
height :: Option Properties String
height = opt "height"

-- | Creates an option for the `hidden` property.
hidden :: Option Properties Boolean
hidden = opt "hidden"

-- | Creates an option for the `high` property.
high :: Option Properties String
high = opt "high"

-- | Creates an option for the `href` property.
href :: Option Properties String
href = opt "href"

-- | Creates an option for the `hrefLang` property.
hrefLang :: Option Properties String
hrefLang = opt "hrefLang"

-- | Creates an option for the `htmlFor` property.
htmlFor :: Option Properties String
htmlFor = opt "htmlFor"

-- | Creates an option for the `httpEquiv` property.
httpEquiv :: Option Properties String
httpEquiv = opt "httpEquiv"

-- | Creates an option for the `icon` property.
icon :: Option Properties String
icon = opt "icon"

-- | Creates an option for the `_id` property.
_id :: Option Properties String
_id = opt "id"

-- | Creates an option for the `inputMode` property.
inputMode :: Option Properties String
inputMode = opt "inputMode"

-- | Creates an option for the `integrity` property.
integrity :: Option Properties String
integrity = opt "integrity"

-- | Creates an option for the `is` property.
is :: Option Properties String
is = opt "is"

-- | Creates an option for the `key` property.
key :: Option Properties String
key = opt "key"

-- | Creates an option for the `keyParams` property.
keyParams :: Option Properties String
keyParams = opt "keyParams"

-- | Creates an option for the `keyType` property.
keyType :: Option Properties String
keyType = opt "keyType"

-- | Creates an option for the `kind` property.
kind :: Option Properties String
kind = opt "kind"

-- | Creates an option for the `label` property.
label :: Option Properties String
label = opt "label"

-- | Creates an option for the `lang` property.
lang :: Option Properties String
lang = opt "lang"

-- | Creates an option for the `list` property.
list :: Option Properties String
list = opt "list"

-- | Creates an option for the `loop` property.
loop :: Option Properties Boolean
loop = opt "loop"

-- | Creates an option for the `low` property.
low :: Option Properties String
low = opt "low"

-- | Creates an option for the `manifest` property.
manifest :: Option Properties String
manifest = opt "manifest"

-- | Creates an option for the `marginHeight` property.
marginHeight :: Option Properties String
marginHeight = opt "marginHeight"

-- | Creates an option for the `marginWidth` property.
marginWidth :: Option Properties String
marginWidth = opt "marginWidth"

-- | Creates an option for the `max` property.
max :: Option Properties String
max = opt "max"

-- | Creates an option for the `maxLength` property.
maxLength :: Option Properties String
maxLength = opt "maxLength"

-- | Creates an option for the `media` property.
media :: Option Properties String
media = opt "media"

-- | Creates an option for the `mediaGroup` property.
mediaGroup :: Option Properties String
mediaGroup = opt "mediaGroup"

-- | Creates an option for the `method` property.
method :: Option Properties String
method = opt "method"

-- | Creates an option for the `min` property.
min :: Option Properties String
min = opt "min"

-- | Creates an option for the `minLength` property.
minLength :: Option Properties String
minLength = opt "minLength"

-- | Creates an option for the `multiple` property.
multiple :: Option Properties Boolean
multiple = opt "multiple"

-- | Creates an option for the `muted` property.
muted :: Option Properties Boolean
muted = opt "muted"

-- | Creates an option for the `name` property.
name :: Option Properties String
name = opt "name"

-- | Creates an option for the `nonce` property.
nonce :: Option Properties String
nonce = opt "nonce"

-- | Creates an option for the `noValidate` property.
noValidate :: Option Properties Boolean
noValidate = opt "noValidate"

-- | Creates an option for the `open` property.
open :: Option Properties Boolean
open = opt "open"

-- | Creates an option for the `optimum` property.
optimum :: Option Properties String
optimum = opt "optimum"

-- | Creates an option for the `pattern` property.
pattern :: Option Properties String
pattern = opt "pattern"

-- | Creates an option for the `placeholder` property.
placeholder :: Option Properties String
placeholder = opt "placeholder"

-- | Creates an option for the `poster` property.
poster :: Option Properties String
poster = opt "poster"

-- | Creates an option for the `preload` property.
preload :: Option Properties String
preload = opt "preload"

-- | Creates an option for the `profile` property.
profile :: Option Properties String
profile = opt "profile"

-- | Creates an option for the `radioGroup` property.
radioGroup :: Option Properties String
radioGroup = opt "radioGroup"

-- | Creates an option for the `readOnly` property.
readOnly :: Option Properties Boolean
readOnly = opt "readOnly"

-- | Creates an option for the `rel` property.
rel :: Option Properties String
rel = opt "rel"

-- | Creates an option for the `required` property.
required :: Option Properties Boolean
required = opt "required"

-- | Creates an option for the `reversed` property.
reversed :: Option Properties Boolean
reversed = opt "reversed"

-- | Creates an option for the `role` property.
role :: Option Properties String
role = opt "role"

-- | Creates an option for the `rows` property.
rows :: Option Properties Int
rows = opt "rows"

-- | Creates an option for the `rowSpan` property.
rowSpan :: Option Properties Int
rowSpan = opt "rowSpan"

-- | Creates an option for the `sandbox` property.
sandbox :: Option Properties String
sandbox = opt "sandbox"

-- | Creates an option for the `scope` property.
scope :: Option Properties String
scope = opt "scope"

-- | Creates an option for the `scoped` property.
scoped :: Option Properties Boolean
scoped = opt "scoped"

-- | Creates an option for the `scrolling` property.
scrolling :: Option Properties String
scrolling = opt "scrolling"

-- | Creates an option for the `seamless` property.
seamless :: Option Properties Boolean
seamless = opt "seamless"

-- | Creates an option for the `selected` property.
selected :: Option Properties Boolean
selected = opt "selected"

-- | Creates an option for the `shape` property.
shape :: Option Properties String
shape = opt "shape"

-- | Creates an option for the `size` property.
size :: Option Properties Int
size = opt "size"

-- | Creates an option for the `sizes` property.
sizes :: Option Properties String
sizes = opt "sizes"

-- | Creates an option for the `span` property.
span :: Option Properties Int
span = opt "span"

-- | Creates an option for the `spellCheck` property.
spellCheck :: Option Properties Boolean
spellCheck = opt "spellCheck"

-- | Creates an option for the `src` property.
src :: Option Properties String
src = opt "src"

-- | Creates an option for the `srcDoc` property.
srcDoc :: Option Properties String
srcDoc = opt "srcDoc"

-- | Creates an option for the `srcLang` property.
srcLang :: Option Properties String
srcLang = opt "srcLang"

-- | Creates an option for the `srcSet` property.
srcSet :: Option Properties String
srcSet = opt "srcSet"

-- | Creates an option for the `start` property.
start :: Option Properties Int
start = opt "start"

-- | Creates an option for the `step` property.
step :: Option Properties String
step = opt "step"

-- | Creates an option for the `summary` property.
summary :: Option Properties String
summary = opt "summary"

-- | Creates an option for the `tabIndex` property.
tabIndex :: Option Properties Int
tabIndex = opt "tabIndex"

-- | Creates an option for the `target` property.
target :: Option Properties String
target = opt "target"

-- | Creates an option for the `title` property.
title :: Option Properties String
title = opt "title"

-- | Creates an option for the `_type` property.
_type :: Option Properties String
_type = opt "type"

-- | Creates an option for the `useMap` property.
useMap :: Option Properties String
useMap = opt "useMap"

-- | Creates an option for the `value` property.
value :: Option Properties String
value = opt "value"

-- | Creates an option for the `valueArray` property.
valueArray :: Option Properties (Array String)
valueArray = opt "value"

-- | Creates an option for the `width` property.
width :: Option Properties String
width = opt "width"

-- | Creates an option for the `wmode` property.
wmode :: Option Properties String
wmode = opt "wmode"

-- | Creates an option for the `wrap` property.
wrap :: Option Properties String
wrap = opt "wrap"

-- RDFa Attributes

-- | Creates an option for the `about` property.
about :: Option Properties String
about = opt "about"

-- | Creates an option for the `datatype` property.
datatype :: Option Properties String
datatype = opt "datatype"

-- | Creates an option for the `inlist` property.
inlist :: Option Properties String
inlist = opt "inlist"

-- | Creates an option for the `prefix` property.
prefix :: Option Properties String
prefix = opt "prefix"

-- | Creates an option for the `property` property.
property :: Option Properties String
property = opt "property"

-- | Creates an option for the `resource` property.
resource :: Option Properties String
resource = opt "resource"

-- | Creates an option for the `typeof` property.
typeof :: Option Properties String
typeof = opt "typeof"

-- | Creates an option for the `vocab` property.
vocab :: Option Properties String
vocab = opt "vocab"

-- Non-standard Attributes

-- | Creates an option for the `autoCapitalize` property.
autoCapitalize :: Option Properties String
autoCapitalize = opt "autoCapitalize"

-- | Creates an option for the `autoCorrect` property.
autoCorrect :: Option Properties String
autoCorrect = opt "autoCorrect"

-- | Creates an option for the `autoSave` property.
autoSave :: Option Properties String
autoSave = opt "autoSave"

-- | Creates an option for the `color` property.
color :: Option Properties String
color = opt "color"

-- | Creates an option for the `itemProp` property.
itemProp :: Option Properties String
itemProp = opt "itemProp"

-- | Creates an option for the `itemScope` property.
itemScope :: Option Properties Boolean
itemScope = opt "itemScope"

-- | Creates an option for the `itemType` property.
itemType :: Option Properties String
itemType = opt "itemType"

-- | Creates an option for the `itemID` property.
itemID :: Option Properties String
itemID = opt "itemID"

-- | Creates an option for the `itemRef` property.
itemRef :: Option Properties String
itemRef = opt "itemRef"

-- | Creates an option for the `results` property.
results :: Option Properties Int
results = opt "results"

-- | Creates an option for the `security` property.
security :: Option Properties String
security = opt "security"

-- | Creates an option for the `unselectable` property.
unselectable :: Option Properties Boolean
unselectable = opt "unselectable"

-- Events

-- | Creates an option for the `onAnimationStart` event.
onAnimationStart
  :: forall s e
   . Option Properties (SyntheticAnimationEvent -> EventHandler s e Unit)
onAnimationStart = opt "onAnimationStart"

-- | Creates an option for the `onAnimationEnd` event.
onAnimationEnd
  :: forall s e
   . Option Properties (SyntheticAnimationEvent -> EventHandler s e Unit)
onAnimationEnd = opt "onAnimationEnd"

-- | Creates an option for the `onAnimationIteration` event.
onAnimationIteration
  :: forall s e
   . Option Properties (SyntheticAnimationEvent -> EventHandler s e Unit)
onAnimationIteration = opt "onAnimationIteration"

-- | Creates an option for the `onTransitionEnd` event.
onTransitionEnd
  :: forall s e
   . Option Properties (SyntheticTransitionEvent -> EventHandler s e Unit)
onTransitionEnd = opt "onTransitionEnd"

-- | Creates an option for the `onToggle` event.
onToggle
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onToggle = opt "onToggle"

-- | Creates an option for the `onError` event.
onError
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onError = opt "onError"

-- | Creates an option for the `onLoad` event.
onLoad
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onLoad = opt "onLoad"

-- | Creates an option for the `onAbort` event.
onAbort
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onAbort = opt "onAbort"

-- | Creates an option for the `onCanPlay` event.
onCanPlay
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onCanPlay = opt "onCanPlay"

-- | Creates an option for the `onCanPlayThrough` event.
onCanPlayThrough
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onCanPlayThrough = opt "onCanPlayThrough"

-- | Creates an option for the `onDurationChange` event.
onDurationChange
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onDurationChange = opt "onDurationChange"

-- | Creates an option for the `onEmptied` event.
onEmptied
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onEmptied = opt "onEmptied"

-- | Creates an option for the `onEncrypted` event.
onEncrypted
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onEncrypted = opt "onEncrypted"

-- | Creates an option for the `onEnded` event.
onEnded
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onEnded = opt "onEnded"

-- | Creates an option for the `onLoadedData` event.
onLoadedData
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onLoadedData = opt "onLoadedData"

-- | Creates an option for the `onLoadedMetadata` event.
onLoadedMetadata
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onLoadedMetadata = opt "onLoadedMetadata"

-- | Creates an option for the `onLoadStart` event.
onLoadStart
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onLoadStart = opt "onLoadStart"

-- | Creates an option for the `onPause` event.
onPause
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onPause = opt "onPause"

-- | Creates an option for the `onPlay` event.
onPlay
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onPlay = opt "onPlay"

-- | Creates an option for the `onPlaying` event.
onPlaying
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onPlaying = opt "onPlaying"

-- | Creates an option for the `onProgress` event.
onProgress
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onProgress = opt "onProgress"

-- | Creates an option for the `onRateChange` event.
onRateChange
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onRateChange = opt "onRateChange"

-- | Creates an option for the `onSeeked` event.
onSeeked
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onSeeked = opt "onSeeked"

-- | Creates an option for the `onSeeking` event.
onSeeking
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onSeeking = opt "onSeeking"

-- | Creates an option for the `onStalled` event.
onStalled
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onStalled = opt "onStalled"

-- | Creates an option for the `onSuspend` event.
onSuspend
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onSuspend = opt "onSuspend"

-- | Creates an option for the `onTimeUpdate` event.
onTimeUpdate
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onTimeUpdate = opt "onTimeUpdate"

-- | Creates an option for the `onVolumeChange` event.
onVolumeChange
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onVolumeChange = opt "onVolumeChange"

-- | Creates an option for the `onWaiting` event.
onWaiting
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onWaiting = opt "onWaiting"

-- | Creates an option for the `onCopy` event.
onCopy
  :: forall s e
   . Option Properties (SyntheticClipboardEvent -> EventHandler s e Unit)
onCopy = opt "onCopy"

-- | Creates an option for the `onCut` event.
onCut
  :: forall s e
   . Option Properties (SyntheticClipboardEvent -> EventHandler s e Unit)
onCut = opt "onCut"

-- | Creates an option for the `onPaste` event.
onPaste
  :: forall s e
   . Option Properties (SyntheticClipboardEvent -> EventHandler s e Unit)
onPaste = opt "onPaste"

-- | Creates an option for the `onCompositionEnd` event.
onCompositionEnd
  :: forall s e
   . Option Properties (SyntheticCompositionEvent -> EventHandler s e Unit)
onCompositionEnd = opt "onCompositionEnd"

-- | Creates an option for the `onCompositionStart` event.
onCompositionStart
  :: forall s e
   . Option Properties (SyntheticCompositionEvent -> EventHandler s e Unit)
onCompositionStart = opt "onCompositionStart"

-- | Creates an option for the `onCompositionUpdate` event.
onCompositionUpdate
  :: forall s e
   . Option Properties (SyntheticCompositionEvent -> EventHandler s e Unit)
onCompositionUpdate = opt "onCompositionUpdate"

-- | Creates an option for the `onKeyDown` event.
onKeyDown
  :: forall s e
   . Option Properties (SyntheticKeyboardEvent -> EventHandler s e Unit)
onKeyDown = opt "onKeyDown"

-- | Creates an option for the `onKeyPress` event.
onKeyPress
  :: forall s e
   . Option Properties (SyntheticKeyboardEvent -> EventHandler s e Unit)
onKeyPress = opt "onKeyPress"

-- | Creates an option for the `onKeyUp` event.
onKeyUp
  :: forall s e
   . Option Properties (SyntheticKeyboardEvent -> EventHandler s e Unit)
onKeyUp = opt "onKeyUp"

-- | Creates an option for the `onFocus` event.
onFocus
  :: forall s e
   . Option Properties (SyntheticFocusEvent -> EventHandler s e Unit)
onFocus = opt "onFocus"

-- | Creates an option for the `onBlur` event.
onBlur
  :: forall s e
   . Option Properties (SyntheticFocusEvent -> EventHandler s e Unit)
onBlur = opt "onBlur"

-- | Creates an option for the `onChange` event.
onChange
  :: forall s e
   . Option Properties (SyntheticInputEvent -> EventHandler s e Unit)
onChange = opt "onChange"

-- | Creates an option for the `onInput` event.
onInput
  :: forall s e
   . Option Properties (SyntheticInputEvent -> EventHandler s e Unit)
onInput = opt "onInput"

-- | Creates an option for the `onInvalid` event.
onInvalid
  :: forall s e
   . Option Properties (SyntheticInputEvent -> EventHandler s e Unit)
onInvalid = opt "onInvalid"

-- | Creates an option for the `onSubmit` event.
onSubmit
  :: forall s e
   . Option Properties (SyntheticInputEvent -> EventHandler s e Unit)
onSubmit = opt "onSubmit"

-- | Creates an option for the `onClick` event.
onClick
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onClick = opt "onClick"

-- | Creates an option for the `onContextMenu` event.
onContextMenu
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onContextMenu = opt "onContextMenu"

-- | Creates an option for the `onDoubleClick` event.
onDoubleClick
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDoubleClick = opt "onDoubleClick"

-- | Creates an option for the `onDrag` event.
onDrag
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDrag = opt "onDrag"

-- | Creates an option for the `onDragEnd` event.
onDragEnd
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragEnd = opt "onDragEnd"

-- | Creates an option for the `onDragEnter` event.
onDragEnter
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragEnter = opt "onDragEnter"

-- | Creates an option for the `onDragExit` event.
onDragExit
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragExit = opt "onDragExit"

-- | Creates an option for the `onDragLeave` event.
onDragLeave
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragLeave = opt "onDragLeave"

-- | Creates an option for the `onDragOver` event.
onDragOver
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragOver = opt "onDragOver"

-- | Creates an option for the `onDragStart` event.
onDragStart
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragStart = opt "onDragStart"

-- | Creates an option for the `onDrop` event.
onDrop
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDrop = opt "onDrop"

-- | Creates an option for the `onMouseDown` event.
onMouseDown
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseDown = opt "onMouseDown"

-- | Creates an option for the `onMouseEnter` event.
onMouseEnter
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseEnter = opt "onMouseEnter"

-- | Creates an option for the `onMouseLeave` event.
onMouseLeave
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseLeave = opt "onMouseLeave"

-- | Creates an option for the `onMouseMove` event.
onMouseMove
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseMove = opt "onMouseMove"

-- | Creates an option for the `onMouseOut` event.
onMouseOut
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseOut = opt "onMouseOut"

-- | Creates an option for the `onMouseOver` event.
onMouseOver
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseOver = opt "onMouseOver"

-- | Creates an option for the `onMouseUp` event.
onMouseUp
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseUp = opt "onMouseUp"

-- | Creates an option for the `onSelect` event.
onSelect
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onSelect = opt "onSelect"

-- | Creates an option for the `onTouchCancel` event.
onTouchCancel
  :: forall s e
   . Option Properties (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchCancel = opt "onTouchCancel"

-- | Creates an option for the `onTouchEnd` event.
onTouchEnd
  :: forall s e
   . Option Properties (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchEnd = opt "onTouchEnd"

-- | Creates an option for the `onTouchMove` event.
onTouchMove
  :: forall s e
   . Option Properties (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchMove = opt "onTouchMove"

-- | Creates an option for the `onTouchStart` event.
onTouchStart
  :: forall s e
   . Option Properties (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchStart = opt "onTouchStart"

-- | Creates an option for the `onScroll` event.
onScroll
  :: forall s e
   . Option Properties (SyntheticUIEvent -> EventHandler s e Unit)
onScroll = opt "onScroll"

-- | Creates an option for the `onWheel` event.
onWheel
  :: forall s e
   . Option Properties (SyntheticWheelEvent -> EventHandler s e Unit)
onWheel = opt "onWheel"

-- | Creates an option for the `onAnimationStartCapture` event.
onAnimationStartCapture
  :: forall s e
   . Option Properties (SyntheticAnimationEvent -> EventHandler s e Unit)
onAnimationStartCapture = opt "onAnimationStartCapture"

-- | Creates an option for the `onAnimationEndCapture` event.
onAnimationEndCapture
  :: forall s e
   . Option Properties (SyntheticAnimationEvent -> EventHandler s e Unit)
onAnimationEndCapture = opt "onAnimationEndCapture"

-- | Creates an option for the `onAnimationIterationCapture` event.
onAnimationIterationCapture
  :: forall s e
   . Option Properties (SyntheticAnimationEvent -> EventHandler s e Unit)
onAnimationIterationCapture = opt "onAnimationIterationCapture"

-- | Creates an option for the `onTransitionEndCapture` event.
onTransitionEndCapture
  :: forall s e
   . Option Properties (SyntheticTransitionEvent -> EventHandler s e Unit)
onTransitionEndCapture = opt "onTransitionEndCapture"

-- | Creates an option for the `onToggleCapture` event.
onToggleCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onToggleCapture = opt "onToggleCapture"

-- | Creates an option for the `onErrorCapture` event.
onErrorCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onErrorCapture = opt "onErrorCapture"

-- | Creates an option for the `onLoadCapture` event.
onLoadCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onLoadCapture = opt "onLoadCapture"

-- | Creates an option for the `onAbortCapture` event.
onAbortCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onAbortCapture = opt "onAbortCapture"

-- | Creates an option for the `onCanPlayCapture` event.
onCanPlayCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onCanPlayCapture = opt "onCanPlayCapture"

-- | Creates an option for the `onCanPlayThroughCapture` event.
onCanPlayThroughCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onCanPlayThroughCapture = opt "onCanPlayThroughCapture"

-- | Creates an option for the `onDurationChangeCapture` event.
onDurationChangeCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onDurationChangeCapture = opt "onDurationChangeCapture"

-- | Creates an option for the `onEmptiedCapture` event.
onEmptiedCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onEmptiedCapture = opt "onEmptiedCapture"

-- | Creates an option for the `onEncryptedCapture` event.
onEncryptedCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onEncryptedCapture = opt "onEncryptedCapture"

-- | Creates an option for the `onEndedCapture` event.
onEndedCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onEndedCapture = opt "onEndedCapture"

-- | Creates an option for the `onLoadedDataCapture` event.
onLoadedDataCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onLoadedDataCapture = opt "onLoadedDataCapture"

-- | Creates an option for the `onLoadedMetadataCapture` event.
onLoadedMetadataCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onLoadedMetadataCapture = opt "onLoadedMetadataCapture"

-- | Creates an option for the `onLoadStartCapture` event.
onLoadStartCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onLoadStartCapture = opt "onLoadStartCapture"

-- | Creates an option for the `onPauseCapture` event.
onPauseCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onPauseCapture = opt "onPauseCapture"

-- | Creates an option for the `onPlayCapture` event.
onPlayCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onPlayCapture = opt "onPlayCapture"

-- | Creates an option for the `onPlayingCapture` event.
onPlayingCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onPlayingCapture = opt "onPlayingCapture"

-- | Creates an option for the `onProgressCapture` event.
onProgressCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onProgressCapture = opt "onProgressCapture"

-- | Creates an option for the `onRateChangeCapture` event.
onRateChangeCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onRateChangeCapture = opt "onRateChangeCapture"

-- | Creates an option for the `onSeekedCapture` event.
onSeekedCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onSeekedCapture = opt "onSeekedCapture"

-- | Creates an option for the `onSeekingCapture` event.
onSeekingCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onSeekingCapture = opt "onSeekingCapture"

-- | Creates an option for the `onStalledCapture` event.
onStalledCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onStalledCapture = opt "onStalledCapture"

-- | Creates an option for the `onSuspendCapture` event.
onSuspendCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onSuspendCapture = opt "onSuspendCapture"

-- | Creates an option for the `onTimeUpdateCapture` event.
onTimeUpdateCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onTimeUpdateCapture = opt "onTimeUpdateCapture"

-- | Creates an option for the `onVolumeChangeCapture` event.
onVolumeChangeCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onVolumeChangeCapture = opt "onVolumeChangeCapture"

-- | Creates an option for the `onWaitingCapture` event.
onWaitingCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onWaitingCapture = opt "onWaitingCapture"

-- | Creates an option for the `onCopyCapture` event.
onCopyCapture
  :: forall s e
   . Option Properties (SyntheticClipboardEvent -> EventHandler s e Unit)
onCopyCapture = opt "onCopyCapture"

-- | Creates an option for the `onCutCapture` event.
onCutCapture
  :: forall s e
   . Option Properties (SyntheticClipboardEvent -> EventHandler s e Unit)
onCutCapture = opt "onCutCapture"

-- | Creates an option for the `onPasteCapture` event.
onPasteCapture
  :: forall s e
   . Option Properties (SyntheticClipboardEvent -> EventHandler s e Unit)
onPasteCapture = opt "onPasteCapture"

-- | Creates an option for the `onCompositionEndCapture` event.
onCompositionEndCapture
  :: forall s e
   . Option Properties (SyntheticCompositionEvent -> EventHandler s e Unit)
onCompositionEndCapture = opt "onCompositionEndCapture"

-- | Creates an option for the `onCompositionStartCapture` event.
onCompositionStartCapture
  :: forall s e
   . Option Properties (SyntheticCompositionEvent -> EventHandler s e Unit)
onCompositionStartCapture = opt "onCompositionStartCapture"

-- | Creates an option for the `onCompositionUpdateCapture` event.
onCompositionUpdateCapture
  :: forall s e
   . Option Properties (SyntheticCompositionEvent -> EventHandler s e Unit)
onCompositionUpdateCapture = opt "onCompositionUpdateCapture"

-- | Creates an option for the `onKeyDownCapture` event.
onKeyDownCapture
  :: forall s e
   . Option Properties (SyntheticKeyboardEvent -> EventHandler s e Unit)
onKeyDownCapture = opt "onKeyDownCapture"

-- | Creates an option for the `onKeyPressCapture` event.
onKeyPressCapture
  :: forall s e
   . Option Properties (SyntheticKeyboardEvent -> EventHandler s e Unit)
onKeyPressCapture = opt "onKeyPressCapture"

-- | Creates an option for the `onKeyUpCapture` event.
onKeyUpCapture
  :: forall s e
   . Option Properties (SyntheticKeyboardEvent -> EventHandler s e Unit)
onKeyUpCapture = opt "onKeyUpCapture"

-- | Creates an option for the `onFocusCapture` event.
onFocusCapture
  :: forall s e
   . Option Properties (SyntheticFocusEvent -> EventHandler s e Unit)
onFocusCapture = opt "onFocusCapture"

-- | Creates an option for the `onBlurCapture` event.
onBlurCapture
  :: forall s e
   . Option Properties (SyntheticFocusEvent -> EventHandler s e Unit)
onBlurCapture = opt "onBlurCapture"

-- | Creates an option for the `onChangeCapture` event.
onChangeCapture
  :: forall s e
   . Option Properties (SyntheticInputEvent -> EventHandler s e Unit)
onChangeCapture = opt "onChangeCapture"

-- | Creates an option for the `onInputCapture` event.
onInputCapture
  :: forall s e
   . Option Properties (SyntheticInputEvent -> EventHandler s e Unit)
onInputCapture = opt "onInputCapture"

-- | Creates an option for the `onInvalidCapture` event.
onInvalidCapture
  :: forall s e
   . Option Properties (SyntheticInputEvent -> EventHandler s e Unit)
onInvalidCapture = opt "onInvalidCapture"

-- | Creates an option for the `onSubmitCapture` event.
onSubmitCapture
  :: forall s e
   . Option Properties (SyntheticInputEvent -> EventHandler s e Unit)
onSubmitCapture = opt "onSubmitCapture"

-- | Creates an option for the `onClickCapture` event.
onClickCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onClickCapture = opt "onClickCapture"

-- | Creates an option for the `onContextMenuCapture` event.
onContextMenuCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onContextMenuCapture = opt "onContextMenuCapture"

-- | Creates an option for the `onDoubleClickCapture` event.
onDoubleClickCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDoubleClickCapture = opt "onDoubleClickCapture"

-- | Creates an option for the `onDragCapture` event.
onDragCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragCapture = opt "onDragCapture"

-- | Creates an option for the `onDragEndCapture` event.
onDragEndCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragEndCapture = opt "onDragEndCapture"

-- | Creates an option for the `onDragEnterCapture` event.
onDragEnterCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragEnterCapture = opt "onDragEnterCapture"

-- | Creates an option for the `onDragExitCapture` event.
onDragExitCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragExitCapture = opt "onDragExitCapture"

-- | Creates an option for the `onDragLeaveCapture` event.
onDragLeaveCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragLeaveCapture = opt "onDragLeaveCapture"

-- | Creates an option for the `onDragOverCapture` event.
onDragOverCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragOverCapture = opt "onDragOverCapture"

-- | Creates an option for the `onDragStartCapture` event.
onDragStartCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragStartCapture = opt "onDragStartCapture"

-- | Creates an option for the `onDropCapture` event.
onDropCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDropCapture = opt "onDropCapture"

-- | Creates an option for the `onMouseDownCapture` event.
onMouseDownCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseDownCapture = opt "onMouseDownCapture"

-- | Creates an option for the `onMouseEnterCapture` event.
onMouseEnterCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseEnterCapture = opt "onMouseEnterCapture"

-- | Creates an option for the `onMouseLeaveCapture` event.
onMouseLeaveCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseLeaveCapture = opt "onMouseLeaveCapture"

-- | Creates an option for the `onMouseMoveCapture` event.
onMouseMoveCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseMoveCapture = opt "onMouseMoveCapture"

-- | Creates an option for the `onMouseOutCapture` event.
onMouseOutCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseOutCapture = opt "onMouseOutCapture"

-- | Creates an option for the `onMouseOverCapture` event.
onMouseOverCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseOverCapture = opt "onMouseOverCapture"

-- | Creates an option for the `onMouseUpCapture` event.
onMouseUpCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseUpCapture = opt "onMouseUpCapture"

-- | Creates an option for the `onSelectCapture` event.
onSelectCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onSelectCapture = opt "onSelectCapture"

-- | Creates an option for the `onTouchCancelCapture` event.
onTouchCancelCapture
  :: forall s e
   . Option Properties (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchCancelCapture = opt "onTouchCancelCapture"

-- | Creates an option for the `onTouchEndCapture` event.
onTouchEndCapture
  :: forall s e
   . Option Properties (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchEndCapture = opt "onTouchEndCapture"

-- | Creates an option for the `onTouchMoveCapture` event.
onTouchMoveCapture
  :: forall s e
   . Option Properties (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchMoveCapture = opt "onTouchMoveCapture"

-- | Creates an option for the `onTouchStartCapture` event.
onTouchStartCapture
  :: forall s e
   . Option Properties (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchStartCapture = opt "onTouchStartCapture"

-- | Creates an option for the `onScrollCapture` event.
onScrollCapture
  :: forall s e
   . Option Properties (SyntheticUIEvent -> EventHandler s e Unit)
onScrollCapture = opt "onScrollCapture"

-- | Creates an option for the `onWheelCapture` event.
onWheelCapture
  :: forall s e
   . Option Properties (SyntheticWheelEvent -> EventHandler s e Unit)
onWheelCapture = opt "onWheelCapture"
