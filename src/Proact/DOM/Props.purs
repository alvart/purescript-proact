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

accept :: Option Properties String
accept = opt "accept"

acceptCharset :: Option Properties String
acceptCharset = opt "acceptCharset"

accessKey :: Option Properties String
accessKey = opt "accessKey"

action :: Option Properties String
action = opt "action"

allowFullScreen :: Option Properties Boolean
allowFullScreen = opt "allowFullScreen"

allowTransparency :: Option Properties Boolean
allowTransparency = opt "allowTransparency"

alt :: Option Properties String
alt = opt "alt"

async :: Option Properties Boolean
async = opt "async"

autoComplete :: Option Properties String
autoComplete = opt "autoComplete"

autoFocus :: Option Properties Boolean
autoFocus = opt "autoFocus"

autoPlay :: Option Properties Boolean
autoPlay = opt "autoPlay"

capture :: Option Properties Boolean
capture = opt "capture"

cellPadding :: Option Properties String
cellPadding = opt "cellPadding"

cellSpacing :: Option Properties String
cellSpacing = opt "cellSpacing"

charSet :: Option Properties String
charSet = opt "charSet"

challenge :: Option Properties String
challenge = opt "checked"

checked :: Option Properties Boolean
checked = opt "checked"

cite :: Option Properties String
cite = opt "cite"

classID :: Option Properties String
classID = opt "classID"

className :: Option Properties String
className = opt "className"

cols :: Option Properties Int
cols = opt "cols"

colSpan :: Option Properties Int
colSpan = opt "colSpan"

content :: Option Properties String
content = opt "content"

contentEditable :: Option Properties Boolean
contentEditable = opt "contentEditable"

contextMenu :: Option Properties String
contextMenu = opt "contextMenu"

controls :: Option Properties Boolean
controls = opt "controls"

coords :: Option Properties String
coords = opt "coords"

crossOrigin :: Option Properties String
crossOrigin = opt "crossOrigin"

dateTime :: Option Properties String
dateTime = opt "dateTime"

default :: Option Properties Boolean
default = opt "default"

defaultChecked :: Option Properties Boolean
defaultChecked = opt "defaultChecked"

defaultValue :: Option Properties String
defaultValue = opt "defaultValue"

defer :: Option Properties Boolean
defer = opt "defer"

dir :: Option Properties String
dir = opt "dir"

disabled :: Option Properties Boolean
disabled = opt "disabled"

download :: Option Properties String
download = opt "download"

draggable :: Option Properties Boolean
draggable = opt "draggable"

encType :: Option Properties String
encType = opt "encType"

form :: Option Properties String
form = opt "form"

formAction :: Option Properties String
formAction = opt "formAction"

formEncType :: Option Properties String
formEncType = opt "formEncType"

formMethod :: Option Properties String
formMethod = opt "formMethod"

formNoValidate :: Option Properties Boolean
formNoValidate = opt "formNoValidate"

formTarget :: Option Properties String
formTarget = opt "formTarget"

frameBorder :: Option Properties String
frameBorder = opt "frameBorder"

headers :: Option Properties String
headers = opt "headers"

height :: Option Properties String
height = opt "height"

hidden :: Option Properties Boolean
hidden = opt "hidden"

high :: Option Properties String
high = opt "high"

href :: Option Properties String
href = opt "href"

hrefLang :: Option Properties String
hrefLang = opt "hrefLang"

htmlFor :: Option Properties String
htmlFor = opt "htmlFor"

httpEquiv :: Option Properties String
httpEquiv = opt "httpEquiv"

icon :: Option Properties String
icon = opt "icon"

_id :: Option Properties String
_id = opt "id"

inputMode :: Option Properties String
inputMode = opt "inputMode"

integrity :: Option Properties String
integrity = opt "integrity"

is :: Option Properties String
is = opt "is"

key :: Option Properties String
key = opt "key"

keyParams :: Option Properties String
keyParams = opt "keyParams"

keyType :: Option Properties String
keyType = opt "keyType"

kind :: Option Properties String
kind = opt "kind"

label :: Option Properties String
label = opt "label"

lang :: Option Properties String
lang = opt "lang"

list :: Option Properties String
list = opt "list"

loop :: Option Properties Boolean
loop = opt "loop"

low :: Option Properties String
low = opt "low"

manifest :: Option Properties String
manifest = opt "manifest"

marginHeight :: Option Properties String
marginHeight = opt "marginHeight"

marginWidth :: Option Properties String
marginWidth = opt "marginWidth"

max :: Option Properties String
max = opt "max"

maxLength :: Option Properties String
maxLength = opt "maxLength"

media :: Option Properties String
media = opt "media"

mediaGroup :: Option Properties String
mediaGroup = opt "mediaGroup"

method :: Option Properties String
method = opt "method"

min :: Option Properties String
min = opt "min"

minLength :: Option Properties String
minLength = opt "minLength"

multiple :: Option Properties Boolean
multiple = opt "multiple"

muted :: Option Properties Boolean
muted = opt "muted"

name :: Option Properties String
name = opt "name"

nonce :: Option Properties String
nonce = opt "nonce"

noValidate :: Option Properties Boolean
noValidate = opt "noValidate"

open :: Option Properties Boolean
open = opt "open"

optimum :: Option Properties String
optimum = opt "optimum"

pattern :: Option Properties String
pattern = opt "pattern"

placeholder :: Option Properties String
placeholder = opt "placeholder"

poster :: Option Properties String
poster = opt "poster"

preload :: Option Properties String
preload = opt "preload"

profile :: Option Properties String
profile = opt "profile"

radioGroup :: Option Properties String
radioGroup = opt "radioGroup"

readOnly :: Option Properties Boolean
readOnly = opt "readOnly"

rel :: Option Properties String
rel = opt "rel"

required :: Option Properties Boolean
required = opt "required"

reversed :: Option Properties Boolean
reversed = opt "reversed"

role :: Option Properties String
role = opt "role"

rows :: Option Properties Int
rows = opt "rows"

rowSpan :: Option Properties Int
rowSpan = opt "rowSpan"

sandbox :: Option Properties String
sandbox = opt "sandbox"

scope :: Option Properties String
scope = opt "scope"

scoped :: Option Properties Boolean
scoped = opt "scoped"

scrolling :: Option Properties String
scrolling = opt "scrolling"

seamless :: Option Properties Boolean
seamless = opt "seamless"

selected :: Option Properties Boolean
selected = opt "selected"

shape :: Option Properties String
shape = opt "shape"

size :: Option Properties Int
size = opt "size"

sizes :: Option Properties String
sizes = opt "sizes"

span :: Option Properties Int
span = opt "span"

spellCheck :: Option Properties Boolean
spellCheck = opt "spellCheck"

src :: Option Properties String
src = opt "src"

srcDoc :: Option Properties String
srcDoc = opt "srcDoc"

srcLang :: Option Properties String
srcLang = opt "srcLang"

srcSet :: Option Properties String
srcSet = opt "srcSet"

start :: Option Properties Int
start = opt "start"

step :: Option Properties String
step = opt "step"

summary :: Option Properties String
summary = opt "summary"

tabIndex :: Option Properties Int
tabIndex = opt "tabIndex"

target :: Option Properties String
target = opt "target"

title :: Option Properties String
title = opt "title"

_type :: Option Properties String
_type = opt "type"

useMap :: Option Properties String
useMap = opt "useMap"

value :: Option Properties String
value = opt "value"

valueArray :: Option Properties (Array String)
valueArray = opt "value"

width :: Option Properties String
width = opt "width"

wmode :: Option Properties String
wmode = opt "wmode"

wrap :: Option Properties String
wrap = opt "wrap"

-- RDFa Attributes

about :: Option Properties String
about = opt "about"

datatype :: Option Properties String
datatype = opt "datatype"

inlist :: Option Properties String
inlist = opt "inlist"

prefix :: Option Properties String
prefix = opt "prefix"

property :: Option Properties String
property = opt "property"

resource :: Option Properties String
resource = opt "resource"

typeof :: Option Properties String
typeof = opt "typeof"

vocab :: Option Properties String
vocab = opt "vocab"

-- Non-standard Attributes

autoCapitalize :: Option Properties String
autoCapitalize = opt "autoCapitalize"

autoCorrect :: Option Properties String
autoCorrect = opt "autoCorrect"

autoSave :: Option Properties String
autoSave = opt "autoSave"

color :: Option Properties String
color = opt "color"

itemProp :: Option Properties String
itemProp = opt "itemProp"

itemScope :: Option Properties Boolean
itemScope = opt "itemScope"

itemType :: Option Properties String
itemType = opt "itemType"

itemID :: Option Properties String
itemID = opt "itemID"

itemRef :: Option Properties String
itemRef = opt "itemRef"

results :: Option Properties Int
results = opt "results"

security :: Option Properties String
security = opt "security"

unselectable :: Option Properties Boolean
unselectable = opt "unselectable"

-- Events

onAnimationStart
  :: forall s e
   . Option Properties (SyntheticAnimationEvent -> EventHandler s e Unit)
onAnimationStart = opt "onAnimationStart"

onAnimationEnd
  :: forall s e
   . Option Properties (SyntheticAnimationEvent -> EventHandler s e Unit)
onAnimationEnd = opt "onAnimationEnd"

onAnimationIteration
  :: forall s e
   . Option Properties (SyntheticAnimationEvent -> EventHandler s e Unit)
onAnimationIteration = opt "onAnimationIteration"

onTransitionEnd
  :: forall s e
   . Option Properties (SyntheticTransitionEvent -> EventHandler s e Unit)
onTransitionEnd = opt "onTransitionEnd"

onToggle
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onToggle = opt "onToggle"

onError
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onError = opt "onError"

onLoad
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onLoad = opt "onLoad"

onAbort
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onAbort = opt "onAbort"

onCanPlay
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onCanPlay = opt "onCanPlay"

onCanPlayThrough
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onCanPlayThrough = opt "onCanPlayThrough"

onDurationChange
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onDurationChange = opt "onDurationChange"

onEmptied
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onEmptied = opt "onEmptied"

onEncrypted
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onEncrypted = opt "onEncrypted"

onEnded
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onEnded = opt "onEnded"

onLoadedData
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onLoadedData = opt "onLoadedData"

onLoadedMetadata
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onLoadedMetadata = opt "onLoadedMetadata"

onLoadStart
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onLoadStart = opt "onLoadStart"

onPause
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onPause = opt "onPause"

onPlay
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onPlay = opt "onPlay"

onPlaying
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onPlaying = opt "onPlaying"

onProgress
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onProgress = opt "onProgress"

onRateChange
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onRateChange = opt "onRateChange"

onSeeked
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onSeeked = opt "onSeeked"

onSeeking
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onSeeking = opt "onSeeking"

onStalled
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onStalled = opt "onStalled"

onSuspend
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onSuspend = opt "onSuspend"

onTimeUpdate
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onTimeUpdate = opt "onTimeUpdate"

onVolumeChange
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onVolumeChange = opt "onVolumeChange"

onWaiting
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onWaiting = opt "onWaiting"

onCopy
  :: forall s e
   . Option Properties (SyntheticClipboardEvent -> EventHandler s e Unit)
onCopy = opt "onCopy"

onCut
  :: forall s e
   . Option Properties (SyntheticClipboardEvent -> EventHandler s e Unit)
onCut = opt "onCut"

onPaste
  :: forall s e
   . Option Properties (SyntheticClipboardEvent -> EventHandler s e Unit)
onPaste = opt "onPaste"

onCompositionEnd
  :: forall s e
   . Option Properties (SyntheticCompositionEvent -> EventHandler s e Unit)
onCompositionEnd = opt "onCompositionEnd"

onCompositionStart
  :: forall s e
   . Option Properties (SyntheticCompositionEvent -> EventHandler s e Unit)
onCompositionStart = opt "onCompositionStart"

onCompositionUpdate
  :: forall s e
   . Option Properties (SyntheticCompositionEvent -> EventHandler s e Unit)
onCompositionUpdate = opt "onCompositionUpdate"

onKeyDown
  :: forall s e
   . Option Properties (SyntheticKeyboardEvent -> EventHandler s e Unit)
onKeyDown = opt "onKeyDown"

onKeyPress
  :: forall s e
   . Option Properties (SyntheticKeyboardEvent -> EventHandler s e Unit)
onKeyPress = opt "onKeyPress"

onKeyUp
  :: forall s e
   . Option Properties (SyntheticKeyboardEvent -> EventHandler s e Unit)
onKeyUp = opt "onKeyUp"

onFocus
  :: forall s e
   . Option Properties (SyntheticFocusEvent -> EventHandler s e Unit)
onFocus = opt "onFocus"

onBlur
  :: forall s e
   . Option Properties (SyntheticFocusEvent -> EventHandler s e Unit)
onBlur = opt "onBlur"

onChange
  :: forall s e
   . Option Properties (SyntheticInputEvent -> EventHandler s e Unit)
onChange = opt "onChange"

onInput
  :: forall s e
   . Option Properties (SyntheticInputEvent -> EventHandler s e Unit)
onInput = opt "onInput"

onInvalid
  :: forall s e
   . Option Properties (SyntheticInputEvent -> EventHandler s e Unit)
onInvalid = opt "onInvalid"

onSubmit
  :: forall s e
   . Option Properties (SyntheticInputEvent -> EventHandler s e Unit)
onSubmit = opt "onSubmit"

onClick
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onClick = opt "onClick"

onContextMenu
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onContextMenu = opt "onContextMenu"

onDoubleClick
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDoubleClick = opt "onDoubleClick"

onDrag
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDrag = opt "onDrag"

onDragEnd
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragEnd = opt "onDragEnd"

onDragEnter
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragEnter = opt "onDragEnter"

onDragExit
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragExit = opt "onDragExit"

onDragLeave
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragLeave = opt "onDragLeave"

onDragOver
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragOver = opt "onDragOver"

onDragStart
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragStart = opt "onDragStart"

onDrop
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDrop = opt "onDrop"

onMouseDown
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseDown = opt "onMouseDown"

onMouseEnter
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseEnter = opt "onMouseEnter"

onMouseLeave
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseLeave = opt "onMouseLeave"

onMouseMove
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseMove = opt "onMouseMove"

onMouseOut
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseOut = opt "onMouseOut"

onMouseOver
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseOver = opt "onMouseOver"

onMouseUp
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseUp = opt "onMouseUp"

onSelect
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onSelect = opt "onSelect"

onTouchCancel
  :: forall s e
   . Option Properties (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchCancel = opt "onTouchCancel"

onTouchEnd
  :: forall s e
   . Option Properties (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchEnd = opt "onTouchEnd"

onTouchMove
  :: forall s e
   . Option Properties (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchMove = opt "onTouchMove"

onTouchStart
  :: forall s e
   . Option Properties (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchStart = opt "onTouchStart"

onScroll
  :: forall s e
   . Option Properties (SyntheticUIEvent -> EventHandler s e Unit)
onScroll = opt "onScroll"

onWheel
  :: forall s e
   . Option Properties (SyntheticWheelEvent -> EventHandler s e Unit)
onWheel = opt "onWheel"

onAnimationStartCapture
  :: forall s e
   . Option Properties (SyntheticAnimationEvent -> EventHandler s e Unit)
onAnimationStartCapture = opt "onAnimationStartCapture"

onAnimationEndCapture
  :: forall s e
   . Option Properties (SyntheticAnimationEvent -> EventHandler s e Unit)
onAnimationEndCapture = opt "onAnimationEndCapture"

onAnimationIterationCapture
  :: forall s e
   . Option Properties (SyntheticAnimationEvent -> EventHandler s e Unit)
onAnimationIterationCapture = opt "onAnimationIterationCapture"

onTransitionEndCapture
  :: forall s e
   . Option Properties (SyntheticTransitionEvent -> EventHandler s e Unit)
onTransitionEndCapture = opt "onTransitionEndCapture"

onToggleCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onToggleCapture = opt "onToggleCapture"

onErrorCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onErrorCapture = opt "onErrorCapture"

onLoadCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onLoadCapture = opt "onLoadCapture"

onAbortCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onAbortCapture = opt "onAbortCapture"

onCanPlayCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onCanPlayCapture = opt "onCanPlayCapture"

onCanPlayThroughCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onCanPlayThroughCapture = opt "onCanPlayThroughCapture"

onDurationChangeCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onDurationChangeCapture = opt "onDurationChangeCapture"

onEmptiedCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onEmptiedCapture = opt "onEmptiedCapture"

onEncryptedCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onEncryptedCapture = opt "onEncryptedCapture"

onEndedCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onEndedCapture = opt "onEndedCapture"

onLoadedDataCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onLoadedDataCapture = opt "onLoadedDataCapture"

onLoadedMetadataCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onLoadedMetadataCapture = opt "onLoadedMetadataCapture"

onLoadStartCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onLoadStartCapture = opt "onLoadStartCapture"

onPauseCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onPauseCapture = opt "onPauseCapture"

onPlayCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onPlayCapture = opt "onPlayCapture"

onPlayingCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onPlayingCapture = opt "onPlayingCapture"

onProgressCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onProgressCapture = opt "onProgressCapture"

onRateChangeCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onRateChangeCapture = opt "onRateChangeCapture"

onSeekedCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onSeekedCapture = opt "onSeekedCapture"

onSeekingCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onSeekingCapture = opt "onSeekingCapture"

onStalledCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onStalledCapture = opt "onStalledCapture"

onSuspendCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onSuspendCapture = opt "onSuspendCapture"

onTimeUpdateCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onTimeUpdateCapture = opt "onTimeUpdateCapture"

onVolumeChangeCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onVolumeChangeCapture = opt "onVolumeChangeCapture"

onWaitingCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onWaitingCapture = opt "onWaitingCapture"

onCopyCapture
  :: forall s e
   . Option Properties (SyntheticClipboardEvent -> EventHandler s e Unit)
onCopyCapture = opt "onCopyCapture"

onCutCapture
  :: forall s e
   . Option Properties (SyntheticClipboardEvent -> EventHandler s e Unit)
onCutCapture = opt "onCutCapture"

onPasteCapture
  :: forall s e
   . Option Properties (SyntheticClipboardEvent -> EventHandler s e Unit)
onPasteCapture = opt "onPasteCapture"

onCompositionEndCapture
  :: forall s e
   . Option Properties (SyntheticCompositionEvent -> EventHandler s e Unit)
onCompositionEndCapture = opt "onCompositionEndCapture"

onCompositionStartCapture
  :: forall s e
   . Option Properties (SyntheticCompositionEvent -> EventHandler s e Unit)
onCompositionStartCapture = opt "onCompositionStartCapture"

onCompositionUpdateCapture
  :: forall s e
   . Option Properties (SyntheticCompositionEvent -> EventHandler s e Unit)
onCompositionUpdateCapture = opt "onCompositionUpdateCapture"

onKeyDownCapture
  :: forall s e
   . Option Properties (SyntheticKeyboardEvent -> EventHandler s e Unit)
onKeyDownCapture = opt "onKeyDownCapture"

onKeyPressCapture
  :: forall s e
   . Option Properties (SyntheticKeyboardEvent -> EventHandler s e Unit)
onKeyPressCapture = opt "onKeyPressCapture"

onKeyUpCapture
  :: forall s e
   . Option Properties (SyntheticKeyboardEvent -> EventHandler s e Unit)
onKeyUpCapture = opt "onKeyUpCapture"

onFocusCapture
  :: forall s e
   . Option Properties (SyntheticFocusEvent -> EventHandler s e Unit)
onFocusCapture = opt "onFocusCapture"

onBlurCapture
  :: forall s e
   . Option Properties (SyntheticFocusEvent -> EventHandler s e Unit)
onBlurCapture = opt "onBlurCapture"

onChangeCapture
  :: forall s e
   . Option Properties (SyntheticInputEvent -> EventHandler s e Unit)
onChangeCapture = opt "onChangeCapture"

onInputCapture
  :: forall s e
   . Option Properties (SyntheticInputEvent -> EventHandler s e Unit)
onInputCapture = opt "onInputCapture"

onInvalidCapture
  :: forall s e
   . Option Properties (SyntheticInputEvent -> EventHandler s e Unit)
onInvalidCapture = opt "onInvalidCapture"

onSubmitCapture
  :: forall s e
   . Option Properties (SyntheticInputEvent -> EventHandler s e Unit)
onSubmitCapture = opt "onSubmitCapture"

onClickCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onClickCapture = opt "onClickCapture"

onContextMenuCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onContextMenuCapture = opt "onContextMenuCapture"

onDoubleClickCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDoubleClickCapture = opt "onDoubleClickCapture"

onDragCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragCapture = opt "onDragCapture"

onDragEndCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragEndCapture = opt "onDragEndCapture"

onDragEnterCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragEnterCapture = opt "onDragEnterCapture"

onDragExitCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragExitCapture = opt "onDragExitCapture"

onDragLeaveCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragLeaveCapture = opt "onDragLeaveCapture"

onDragOverCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragOverCapture = opt "onDragOverCapture"

onDragStartCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDragStartCapture = opt "onDragStartCapture"

onDropCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onDropCapture = opt "onDropCapture"

onMouseDownCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseDownCapture = opt "onMouseDownCapture"

onMouseEnterCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseEnterCapture = opt "onMouseEnterCapture"

onMouseLeaveCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseLeaveCapture = opt "onMouseLeaveCapture"

onMouseMoveCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseMoveCapture = opt "onMouseMoveCapture"

onMouseOutCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseOutCapture = opt "onMouseOutCapture"

onMouseOverCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseOverCapture = opt "onMouseOverCapture"

onMouseUpCapture
  :: forall s e
   . Option Properties (SyntheticMouseEvent -> EventHandler s e Unit)
onMouseUpCapture = opt "onMouseUpCapture"

onSelectCapture
  :: forall s e . Option Properties (SyntheticEvent -> EventHandler s e Unit)
onSelectCapture = opt "onSelectCapture"

onTouchCancelCapture
  :: forall s e
   . Option Properties (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchCancelCapture = opt "onTouchCancelCapture"

onTouchEndCapture
  :: forall s e
   . Option Properties (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchEndCapture = opt "onTouchEndCapture"

onTouchMoveCapture
  :: forall s e
   . Option Properties (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchMoveCapture = opt "onTouchMoveCapture"

onTouchStartCapture
  :: forall s e
   . Option Properties (SyntheticTouchEvent -> EventHandler s e Unit)
onTouchStartCapture = opt "onTouchStartCapture"

onScrollCapture
  :: forall s e
   . Option Properties (SyntheticUIEvent -> EventHandler s e Unit)
onScrollCapture = opt "onScrollCapture"

onWheelCapture
  :: forall s e
   . Option Properties (SyntheticWheelEvent -> EventHandler s e Unit)
onWheelCapture = opt "onWheelCapture"
