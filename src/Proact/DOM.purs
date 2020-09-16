{-
  @license MIT
  DOM.purs
-}

module Proact.DOM
  ( createElement
  , fragment
  , text
  , a
  , a'
  , abbr
  , abbr'
  , address
  , address'
  , area
  , area'
  , article
  , article'
  , aside
  , aside'
  , audio
  , audio'
  , b
  , b'
  , base
  , base'
  , bdi
  , bdi'
  , bdo
  , bdo'
  , big
  , big'
  , blockquote
  , blockquote'
  , body
  , body'
  , br
  , br'
  , button
  , button'
  , canvas
  , canvas'
  , caption
  , caption'
  , cite
  , cite'
  , code
  , code'
  , col
  , col'
  , colgroup
  , colgroup'
  , _data
  , _data'
  , datalist
  , datalist'
  , dd
  , dd'
  , del
  , del'
  , details
  , details'
  , dfn
  , dfn'
  , dialog
  , dialog'
  , div
  , div'
  , dl
  , dl'
  , dt
  , dt'
  , em
  , em'
  , embed
  , embed'
  , fieldset
  , fieldset'
  , figcaption
  , figcaption'
  , figure
  , figure'
  , footer
  , footer'
  , form
  , form'
  , h1
  , h1'
  , h2
  , h2'
  , h3
  , h3'
  , h4
  , h4'
  , h5
  , h5'
  , h6
  , h6'
  , head
  , head'
  , header
  , header'
  , hr
  , hr'
  , html
  , html'
  , i
  , i'
  , iframe
  , iframe'
  , img
  , img'
  , input
  , input'
  , ins
  , ins'
  , kbd
  , kbd'
  , keygen
  , keygen'
  , label
  , label'
  , legend
  , legend'
  , li
  , li'
  , link
  , link'
  , main
  , main'
  , map
  , map'
  , mark
  , mark'
  , menu
  , menu'
  , menuitem
  , menuitem'
  , meta
  , meta'
  , meter
  , meter'
  , nav
  , nav'
  , noscript
  , noscript'
  , object
  , object'
  , ol
  , ol'
  , optgroup
  , optgroup'
  , option
  , option'
  , output
  , output'
  , p
  , p'
  , param
  , param'
  , picture
  , picture'
  , pre
  , pre'
  , progress
  , progress'
  , q
  , q'
  , rp
  , rp'
  , rt
  , rt'
  , ruby
  , ruby'
  , s
  , s'
  , samp
  , samp'
  , script
  , script'
  , section
  , section'
  , select
  , select'
  , small
  , small'
  , source
  , source'
  , span
  , span'
  , strong
  , strong'
  , style
  , style'
  , sub
  , sub'
  , summary
  , summary'
  , sup
  , sup'
  , table
  , table'
  , tbody
  , tbody'
  , td
  , td'
  , textarea
  , textarea'
  , tfoot
  , tfoot'
  , th
  , th'
  , thead
  , thead'
  , time
  , time'
  , title
  , title'
  , tr
  , tr'
  , track
  , track'
  , u
  , u'
  , ul
  , ul'
  , var
  , var'
  , video
  , video'
  , wbr
  , wbr'
  )
where

import Data.Foldable (fold)
import Data.Options (Options, options)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Fiber)
import Foreign (Foreign)
import Prelude (($), (>>=), (<<<), Unit, bind, mempty, pure)
import Proact (EventHandler, PComponent, dispatcher) as P
import Proact.DOM.Props (Properties)
import React (ReactClass, ReactElement)
import React.DOM (text) as R
import Unsafe.Coerce (unsafeCoerce)

-- | Creates a Proact component from a React class, an array of Options and an
-- | array of React children.
createElement
  :: forall s t o p e1 e2
   . ReactClass p
  -> Array (Options o)
  -> Array ReactElement
  -> P.PComponent s t e1 e2 ReactElement
createElement class_ props children =
  do
  dispatcher <- P.dispatcher
  pure $ _createElement class_ dispatcher (options $ fold props) children

-- | Creates a React fragment from an array of elements. Used in conjunction
-- | with `focus` and `iFocus`.
fragment
  :: forall s t e1 e2
   . P.PComponent s t e1 e2 (Array ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
fragment children = children >>= createElement _fragment mempty

-- | Creates a Proact component representing React text.
text :: forall s t e1 e2 . String -> P.PComponent s t e1 e2 ReactElement
text = pure <<< R.text

-- | Creates a Proact component for the `a` React element.
a
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
a = createElement' "a"

-- | A property-less constructor for `a`.
a'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
a' = a mempty

-- | Creates a Proact component for the `abbr` React element.
abbr
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
abbr = createElement' "abbr"

-- | A property-less constructor for `abbr`.
abbr'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
abbr' = abbr mempty

-- | Creates a Proact component for the `address` React element.
address
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
address = createElement' "address"

-- | A property-less constructor for `address`.
address'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
address' = address mempty

-- | Creates a Proact component for the `area` React element.
area
  :: forall s t e1 e2
   . Array (Options (Properties t e2)) -> P.PComponent s t e1 e2 ReactElement
area props = createElement' "area" props [ ]

-- | A property-less constructor for `area`.
area' :: forall s t e1 e2 . P.PComponent s t e1 e2 ReactElement
area' = area mempty

-- | Creates a Proact component for the `article` React element.
article
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
article = createElement' "article"

-- | A property-less constructor for `article`.
article'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
article' = article mempty

-- | Creates a Proact component for the `aside` React element.
aside
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
aside = createElement' "aside"

-- | A property-less constructor for `aside`.
aside'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
aside' = aside mempty

-- | Creates a Proact component for the `audio` React element.
audio
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
audio = createElement' "audio"

-- | A property-less constructor for `audio`.
audio'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
audio' = audio mempty

-- | Creates a Proact component for the `b` React element.
b
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
b = createElement' "b"

-- | A property-less constructor for `b`.
b'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
b' = b mempty

-- | Creates a Proact component for the `base` React element.
base
  :: forall s t e1 e2
   . Array (Options (Properties t e2)) -> P.PComponent s t e1 e2 ReactElement
base props = createElement' "base" props [ ]

-- | A property-less constructor for `base`.
base' :: forall s t e1 e2 . P.PComponent s t e1 e2 ReactElement
base' = base mempty

-- | Creates a Proact component for the `bdi` React element.
bdi
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
bdi = createElement' "bdi"

-- | A property-less constructor for `bdi`.
bdi'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
bdi' = bdi mempty

-- | Creates a Proact component for the `bdo` React element.
bdo
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
bdo = createElement' "bdo"

-- | A property-less constructor for `bdo`.
bdo'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
bdo' = bdo mempty

-- | Creates a Proact component for the `big` React element.
big
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
big = createElement' "big"

-- | A property-less constructor for `big`.
big'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
big' = big mempty

-- | Creates a Proact component for the `blockquote` React element.
blockquote
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
blockquote = createElement' "blockquote"

-- | A property-less constructor for `blockquote`.
blockquote'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
blockquote' = blockquote mempty

-- | Creates a Proact component for the `body` React element.
body
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
body = createElement' "body"

-- | A property-less constructor for `body`.
body'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
body' = body mempty

-- | Creates a Proact component for the `br` React element.
br
  :: forall s t e1 e2
   . Array (Options (Properties t e2)) -> P.PComponent s t e1 e2 ReactElement
br props = createElement' "br" props [ ]

-- | A property-less constructor for `br`.
br' :: forall s t e1 e2 . P.PComponent s t e1 e2 ReactElement
br' = br mempty

-- | Creates a Proact component for the `button` React element.
button
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
button = createElement' "button"

-- | A property-less constructor for `button`.
button'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
button' = button mempty

-- | Creates a Proact component for the `canvas` React element.
canvas
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
canvas = createElement' "canvas"

-- | A property-less constructor for `canvas`.
canvas'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
canvas' = canvas mempty

-- | Creates a Proact component for the `caption` React element.
caption
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
caption = createElement' "caption"

-- | A property-less constructor for `caption`.
caption'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
caption' = caption mempty

-- | Creates a Proact component for the `cite` React element.
cite
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
cite = createElement' "cite"

-- | A property-less constructor for `cite`.
cite'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
cite' = cite mempty

-- | Creates a Proact component for the `code` React element.
code
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
code = createElement' "code"

-- | A property-less constructor for `code`.
code'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
code' = code mempty

-- | Creates a Proact component for the `col` React element.
col
  :: forall s t e1 e2
   . Array (Options (Properties t e2)) -> P.PComponent s t e1 e2 ReactElement
col props = createElement' "col" props [ ]

-- | A property-less constructor for `col`.
col' :: forall s t e1 e2 . P.PComponent s t e1 e2 ReactElement
col' = col mempty

-- | Creates a Proact component for the `colgroup` React element.
colgroup
  :: forall s t e1 e2 
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
colgroup = createElement' "colgroup"

-- | A property-less constructor for `colgroup`.
colgroup'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
colgroup' = colgroup mempty

-- | Creates a Proact component for the `_data` React element.
_data
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
_data = createElement' "_data"

-- | A property-less constructor for `_data`.
_data'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
_data' = _data mempty

-- | Creates a Proact component for the `datalist` React element.
datalist
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
datalist = createElement' "datalist"

-- | A property-less constructor for `datalist`.
datalist'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
datalist' = datalist mempty

-- | Creates a Proact component for the `dd` React element.
dd
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
dd = createElement' "dd"

-- | A property-less constructor for `dd`.
dd'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
dd' = dd mempty

-- | Creates a Proact component for the `del` React element.
del
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
del = createElement' "del"

-- | A property-less constructor for `del`.
del'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
del' = del mempty

-- | Creates a Proact component for the `details` React element.
details
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
details = createElement' "details"

-- | A property-less constructor for `details`.
details'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
details' = details mempty

-- | Creates a Proact component for the `dfn` React element.
dfn
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
dfn = createElement' "dfn"

-- | A property-less constructor for `dfn`.
dfn'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
dfn' = dfn mempty

-- | Creates a Proact component for the `dialog` React element.
dialog
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
dialog = createElement' "dialog"

-- | A property-less constructor for `dialog`.
dialog'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
dialog' = dialog mempty

-- | Creates a Proact component for the `div` React element.
div
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
div = createElement' "div"

-- | A property-less constructor for `div`.
div'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
div' = div mempty

-- | Creates a Proact component for the `dl` React element.
dl
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
dl = createElement' "dl"

-- | A property-less constructor for `dl`.
dl'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
dl' = dl mempty

-- | Creates a Proact component for the `dt` React element.
dt
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
dt = createElement' "dt"

-- | A property-less constructor for `dt`.
dt'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
dt' = dt mempty

-- | Creates a Proact component for the `em` React element.
em
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
em = createElement' "em"

-- | A property-less constructor for `em`.
em'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
em' = em mempty

-- | Creates a Proact component for the `embed` React element.
embed
  :: forall s t e1 e2
   . Array (Options (Properties t e2)) -> P.PComponent s t e1 e2 ReactElement
embed props = createElement' "embed" props [ ]

-- | A property-less constructor for `embed`.
embed' :: forall s t e1 e2 . P.PComponent s t e1 e2 ReactElement
embed' = embed mempty

-- | Creates a Proact component for the `fieldset` React element.
fieldset
  :: forall s t e1 e2
   .  Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
fieldset = createElement' "fieldset"

-- | A property-less constructor for `fieldset`.
fieldset'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
fieldset' = fieldset mempty

-- | Creates a Proact component for the `figcaption` React element.
figcaption
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
figcaption = createElement' "figcaption"

-- | A property-less constructor for `figcaption`.
figcaption'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
figcaption' = figcaption mempty

-- | Creates a Proact component for the `figure` React element.
figure
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
figure = createElement' "figure"

-- | A property-less constructor for `figure`.
figure'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
figure' = figure mempty

-- | Creates a Proact component for the `footer` React element.
footer
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
footer = createElement' "footer"

-- | A property-less constructor for `footer`.
footer'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
footer' = footer mempty

-- | Creates a Proact component for the `form` React element.
form
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
form = createElement' "form"

-- | A property-less constructor for `form`.
form'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
form' = form mempty

-- | Creates a Proact component for the `h1` React element.
h1
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
h1 = createElement' "h1"

-- | A property-less constructor for `h1`.
h1'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
h1' = h1 mempty

-- | Creates a Proact component for the `h2` React element.
h2
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
h2 = createElement' "h2"

-- | A property-less constructor for `h2`.
h2'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
h2' = h2 mempty

-- | Creates a Proact component for the `h3` React element.
h3
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
h3 = createElement' "h3"

-- | A property-less constructor for `h3`.
h3'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
h3' = h3 mempty

-- | Creates a Proact component for the `h4` React element.
h4
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
h4 = createElement' "h4"

-- | A property-less constructor for `h4`.
h4'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
h4' = h4 mempty

-- | Creates a Proact component for the `h5` React element.
h5
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
h5 = createElement' "h5"

-- | A property-less constructor for `h5`.
h5'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
h5' = h5 mempty

-- | Creates a Proact component for the `h6` React element.
h6
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
h6 = createElement' "h6"

-- | A property-less constructor for `h6`.
h6'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
h6' = h6 mempty

-- | Creates a Proact component for the `head` React element.
head
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
head = createElement' "head"

-- | A property-less constructor for `head`.
head'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
head' = head mempty

-- | Creates a Proact component for the `header` React element.
header
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
header = createElement' "header"

-- | A property-less constructor for `header`.
header'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
header' = header mempty

-- | Creates a Proact component for the `hr` React element.
hr
  :: forall s t e1 e2
   . Array (Options (Properties t e2)) -> P.PComponent s t e1 e2 ReactElement
hr props = createElement' "hr" props [ ]

-- | A property-less constructor for `hr`.
hr' :: forall s t e1 e2 . P.PComponent s t e1 e2 ReactElement
hr' = hr mempty

-- | Creates a Proact component for the `html` React element.
html
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
html = createElement' "html"

-- | A property-less constructor for `html`.
html'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
html' = html mempty

-- | Creates a Proact component for the `i` React element.
i
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
i = createElement' "i"

-- | A property-less constructor for `i`.
i'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
i' = i mempty

-- | Creates a Proact component for the `iframe` React element.
iframe
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
iframe = createElement' "iframe"

-- | A property-less constructor for `iframe`.
iframe'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
iframe' = iframe mempty

-- | Creates a Proact component for the `img` React element.
img
  :: forall s t e1 e2
   . Array (Options (Properties t e2)) -> P.PComponent s t e1 e2 ReactElement
img props = createElement' "img" props [ ]

-- | A property-less constructor for `img`.
img' :: forall s t e1 e2 . P.PComponent s t e1 e2 ReactElement
img' = img mempty

-- | Creates a Proact component for the `input` React element.
input
  :: forall s t e1 e2
   . Array (Options (Properties t e2)) -> P.PComponent s t e1 e2 ReactElement
input props = createElement' "input" props [ ]

-- | A property-less constructor for `input`.
input' :: forall s t e1 e2 . P.PComponent s t e1 e2 ReactElement
input' = input mempty

-- | Creates a Proact component for the `ins` React element.
ins
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
ins = createElement' "ins"

-- | A property-less constructor for `ins`.
ins'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
ins' = ins mempty

-- | Creates a Proact component for the `kbd` React element.
kbd
  :: forall s t e1 e2
   .  Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
kbd = createElement' "kbd"

-- | A property-less constructor for `kbd`.
kbd'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
kbd' = kbd mempty

-- | Creates a Proact component for the `keygen` React element.
keygen
  :: forall s t e1 e2
   . Array (Options (Properties t e2)) -> P.PComponent s t e1 e2 ReactElement
keygen props = createElement' "keygen" props [ ]

-- | A property-less constructor for `keygen`.
keygen' :: forall s t e1 e2 . P.PComponent s t e1 e2 ReactElement
keygen' = keygen mempty

-- | Creates a Proact component for the `label` React element.
label
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
label = createElement' "label"

-- | A property-less constructor for `label`.
label'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
label' = label mempty

-- | Creates a Proact component for the `legend` React element.
legend
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
legend = createElement' "legend"

-- | A property-less constructor for `legend`.
legend'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
legend' = legend mempty

-- | Creates a Proact component for the `li` React element.
li
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
li = createElement' "li"

-- | A property-less constructor for `li`.
li'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
li' = li mempty

-- | Creates a Proact component for the `link` React element.
link
  :: forall s t e1 e2
   . Array (Options (Properties t e2)) -> P.PComponent s t e1 e2 ReactElement
link props = createElement' "link" props [ ]

-- | A property-less constructor for `link`.
link' :: forall s t e1 e2 . P.PComponent s t e1 e2 ReactElement
link' = link mempty

-- | Creates a Proact component for the `main` React element.
main
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
main = createElement' "main"

-- | A property-less constructor for `main`.
main'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
main' = main mempty

-- | Creates a Proact component for the `map` React element.
map
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
map = createElement' "map"

-- | A property-less constructor for `map`.
map'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
map' = map mempty

-- | Creates a Proact component for the `mark` React element.
mark
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
mark = createElement' "mark"

-- | A property-less constructor for `mark`.
mark'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
mark' = mark mempty

-- | Creates a Proact component for the `menu` React element.
menu
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
menu = createElement' "menu"

-- | A property-less constructor for `menu`.
menu'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
menu' = menu mempty

-- | Creates a Proact component for the `menuitem` React element.
menuitem
  :: forall s t e1 e2
   . Array (Options (Properties t e2)) -> P.PComponent s t e1 e2 ReactElement
menuitem props = createElement' "menuitem" props [ ]

-- | A property-less constructor for `menuitem`.
menuitem' :: forall s t e1 e2 . P.PComponent s t e1 e2 ReactElement
menuitem' = menuitem mempty

-- | Creates a Proact component for the `meta` React element.
meta
  :: forall s t e1 e2
   . Array (Options (Properties t e2)) -> P.PComponent s t e1 e2 ReactElement
meta props = createElement' "meta" props [ ]

-- | A property-less constructor for `meta`.
meta' :: forall s t e1 e2 . P.PComponent s t e1 e2 ReactElement
meta' = meta mempty

-- | Creates a Proact component for the `meter` React element.
meter
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
meter = createElement' "meter"

-- | A property-less constructor for `meter`.
meter'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
meter' = meter mempty

-- | Creates a Proact component for the `nav` React element.
nav
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
nav = createElement' "nav"

-- | A property-less constructor for `nav`.
nav'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
nav' = nav mempty

-- | Creates a Proact component for the `noscript` React element.
noscript
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
noscript = createElement' "noscript"

-- | A property-less constructor for `noscript`.
noscript'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
noscript' = noscript mempty

-- | Creates a Proact component for the `object` React element.
object
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
object = createElement' "object"

-- | A property-less constructor for `object`.
object'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
object' = object mempty

-- | Creates a Proact component for the `ol` React element.
ol
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
ol = createElement' "ol"

-- | A property-less constructor for `ol`.
ol'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
ol' = ol mempty

-- | Creates a Proact component for the `optgroup` React element.
optgroup
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
optgroup = createElement' "optgroup"

-- | A property-less constructor for `optgroup`.
optgroup'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
optgroup' = optgroup mempty

-- | Creates a Proact component for the `option` React element.
option
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
option = createElement' "option"

-- | A property-less constructor for `option`.
option'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
option' = option mempty

-- | Creates a Proact component for the `output` React element.
output
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
output = createElement' "output"

-- | A property-less constructor for `output`.
output'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
output' = output mempty

-- | Creates a Proact component for the `p` React element.
p
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
p = createElement' "p"

-- | A property-less constructor for `p`.
p'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
p' = p mempty

-- | Creates a Proact component for the `param` React element.
param
  :: forall s t e1 e2
   . Array (Options (Properties t e2)) -> P.PComponent s t e1 e2 ReactElement
param props = createElement' "param" props [ ]

-- | A property-less constructor for `param`.
param' :: forall s t e1 e2 . P.PComponent s t e1 e2 ReactElement
param' = param mempty

-- | Creates a Proact component for the `picture` React element.
picture
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
picture = createElement' "picture"

-- | A property-less constructor for `picture`.
picture'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
picture' = picture mempty

-- | Creates a Proact component for the `pre` React element.
pre
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
pre = createElement' "pre"

-- | A property-less constructor for `pre`.
pre'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
pre' = pre mempty

-- | Creates a Proact component for the `progress` React element.
progress
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
progress = createElement' "progress"

-- | A property-less constructor for `progress`.
progress'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
progress' = progress mempty

-- | Creates a Proact component for the `q` React element.
q
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
q = createElement' "q"

-- | A property-less constructor for `q`.
q'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
q' = q mempty

-- | Creates a Proact component for the `rp` React element.
rp
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
rp = createElement' "rp"

-- | A property-less constructor for `rp`.
rp'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
rp' = rp mempty

-- | Creates a Proact component for the `rt` React element.
rt
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
rt = createElement' "rt"

-- | A property-less constructor for `rt`.
rt'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
rt' = rt mempty

-- | Creates a Proact component for the `ruby` React element.
ruby
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
ruby = createElement' "ruby"

-- | A property-less constructor for `ruby`.
ruby'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
ruby' = ruby mempty

-- | Creates a Proact component for the `s` React element.
s
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
s = createElement' "s"

-- | A property-less constructor for `s`.
s'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
s' = s mempty

-- | Creates a Proact component for the `samp` React element.
samp
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
samp = createElement' "samp"

-- | A property-less constructor for `samp`.
samp'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
samp' = samp mempty

-- | Creates a Proact component for the `script` React element.
script
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
script = createElement' "script"

-- | A property-less constructor for `script`.
script'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
script' = script mempty

-- | Creates a Proact component for the `section` React element.
section
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
section = createElement' "section"

-- | A property-less constructor for `section`.
section'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
section' = section mempty

-- | Creates a Proact component for the `select` React element.
select
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
select = createElement' "select"

-- | A property-less constructor for `select`.
select'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
select' = select mempty

-- | Creates a Proact component for the `small` React element.
small
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
small = createElement' "small"

-- | A property-less constructor for `small`.
small'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
small' = small mempty

-- | Creates a Proact component for the `source` React element.
source
  :: forall s t e1 e2
   . Array (Options (Properties t e2)) -> P.PComponent s t e1 e2 ReactElement
source props = createElement' "source" props [ ]

-- | A property-less constructor for `source`.
source' :: forall s t e1 e2 . P.PComponent s t e1 e2 ReactElement
source' = source mempty

-- | Creates a Proact component for the `span` React element.
span
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
span = createElement' "span"

-- | A property-less constructor for `span`.
span'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
span' = span mempty

-- | Creates a Proact component for the `strong` React element.
strong
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
strong = createElement' "strong"

-- | A property-less constructor for `strong`.
strong'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
strong' = strong mempty

-- | Creates a Proact component for the `style` React element.
style
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
style = createElement' "style"

-- | A property-less constructor for `style`.
style'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
style' = style mempty

-- | Creates a Proact component for the `sub` React element.
sub
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
sub = createElement' "sub"

-- | A property-less constructor for `sub`.
sub'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
sub' = sub mempty

-- | Creates a Proact component for the `summary` React element.
summary
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
summary = createElement' "summary"

-- | A property-less constructor for `summary`.
summary'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
summary' = summary mempty

-- | Creates a Proact component for the `sup` React element.
sup
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
sup = createElement' "sup"

-- | A property-less constructor for `sup`.
sup'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
sup' = sup mempty

-- | Creates a Proact component for the `table` React element.
table
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
table = createElement' "table"

-- | A property-less constructor for `table`.
table'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
table' = table mempty

-- | Creates a Proact component for the `tbody` React element.
tbody
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
tbody = createElement' "tbody"

-- | A property-less constructor for `tbody`.
tbody'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
tbody' = tbody mempty

-- | Creates a Proact component for the `td` React element.
td
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
td = createElement' "td"

-- | A property-less constructor for `td`.
td'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
td' = td mempty

-- | Creates a Proact component for the `textarea` React element.
textarea
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
textarea = createElement' "textarea"

-- | A property-less constructor for `textarea`.
textarea'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
textarea' = textarea mempty

-- | Creates a Proact component for the `tfoot` React element.
tfoot
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
tfoot = createElement' "tfoot"

-- | A property-less constructor for `tfoot`.
tfoot'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
tfoot' = tfoot mempty

-- | Creates a Proact component for the `th` React element.
th
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
th = createElement' "th"

-- | A property-less constructor for `th`.
th'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
th' = th mempty

-- | Creates a Proact component for the `thead` React element.
thead
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
thead = createElement' "thead"

-- | A property-less constructor for `thead`.
thead'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
thead' = thead mempty

-- | Creates a Proact component for the `time` React element.
time
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
time = createElement' "time"

-- | A property-less constructor for `time`.
time'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
time' = time mempty

-- | Creates a Proact component for the `title` React element.
title
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
title = createElement' "title"

-- | A property-less constructor for `title`.
title'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
title' = title mempty

-- | Creates a Proact component for the `tr` React element.
tr
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
tr = createElement' "tr"

-- | A property-less constructor for `tr`.
tr'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
tr' = tr mempty

-- | Creates a Proact component for the `track` React element.
track
  :: forall s t e1 e2
   . Array (Options (Properties t e2)) -> P.PComponent s t e1 e2 ReactElement
track props = createElement' "track" props [ ]

-- | A property-less constructor for `track`.
track' :: forall s t e1 e2 . P.PComponent s t e1 e2 ReactElement
track' = track mempty

-- | Creates a Proact component for the `u` React element.
u
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
u = createElement' "u"

-- | A property-less constructor for `u`.
u'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
u' = u mempty

-- | Creates a Proact component for the `ul` React element.
ul
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
ul = createElement' "ul"

-- | A property-less constructor for `ul`.
ul'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
ul' = ul mempty

-- | Creates a Proact component for the `var` React element.
var
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
var = createElement' "var"

-- | A property-less constructor for `var`.
var'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
var' = var mempty

-- | Creates a Proact component for the `video` React element.
video
  :: forall s t e1 e2
   . Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
video = createElement' "video"

-- | A property-less constructor for `video`.
video'
  :: forall s t e1 e2
   . Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
video' = video mempty

-- | Creates a Proact component for the `wbr` React element.
wbr
  :: forall s t e1 e2
   . Array (Options (Properties t e2)) -> P.PComponent s t e1 e2 ReactElement
wbr props = createElement' "wbr" props [ ]

-- | A property-less constructor for `wbr`.
wbr' :: forall s t e1 e2 . P.PComponent s t e1 e2 ReactElement
wbr' = wbr mempty

-- Creates a React element from a class, dispatcher, list of options and an
-- array of React children.
foreign import _createElement
  :: forall p s e
   . ReactClass p
  -> (P.EventHandler s e Unit -> Effect (Fiber Unit))
  -> Foreign
  -> Array ReactElement
  -> ReactElement

-- The Fragment React class.
foreign import _fragment :: forall p . ReactClass p

-- Creates a Proact component from a tagged React class.
createElement'
  :: forall s t e1 e2
   . String
  -> Array (Options (Properties t e2))
  -> Array (P.PComponent s t e1 e2 ReactElement)
  -> P.PComponent s t e1 e2 ReactElement
createElement' class_ props children =
  sequence children >>= createElement (tag class_) props

-- Constructs a React class from a label.
tag :: forall p . String -> ReactClass p
tag = unsafeCoerce
