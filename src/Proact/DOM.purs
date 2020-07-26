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
import Foreign (Foreign)
import Prelude (map) as P
import Prelude (($), (>>=), (<<<), Unit, bind, mempty, pure)
import Proact (EventHandler, PComponent, dispatcher) as P
import Proact.DOM.Props (Properties)
import React (ReactElement)
import React.DOM (text) as R

createElement
  :: forall s t e
   . String
  -> Array (Options Properties)
  -> Array ReactElement
  -> P.PComponent s t e ReactElement
createElement class_ props children =
  do
  dispatcher <- P.dispatcher
  pure $ _createElement class_ dispatcher (options $ fold props) children

fragment
  :: forall s t e
   . P.PComponent s t e (Array ReactElement) -> P.PComponent s t e ReactElement
fragment children = P.map _createFragment children

text :: forall s t e . String -> P.PComponent s t e ReactElement
text = pure <<< R.text

a
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
a = createElement' "a"

a'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
a' = a mempty

abbr
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
abbr = createElement' "abbr"

abbr'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
abbr' = abbr mempty

address
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
address = createElement' "address"

address'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
address' = address mempty

area
  :: forall s t e
   . Array (Options Properties) -> P.PComponent s t e ReactElement
area props = createElement' "area" props [ ]

area' :: forall s t e . P.PComponent s t e ReactElement
area' = area mempty

article
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
article = createElement' "article"

article'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
article' = article mempty

aside
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
aside = createElement' "aside"

aside'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
aside' = aside mempty

audio
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
audio = createElement' "audio"

audio'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
audio' = audio mempty

b
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
b = createElement' "b"

b'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
b' = b mempty

base
  :: forall s t e
   . Array (Options Properties) -> P.PComponent s t e ReactElement
base props = createElement' "base" props [ ]

base' :: forall s t e . P.PComponent s t e ReactElement
base' = base mempty

bdi
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
bdi = createElement' "bdi"

bdi'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
bdi' = bdi mempty

bdo
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
bdo = createElement' "bdo"

bdo'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
bdo' = bdo mempty

big
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
big = createElement' "big"

big'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
big' = big mempty

blockquote
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
blockquote = createElement' "blockquote"

blockquote'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
blockquote' = blockquote mempty

body
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
body = createElement' "body"

body'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
body' = body mempty

br
  :: forall s t e
   . Array (Options Properties) -> P.PComponent s t e ReactElement
br props = createElement' "br" props [ ]

br' :: forall s t e . P.PComponent s t e ReactElement
br' = br mempty

button
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
button = createElement' "button"

button'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
button' = button mempty

canvas
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
canvas = createElement' "canvas"

canvas'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
canvas' = canvas mempty

caption
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
caption = createElement' "caption"

caption'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
caption' = caption mempty

cite
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
cite = createElement' "cite"

cite'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
cite' = cite mempty

code
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
code = createElement' "code"

code'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
code' = code mempty

col
  :: forall s t e
   . Array (Options Properties) -> P.PComponent s t e ReactElement
col props = createElement' "col" props [ ]

col' :: forall s t e . P.PComponent s t e ReactElement
col' = col mempty

colgroup
  :: forall s t e 
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
colgroup = createElement' "colgroup"

colgroup'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
colgroup' = colgroup mempty

_data
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
_data = createElement' "_data"

_data'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
_data' = _data mempty

datalist
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
datalist = createElement' "datalist"

datalist'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
datalist' = datalist mempty

dd
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
dd = createElement' "dd"

dd'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
dd' = dd mempty

del
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
del = createElement' "del"

del'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
del' = del mempty

details
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
details = createElement' "details"

details'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
details' = details mempty

dfn
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
dfn = createElement' "dfn"

dfn'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
dfn' = dfn mempty

dialog
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
dialog = createElement' "dialog"

dialog'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
dialog' = dialog mempty

div
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
div = createElement' "div"

div'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
div' = div mempty

dl
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
dl = createElement' "dl"

dl'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
dl' = dl mempty

dt
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
dt = createElement' "dt"

dt'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
dt' = dt mempty

em
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
em = createElement' "em"

em'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
em' = em mempty

embed
  :: forall s t e
   . Array (Options Properties) -> P.PComponent s t e ReactElement
embed props = createElement' "embed" props [ ]

embed' :: forall s t e . P.PComponent s t e ReactElement
embed' = embed mempty

fieldset
  :: forall s t e
   .  Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
fieldset = createElement' "fieldset"

fieldset'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
fieldset' = fieldset mempty

figcaption
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
figcaption = createElement' "figcaption"

figcaption'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
figcaption' = figcaption mempty

figure
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
figure = createElement' "figure"

figure'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
figure' = figure mempty

footer
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
footer = createElement' "footer"

footer'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
footer' = footer mempty

form
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
form = createElement' "form"

form'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
form' = form mempty

h1
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
h1 = createElement' "h1"

h1'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
h1' = h1 mempty

h2
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
h2 = createElement' "h2"

h2'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
h2' = h2 mempty

h3
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
h3 = createElement' "h3"

h3'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
h3' = h3 mempty

h4
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
h4 = createElement' "h4"

h4'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
h4' = h4 mempty

h5
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
h5 = createElement' "h5"

h5'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
h5' = h5 mempty

h6
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
h6 = createElement' "h6"

h6'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
h6' = h6 mempty

head
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
head = createElement' "head"

head'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
head' = head mempty

header
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
header = createElement' "header"

header'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
header' = header mempty

hr
  :: forall s t e
   . Array (Options Properties) -> P.PComponent s t e ReactElement
hr props = createElement' "hr" props [ ]

hr' :: forall s t e . P.PComponent s t e ReactElement
hr' = hr mempty

html
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
html = createElement' "html"

html'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
html' = html mempty

i
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
i = createElement' "i"

i'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
i' = i mempty

iframe
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
iframe = createElement' "iframe"

iframe'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
iframe' = iframe mempty

img
  :: forall s t e
   . Array (Options Properties) -> P.PComponent s t e ReactElement
img props = createElement' "img" props [ ]

img' :: forall s t e . P.PComponent s t e ReactElement
img' = img mempty

input
  :: forall s t e
   . Array (Options Properties) -> P.PComponent s t e ReactElement
input props = createElement' "input" props [ ]

input' :: forall s t e . P.PComponent s t e ReactElement
input' = input mempty

ins
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
ins = createElement' "ins"

ins'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
ins' = ins mempty

kbd
  :: forall s t e
   .  Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
kbd = createElement' "kbd"

kbd'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
kbd' = kbd mempty

keygen
  :: forall s t e
   . Array (Options Properties) -> P.PComponent s t e ReactElement
keygen props = createElement' "keygen" props [ ]

keygen' :: forall s t e . P.PComponent s t e ReactElement
keygen' = keygen mempty

label
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
label = createElement' "label"

label'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
label' = label mempty

legend
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
legend = createElement' "legend"

legend'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
legend' = legend mempty

li
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
li = createElement' "li"

li'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
li' = li mempty

link
  :: forall s t e
   . Array (Options Properties) -> P.PComponent s t e ReactElement
link props = createElement' "link" props [ ]

link' :: forall s t e . P.PComponent s t e ReactElement
link' = link mempty

main
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
main = createElement' "main"

main'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
main' = main mempty

map
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
map = createElement' "map"

map'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
map' = map mempty

mark
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
mark = createElement' "mark"

mark'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
mark' = mark mempty

menu
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
menu = createElement' "menu"

menu'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
menu' = menu mempty

menuitem
  :: forall s t e
   . Array (Options Properties) -> P.PComponent s t e ReactElement
menuitem props = createElement' "menuitem" props [ ]

menuitem' :: forall s t e . P.PComponent s t e ReactElement
menuitem' = menuitem mempty

meta
  :: forall s t e
   . Array (Options Properties) -> P.PComponent s t e ReactElement
meta props = createElement' "meta" props [ ]

meta' :: forall s t e . P.PComponent s t e ReactElement
meta' = meta mempty

meter
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
meter = createElement' "meter"

meter'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
meter' = meter mempty

nav
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
nav = createElement' "nav"

nav'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
nav' = nav mempty

noscript
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
noscript = createElement' "noscript"

noscript'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
noscript' = noscript mempty

object
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
object = createElement' "object"

object'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
object' = object mempty

ol
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
ol = createElement' "ol"

ol'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
ol' = ol mempty

optgroup
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
optgroup = createElement' "optgroup"

optgroup'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
optgroup' = optgroup mempty

option
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
option = createElement' "option"

option'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
option' = option mempty

output
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
output = createElement' "output"

output'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
output' = output mempty

p
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
p = createElement' "p"

p'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
p' = p mempty

param
  :: forall s t e
   . Array (Options Properties) -> P.PComponent s t e ReactElement
param props = createElement' "param" props [ ]

param' :: forall s t e . P.PComponent s t e ReactElement
param' = param mempty

picture
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
picture = createElement' "picture"

picture'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
picture' = picture mempty

pre
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
pre = createElement' "pre"

pre'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
pre' = pre mempty

progress
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
progress = createElement' "progress"

progress'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
progress' = progress mempty

q
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
q = createElement' "q"

q'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
q' = q mempty

rp
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
rp = createElement' "rp"

rp'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
rp' = rp mempty

rt
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
rt = createElement' "rt"

rt'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
rt' = rt mempty

ruby
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
ruby = createElement' "ruby"

ruby'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
ruby' = ruby mempty

s
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
s = createElement' "s"

s'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
s' = s mempty

samp
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
samp = createElement' "samp"

samp'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
samp' = samp mempty

script
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
script = createElement' "script"

script'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
script' = script mempty

section
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
section = createElement' "section"

section'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
section' = section mempty

select
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
select = createElement' "select"

select'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
select' = select mempty

small
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
small = createElement' "small"

small'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
small' = small mempty

source
  :: forall s t e
   . Array (Options Properties) -> P.PComponent s t e ReactElement
source props = createElement' "source" props [ ]

source' :: forall s t e . P.PComponent s t e ReactElement
source' = source mempty

span
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
span = createElement' "span"

span'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
span' = span mempty

strong
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
strong = createElement' "strong"

strong'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
strong' = strong mempty

style
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
style = createElement' "style"

style'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
style' = style mempty

sub
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
sub = createElement' "sub"

sub'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
sub' = sub mempty

summary
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
summary = createElement' "summary"

summary'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
summary' = summary mempty

sup
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
sup = createElement' "sup"

sup'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
sup' = sup mempty

table
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
table = createElement' "table"

table'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
table' = table mempty

tbody
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
tbody = createElement' "tbody"

tbody'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
tbody' = tbody mempty

td
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
td = createElement' "td"

td'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
td' = td mempty

textarea
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
textarea = createElement' "textarea"

textarea'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
textarea' = textarea mempty

tfoot
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
tfoot = createElement' "tfoot"

tfoot'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
tfoot' = tfoot mempty

th
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
th = createElement' "th"

th'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
th' = th mempty

thead
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
thead = createElement' "thead"

thead'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
thead' = thead mempty

time
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
time = createElement' "time"

time'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
time' = time mempty

title
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
title = createElement' "title"

title'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
title' = title mempty

tr
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
tr = createElement' "tr"

tr'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
tr' = tr mempty

track
  :: forall s t e
   . Array (Options Properties) -> P.PComponent s t e ReactElement
track props = createElement' "track" props [ ]

track' :: forall s t e . P.PComponent s t e ReactElement
track' = track mempty

u
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
u = createElement' "u"

u'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
u' = u mempty

ul
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
ul = createElement' "ul"

ul'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
ul' = ul mempty

var
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
var = createElement' "var"

var'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
var' = var mempty

video
  :: forall s t e
   . Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
video = createElement' "video"

video'
  :: forall s t e
   . Array (P.PComponent s t e ReactElement) -> P.PComponent s t e ReactElement
video' = video mempty

wbr
  :: forall s t e
   . Array (Options Properties) -> P.PComponent s t e ReactElement
wbr props = createElement' "wbr" props [ ]

wbr' :: forall s t e . P.PComponent s t e ReactElement
wbr' = wbr mempty

foreign import _createElement
  :: forall s e
   .  String
  -> (P.EventHandler s e Unit -> Effect Unit)
  -> Foreign
  -> Array ReactElement
  -> ReactElement

foreign import _createFragment :: Array ReactElement -> ReactElement

createElement'
  :: forall s t e
   . String
  -> Array (Options Properties)
  -> Array (P.PComponent s t e ReactElement)
  -> P.PComponent s t e ReactElement
createElement' class_ props children =
  sequence children >>= createElement class_ props
