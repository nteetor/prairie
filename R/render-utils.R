#' Create HTML tag string
#'
#' A handy function to create HTML tags. Named arguments are interpreted as HTML
#' tag attributes and unnamed arguments are used as the value. Mulitple unnamed
#' values will be concatenated together in the order they appear.
#'
#' @param name The tag name
#' @param args Additional HTML attributes and tag values
#'
#' @details
#'
#' Most of the time one will want to use the default wrapper functions like
#' \code{a}, \code{body} or \code{div}, see below for the complete list.
#' However, \code{tag} allows for one to work with custom tags. Because of this,
#' \code{tag} naiveley assumes the named arguments passed as \code{...} are
#' appropriate attributes for tag \code{name}.
#'
#' @name templating
#' @export
tag <- function(name, args) {
  if (length(args) == 0) return(paste0('<', name, '/>'))

  attributes <- args[names(args) != ""]
  values <- if (is.null(names(args))) as.character(args) else args[names(args) == ""]

  attribute_string <- paste(
    vapply(
      names(attributes),
      function(nm) {
        quote_char <- if (grepl('\"', attributes[[nm]])) '\'' else '\"' 
        paste0(nm, '=', quote_char, attributes[[nm]], quote_char)
      },
      character(1)
    ),
    collapse = ' '
  )

  attribute_string <- if (nchar(attribute_string) > 0) paste0(' ', attribute_string) else attribute_string
  value_string <- paste(values, collapse = '')

  if (nchar(value_string) == 0) {
    paste0('<', name, attribute_string, '/>')
  } else {
    paste0('<', name, attribute_string, '>', value_string, '</', name, '>')
  }
}

# <!--...-->	Defines a comment
# <!DOCTYPE> 	Defines the document type
# <a>	Defines a hyperlink
a <- function(...) tag('a', list(...))
# <abbr>	Defines an abbreviation or an acronym
abbr <- function(...) tag('abbr', list(...))
# <acronym>	Not supported in HTML5. Use <abbr> instead.
acronym <- function(...) tag('acronym', list(...))
# Defines an acronym
# <address>	Defines contact information for the author/owner of a document
address <- function(...) tag('address', list(...))
# <applet>	Not supported in HTML5. Use <embed> or <object> instead.
applet <- function(...) tag('applet', list(...))
# Defines an embedded applet
# <area>	Defines an area inside an image-map
area <- function(...) tag('area', list(...))
# <article>	Defines an article
article <- function(...) tag('article', list(...))
# <aside>	Defines content aside from the page content
aside <- function(...) tag('aside', list(...))
# <audio>	Defines sound content
audio <- function(...) tag('audio', list(...))
# <b>	Defines bold text
b <- function(...) tag('b', list(...))
# <base>	Specifies the base URL/target for all relative URLs in a document
base <- function(...) tag('base', list(...))
# <basefont>	Not supported in HTML5. Use CSS instead.
basefont <- function(...) tag('basefont', list(...))
# Specifies a default color, size, and font for all text in a document
# <bdi>	Isolates a part of text that might be formatted in a different direction from other text outside it
bdi <- function(...) tag('bdi', list(...))
# <bdo>	Overrides the current text direction
bdo <- function(...) tag('bdo', list(...))
# <big>	Not supported in HTML5. Use CSS instead.
big <- function(...) tag('big', list(...))
# Defines big text
# <blockquote>	Defines a section that is quoted from another source
blockquote <- function(...) tag('blockquote', list(...))
# <body>	Defines the document's body
body <- function(...) tag('body', list(...))
# <br>	Defines a single line break
br <- function(...) '<br>'
# <button>	Defines a clickable button
button <- function(...) tag('button', list(...))
# <canvas>	Used to draw graphics, on the fly, via scripting (usually JavaScript)
canvas <- function(...) tag('canvas', list(...))
# <caption>	Defines a table caption
caption <- function(...) tag('caption', list(...))
# <center>	Not supported in HTML5. Use CSS instead.
center <- function(...) tag('center', list(...))
# Defines centered text
# <cite>	Defines the title of a work
cite <- function(...) tag('cite', list(...))
# <code>	Defines a piece of computer code
code <- function(...) tag('code', list(...))
# <col>	Specifies column properties for each column within a <colgroup> element
col <- function(...) tag('col', list(...))
# <colgroup>	Specifies a group of one or more columns in a table for formatting
colgroup <- function(...) tag('colgroup', list(...))
# <datalist>	Specifies a list of pre-defined options for input controls
datalist <- function(...) tag('datalist', list(...))
# <dd>	Defines a description/value of a term in a description list
dd <- function(...) tag('dd', list(...))
# <del>	Defines text that has been deleted from a document
del <- function(...) tag('del', list(...))
# <details>	Defines additional details that the user can view or hide
details <- function(...) tag('details', list(...))
# <dfn>	Represents the defining instance of a term
dfn <- function(...) tag('dfn', list(...))
# <dialog>	Defines a dialog box or window
dialog <- function(...) tag('dialog', list(...))
# <dir>	Not supported in HTML5. Use <ul> instead.
dir <- function(...) tag('dir', list(...))
# Defines a directory list
# <div>	Defines a section in a document
div <- function(...) tag('div', list(...))
# <dl>	Defines a description list
dl <- function(...) tag('dl', list(...))
# <dt>	Defines a term/name in a description list
dt <- function(...) tag('dt', list(...))
# <em>	Defines emphasized text
em <- function(...) tag('em', list(...))
# <embed>	Defines a container for an external (non-HTML) application
embed <- function(...) tag('embed', list(...))
# <fieldset>	Groups related elements in a form
fieldset <- function(...) tag('fieldset', list(...))
# <figcaption>	Defines a caption for a <figure> element
figcaption <- function(...) tag('figcaption', list(...))
# <figure>	Specifies self-contained content
figure <- function(...) tag('figure', list(...))
# <font>	Not supported in HTML5. Use CSS instead.
font <- function(...) tag('font', list(...))
# Defines font, color, and size for text
# <footer>	Defines a footer for a document or section
footer <- function(...) tag('footer', list(...))
# <form>	Defines an HTML form for user input
form <- function(...) tag('form', list(...))
# <frame>	Not supported in HTML5.
frame <- function(...) tag('frame', list(...))
# Defines a window (a frame) in a frameset
# <frameset>	Not supported in HTML5.
frameset <- function(...) tag('frameset', list(...))
# Defines a set of frames
# <h1> to <h6>	Defines HTML headings
h1 <- function(...) tag('h1', list(...))
h2 <- function(...) tag('h2', list(...))
h3 <- function(...) tag('h3', list(...))
h4 <- function(...) tag('h4', list(...))
h5 <- function(...) tag('h5', list(...))
h6 <- function(...) tag('h6', list(...))
# <head>	Defines information about the document
head <- function(...) tag('head', list(...))
# <header>	Defines a header for a document or section
header <- function(...) tag('header', list(...))
# <hr>	Defines a thematic change in the content
hr <- function(...) tag('hr', list(...))
# <html>	Defines the root of an HTML document
html <- function(...) tag('html', list(...))
# <i>	Defines a part of text in an alternate voice or mood
i <- function(...) tag('i', list(...))
# <iframe>	Defines an inline frame
iframe <- function(...) tag('iframe', list(...))
# <img>	Defines an image
img <- function(...) tag('img', list(...))
# <input>	Defines an input control
input <- function(...) tag('input', list(...))
# <ins>	Defines a text that has been inserted into a document
ins <- function(...) tag('ins', list(...))
# <kbd>	Defines keyboard input
kbd <- function(...) tag('kbd', list(...))
# <keygen>	Defines a key-pair generator field (for forms)
keygen <- function(...) tag('keygen', list(...))
# <label>	Defines a label for an <input> element
label <- function(...) tag('label', list(...))
# <legend>	Defines a caption for a <fieldset> element
legend <- function(...) tag('legend', list(...))
# <li>	Defines a list item
li <- function(...) tag('li', list(...))
# <link>	Defines the relationship between a document and an external resource (most used to link to style sheets)
link <- function(...) tag('link', list(...))
# <main>	Specifies the main content of a document
main <- function(...) tag('main', list(...))
# <map>	Defines a client-side image-map
map <- function(...) tag('map', list(...))
# <mark>	Defines marked/highlighted text
mark <- function(...) tag('mark', list(...))
# <menu>	Defines a list/menu of commands
menu <- function(...) tag('menu', list(...))
# <menuitem>	Defines a command/menu item that the user can invoke from a popup menu
menuitem <- function(...) tag('menuitem', list(...))
# <meta>	Defines metadata about an HTML document
meta <- function(...) tag('meta', list(...))
# <meter>	Defines a scalar measurement within a known range (a gauge)
meter <- function(...) tag('meter', list(...))
# <nav>	Defines navigation links
nav <- function(...) tag('nav', list(...))
# <noframes>	Not supported in HTML5.
noframes <- function(...) tag('noframes', list(...))
# Defines an alternate content for users that do not support frames
# <noscript>	Defines an alternate content for users that do not support client-side scripts
noscript <- function(...) tag('noscript', list(...))
# <object>	Defines an embedded object
object <- function(...) tag('object', list(...))
# <ol>	Defines an ordered list
ol <- function(...) tag('ol', list(...))
# <optgroup>	Defines a group of related options in a drop-down list
optgroup <- function(...) tag('optgroup', list(...))
# <option>	Defines an option in a drop-down list
option <- function(...) tag('option', list(...))
# <output>	Defines the result of a calculation
output <- function(...) tag('output', list(...))
# <p>	Defines a paragraph
p <- function(...) tag('p', list(...))
# <param>	Defines a parameter for an object
param <- function(...) tag('param', list(...))
# <pre>	Defines preformatted text
pre <- function(...) tag('pre', list(...))
# <progress>	Represents the progress of a task
progress <- function(...) tag('progress', list(...))
# <q>	Defines a short quotation
q <- function(...) tag('q', list(...))
# <rp>	Defines what to show in browsers that do not support ruby annotations
rp <- function(...) tag('rp', list(...))
# <rt>	Defines an explanation/pronunciation of characters (for East Asian typography)
rt <- function(...) tag('rt', list(...))
# <ruby>	Defines a ruby annotation (for East Asian typography)
ruby <- function(...) tag('ruby', list(...))
# <s>	Defines text that is no longer correct
s <- function(...) tag('s', list(...))
# <samp>	Defines sample output from a computer program
samp <- function(...) tag('samp', list(...))
# <script>	Defines a client-side script
script <- function(...) tag('script', list(...))
# <section>	Defines a section in a document
section <- function(...) tag('section', list(...))
# <select>	Defines a drop-down list
select <- function(...) tag('select', list(...))
# <small>	Defines smaller text
small <- function(...) tag('small', list(...))
# <source>	Defines multiple media resources for media elements (<video> and <audio>)
source <- function(...) tag('source', list(...))
# <span>	Defines a section in a document
span <- function(...) tag('span', list(...))
# <strike>	Not supported in HTML5. Use <del> or <s> instead.
strike <- function(...) tag('strike', list(...))
# Defines strikethrough text
# <strong>	Defines important text
strong <- function(...) tag('strong', list(...))
# <style>	Defines style information for a document
style <- function(...) tag('style', list(...))
# <sub>	Defines subscripted text
sub <- function(...) tag('sub', list(...))
# <summary>	Defines a visible heading for a <details> element
summary <- function(...) tag('summary', list(...))
# <sup>	Defines superscripted text
sup <- function(...) tag('sup', list(...))
# <table>	Defines a table
table <- function(...) tag('table', list(...))
# <tbody>	Groups the body content in a table
tbody <- function(...) tag('tbody', list(...))
# <td>	Defines a cell in a table
td <- function(...) tag('td', list(...))
# <textarea>	Defines a multiline input control (text area)
textarea <- function(...) tag('textarea', list(...))
# <tfoot>	Groups the footer content in a table
tfoot <- function(...) tag('tfoot', list(...))
# <th>	Defines a header cell in a table
th <- function(...) tag('th', list(...))
# <thead>	Groups the header content in a table
thead <- function(...) tag('thead', list(...))
# <time>	Defines a date/time
time <- function(...) tag('time', list(...))
# <title>	Defines a title for the document
title <- function(...) tag('title', list(...))
# <tr>	Defines a row in a table
tr <- function(...) tag('tr', list(...))
# <track>	Defines text tracks for media elements (<video> and <audio>)
track <- function(...) tag('track', list(...))
# <tt>	Not supported in HTML5. Use CSS instead.
tt <- function(...) tag('tt', list(...))
# Defines teletype text
# <u>	Defines text that should be stylistically different from normal text
u <- function(...) tag('u', list(...))
# <ul>	Defines an unordered list
ul <- function(...) tag('ul', list(...))
# <var>	Defines a variable
var <- function(...) tag('var', list(...))
# <video>	Defines a video or movie
video <- function(...) tag('video', list(...))
# <wbr>	Defines a possible line-break
wbr <- function(...) tag('wbr', list(...))
