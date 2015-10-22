#' Create an HTML tag string
#'
#' A handy function to create HTML tags. Named arguments are interpreted as HTML
#' tag attributes and unnamed arguments are used as the value. Mulitple unnamed
#' values are concatenated together as a single value in the order they appear.
#'
#' @param name The tag name
#' @param args Additional HTML attributes and tag values
#' @param ... HTML attributes and values for the specific tag
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
#' @examples
#' ## Basic tags
#' h3("Third Time's a Charm!")
#' b("Emphasize this and that")
#'
#' ## Nesting tags
#' body(
#'   h1("I'm Number One!"),
#'   p("A biased analysis of life as an eldest child.")
#' )
#'
#' ## Custom tags
#' tag("hmmm", type = vague, "Was this a good idea?")
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

# <!DOCTYPE> 	Defines the document type
#' @rdname templating
#' @export
doctype <- function(...) tag('!DOCTYPE', list(...))

# <a>	Defines a hyperlink
#' @rdname templating
#' @export
a <- function(...) tag('a', list(...))

# <abbr>	Defines an abbreviation or an acronym
#' @rdname templating
#' @export
abbr <- function(...) tag('abbr', list(...))

# <acronym>	Not supported in HTML5. Use <abbr> instead. Defines an acronym
#' @rdname templating
#' @export
acronym <- function(...) tag('acronym', list(...))

# <address>	Defines contact information for the author/owner of a document
#' @rdname templating
#' @export
address <- function(...) tag('address', list(...))

# <applet>	Not supported in HTML5. Use <embed> or <object> instead. Defines an embedded applet
#' @rdname templating
#' @export
applet <- function(...) tag('applet', list(...))

# <area>	Defines an area inside an image-map
#' @rdname templating
#' @export
area <- function(...) tag('area', list(...))

# <article>	Defines an article
#' @rdname templating
#' @export
article <- function(...) tag('article', list(...))

# <aside>	Defines content aside from the page content
#' @rdname templating
#' @export
aside <- function(...) tag('aside', list(...))

# <audio>	Defines sound content
#' @rdname templating
#' @export
audio <- function(...) tag('audio', list(...))

# <b>	Defines bold text
#' @rdname templating
#' @export
b <- function(...) tag('b', list(...))

# <base>	Specifies the base URL/target for all relative URLs in a document
#' @rdname templating
#' @export
base <- function(...) tag('base', list(...))

# <basefont>	Not supported in HTML5. Use CSS instead. Specifies a default color, size, and font for all text in a document
#' @rdname templating
#' @export
basefont <- function(...) tag('basefont', list(...))

# <bdi>	Isolates a part of text that might be formatted in a different direction from other text outside it
#' @rdname templating
#' @export
bdi <- function(...) tag('bdi', list(...))

# <bdo>	Overrides the current text direction
#' @rdname templating
#' @export
bdo <- function(...) tag('bdo', list(...))

# <big>	Not supported in HTML5. Use CSS instead. Defines big text
#' @rdname templating
#' @export
big <- function(...) tag('big', list(...))

# <blockquote>	Defines a section that is quoted from another source
#' @rdname templating
#' @export
blockquote <- function(...) tag('blockquote', list(...))

# <body>	Defines the document's body
#' @rdname templating
#' @export
body <- function(...) tag('body', list(...))

# <br>	Defines a single line break
#' @rdname templating
#' @export
br <- function(...) '<br>'

# <button>	Defines a clickable button
#' @rdname templating
#' @export
button <- function(...) tag('button', list(...))

# <canvas>	Used to draw graphics, on the fly, via scripting (usually JavaScript)
#' @rdname templating
#' @export
canvas <- function(...) tag('canvas', list(...))

# <caption>	Defines a table caption
#' @rdname templating
#' @export
caption <- function(...) tag('caption', list(...))

# <center>	Not supported in HTML5. Use CSS instead. Defines centered text
#' @rdname templating
#' @export
center <- function(...) tag('center', list(...))

# <cite>	Defines the title of a work
#' @rdname templating
#' @export
cite <- function(...) tag('cite', list(...))

# <code>	Defines a piece of computer code
#' @rdname templating
#' @export
code <- function(...) tag('code', list(...))

# <col>	Specifies column properties for each column within a <colgroup> element
#' @rdname templating
#' @export
col <- function(...) tag('col', list(...))

# <colgroup>	Specifies a group of one or more columns in a table for formatting
#' @rdname templating
#' @export
colgroup <- function(...) tag('colgroup', list(...))

# <datalist>	Specifies a list of pre-defined options for input controls
#' @rdname templating
#' @export
datalist <- function(...) tag('datalist', list(...))

# <dd>	Defines a description/value of a term in a description list
#' @rdname templating
#' @export
dd <- function(...) tag('dd', list(...))

# <del>	Defines text that has been deleted from a document
#' @rdname templating
#' @export
del <- function(...) tag('del', list(...))

# <details>	Defines additional details that the user can view or hide
#' @rdname templating
#' @export
details <- function(...) tag('details', list(...))

# <dfn>	Represents the defining instance of a term
#' @rdname templating
#' @export
dfn <- function(...) tag('dfn', list(...))

# <dialog>	Defines a dialog box or window
#' @rdname templating
#' @export
dialog <- function(...) tag('dialog', list(...))

# <dir>	Not supported in HTML5. Use <ul> instead.
#' @rdname templating
#' @export
dir <- function(...) tag('dir', list(...))

# Defines a directory list

# <div>	Defines a section in a document
#' @rdname templating
#' @export
div <- function(...) tag('div', list(...))

# <dl>	Defines a description list
#' @rdname templating
#' @export
dl <- function(...) tag('dl', list(...))

# <dt>	Defines a term/name in a description list
#' @rdname templating
#' @export
dt <- function(...) tag('dt', list(...))

# <em>	Defines emphasized text
#' @rdname templating
#' @export
em <- function(...) tag('em', list(...))

# <embed>	Defines a container for an external (non-HTML) application
#' @rdname templating
#' @export
embed <- function(...) tag('embed', list(...))

# <fieldset>	Groups related elements in a form
#' @rdname templating
#' @export
fieldset <- function(...) tag('fieldset', list(...))

# <figcaption>	Defines a caption for a <figure> element
#' @rdname templating
#' @export
figcaption <- function(...) tag('figcaption', list(...))

# <figure>	Specifies self-contained content
#' @rdname templating
#' @export
figure <- function(...) tag('figure', list(...))

# <font>	Not supported in HTML5. Use CSS instead.
#' @rdname templating
#' @export
font <- function(...) tag('font', list(...))

# Defines font, color, and size for text

# <footer>	Defines a footer for a document or section
#' @rdname templating
#' @export
footer <- function(...) tag('footer', list(...))

# <form>	Defines an HTML form for user input
#' @rdname templating
#' @export
form <- function(...) tag('form', list(...))

# <frame>	Not supported in HTML5. Defines a window (a frame) in a frameset
#' @rdname templating
#' @export
frame <- function(...) tag('frame', list(...))

# <frameset>	Not supported in HTML5. Defines a set of frames
#' @rdname templating
#' @export
frameset <- function(...) tag('frameset', list(...))

# <h1> to <h6>	Defines HTML headings
#' @rdname templating
#' @export
h1 <- function(...) tag('h1', list(...))
#' @rdname templating
#' @export
h2 <- function(...) tag('h2', list(...))
#' @rdname templating
#' @export
h3 <- function(...) tag('h3', list(...))
#' @rdname templating
#' @export
h4 <- function(...) tag('h4', list(...))
#' @rdname templating
#' @export
h5 <- function(...) tag('h5', list(...))
#' @rdname templating
#' @export
h6 <- function(...) tag('h6', list(...))

# <head>	Defines information about the document
#' @rdname templating
#' @export
head <- function(...) tag('head', list(...))

# <header>	Defines a header for a document or section
#' @rdname templating
#' @export
header <- function(...) tag('header', list(...))

# <hr>	Defines a thematic change in the content
#' @rdname templating
#' @export
hr <- function(...) tag('hr', list(...))

# <html>	Defines the root of an HTML document
#' @rdname templating
#' @export
html <- function(...) tag('html', list(...))

# <i>	Defines a part of text in an alternate voice or mood
#' @rdname templating
#' @export
i <- function(...) tag('i', list(...))

# <iframe>	Defines an inline frame
#' @rdname templating
#' @export
iframe <- function(...) tag('iframe', list(...))

# <img>	Defines an image
#' @rdname templating
#' @export
img <- function(...) tag('img', list(...))

# <input>	Defines an input control
#' @rdname templating
#' @export
input <- function(...) tag('input', list(...))

# <ins>	Defines a text that has been inserted into a document
#' @rdname templating
#' @export
ins <- function(...) tag('ins', list(...))

# <kbd>	Defines keyboard input
#' @rdname templating
#' @export
kbd <- function(...) tag('kbd', list(...))

# <keygen>	Defines a key-pair generator field (for forms)
#' @rdname templating
#' @export
keygen <- function(...) tag('keygen', list(...))

# <label>	Defines a label for an <input> element
#' @rdname templating
#' @export
label <- function(...) tag('label', list(...))

# <legend>	Defines a caption for a <fieldset> element
#' @rdname templating
#' @export
legend <- function(...) tag('legend', list(...))

# <li>	Defines a list item
#' @rdname templating
#' @export
li <- function(...) tag('li', list(...))

# <link>	Defines the relationship between a document and an external resource (most used to link to style sheets)
#' @rdname templating
#' @export
link <- function(...) tag('link', list(...))

# <main>	Specifies the main content of a document
#' @rdname templating
#' @export
main <- function(...) tag('main', list(...))

# <map>	Defines a client-side image-map
#' @rdname templating
#' @export
map <- function(...) tag('map', list(...))

# <mark>	Defines marked/highlighted text
#' @rdname templating
#' @export
mark <- function(...) tag('mark', list(...))

# <menu>	Defines a list/menu of commands
#' @rdname templating
#' @export
menu <- function(...) tag('menu', list(...))

# <menuitem>	Defines a command/menu item that the user can invoke from a popup menu
#' @rdname templating
#' @export
menuitem <- function(...) tag('menuitem', list(...))

# <meta>	Defines metadata about an HTML document
#' @rdname templating
#' @export
meta <- function(...) tag('meta', list(...))

# <meter>	Defines a scalar measurement within a known range (a gauge)
#' @rdname templating
#' @export
meter <- function(...) tag('meter', list(...))

# <nav>	Defines navigation links
#' @rdname templating
#' @export
nav <- function(...) tag('nav', list(...))

# <noframes>	Not supported in HTML5. Defines an alternate content for users that do not support frames
#' @rdname templating
#' @export
noframes <- function(...) tag('noframes', list(...))

# <noscript>	Defines an alternate content for users that do not support client-side scripts
#' @rdname templating
#' @export
noscript <- function(...) tag('noscript', list(...))

# <object>	Defines an embedded object
#' @rdname templating
#' @export
object <- function(...) tag('object', list(...))

# <ol>	Defines an ordered list
#' @rdname templating
#' @export
ol <- function(...) tag('ol', list(...))

# <optgroup>	Defines a group of related options in a drop-down list
#' @rdname templating
#' @export
optgroup <- function(...) tag('optgroup', list(...))

# <option>	Defines an option in a drop-down list
#' @rdname templating
#' @export
option <- function(...) tag('option', list(...))

# <output>	Defines the result of a calculation
#' @rdname templating
#' @export
output <- function(...) tag('output', list(...))

# <p>	Defines a paragraph
#' @rdname templating
#' @export
p <- function(...) tag('p', list(...))

# <param>	Defines a parameter for an object
#' @rdname templating
#' @export
param <- function(...) tag('param', list(...))

# <pre>	Defines preformatted text
#' @rdname templating
#' @export
pre <- function(...) tag('pre', list(...))

# <progress>	Represents the progress of a task
#' @rdname templating
#' @export
progress <- function(...) tag('progress', list(...))

# <q>	Defines a short quotation
#' @rdname templating
#' @export
q <- function(...) tag('q', list(...))

# <rp>	Defines what to show in browsers that do not support ruby annotations
#' @rdname templating
#' @export
rp <- function(...) tag('rp', list(...))

# <rt>	Defines an explanation/pronunciation of characters (for East Asian typography)
#' @rdname templating
#' @export
rt <- function(...) tag('rt', list(...))

# <ruby>	Defines a ruby annotation (for East Asian typography)
#' @rdname templating
#' @export
ruby <- function(...) tag('ruby', list(...))

# <s>	Defines text that is no longer correct
#' @rdname templating
#' @export
s <- function(...) tag('s', list(...))

# <samp>	Defines sample output from a computer program
#' @rdname templating
#' @export
samp <- function(...) tag('samp', list(...))

# <script>	Defines a client-side script
#' @rdname templating
#' @export
script <- function(...) tag('script', list(...))

# <section>	Defines a section in a document
#' @rdname templating
#' @export
section <- function(...) tag('section', list(...))

# <select>	Defines a drop-down list
#' @rdname templating
#' @export
select <- function(...) tag('select', list(...))

# <small>	Defines smaller text
#' @rdname templating
#' @export
small <- function(...) tag('small', list(...))

# <source>	Defines multiple media resources for media elements (<video> and <audio>)
#' @rdname templating
#' @export
source <- function(...) tag('source', list(...))

# <span>	Defines a section in a document
#' @rdname templating
#' @export
span <- function(...) tag('span', list(...))

# <strike>	Not supported in HTML5. Use <del> or <s> instead. Defines strikethrough text
#' @rdname templating
#' @export
strike <- function(...) tag('strike', list(...))

# <strong>	Defines important text
#' @rdname templating
#' @export
strong <- function(...) tag('strong', list(...))

# <style>	Defines style information for a document
#' @rdname templating
#' @export
style <- function(...) tag('style', list(...))

# <sub>	Defines subscripted text
#' @rdname templating
#' @export
sub <- function(...) tag('sub', list(...))

# <summary>	Defines a visible heading for a <details> element
#' @rdname templating
#' @export
summary <- function(...) tag('summary', list(...))

# <sup>	Defines superscripted text
#' @rdname templating
#' @export
sup <- function(...) tag('sup', list(...))

# <table>	Defines a table
#' @rdname templating
#' @export
table <- function(...) tag('table', list(...))

# <tbody>	Groups the body content in a table
#' @rdname templating
#' @export
tbody <- function(...) tag('tbody', list(...))

# <td>	Defines a cell in a table
#' @rdname templating
#' @export
td <- function(...) tag('td', list(...))

# <textarea>	Defines a multiline input control (text area)
#' @rdname templating
#' @export
textarea <- function(...) tag('textarea', list(...))

# <tfoot>	Groups the footer content in a table
#' @rdname templating
#' @export
tfoot <- function(...) tag('tfoot', list(...))

# <th>	Defines a header cell in a table
#' @rdname templating
#' @export
th <- function(...) tag('th', list(...))

# <thead>	Groups the header content in a table
#' @rdname templating
#' @export
thead <- function(...) tag('thead', list(...))

# <time>	Defines a date/time
#' @rdname templating
#' @export
time <- function(...) tag('time', list(...))

# <title>	Defines a title for the document
#' @rdname templating
#' @export
title <- function(...) tag('title', list(...))

# <tr>	Defines a row in a table
#' @rdname templating
#' @export
tr <- function(...) tag('tr', list(...))

# <track>	Defines text tracks for media elements (<video> and <audio>)
#' @rdname templating
#' @export
track <- function(...) tag('track', list(...))

# <tt>	Not supported in HTML5. Use CSS instead. Defines teletype text
#' @rdname templating
#' @export
tt <- function(...) tag('tt', list(...))

# <u>	Defines text that should be stylistically different from normal text
#' @rdname templating
#' @export
u <- function(...) tag('u', list(...))

# <ul>	Defines an unordered list
#' @rdname templating
#' @export
ul <- function(...) tag('ul', list(...))

# <var>	Defines a variable
#' @rdname templating
#' @export
var <- function(...) tag('var', list(...))

# <video>	Defines a video or movie
#' @rdname templating
#' @export
video <- function(...) tag('video', list(...))

# <wbr>	Defines a possible line-break
#' @rdname templating
#' @export
wbr <- function(...) tag('wbr', list(...))
