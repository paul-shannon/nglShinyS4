#' @import shiny
#' @import htmlwidgets
#' @import shinyjs
#' @importFrom methods new
#'
#' @title NglWidget
#------------------------------------------------------------------------------------------------------------------------
#' @name NglWidget-class
#' @rdname NglWidget-class
#' @aliases NglWidget
#'
## @import methods

.NglWidget <- setClass("NglWidget",
                       representation = representation(
                           pdbID="character",
                           htmlContainer="character",
                           width="numeric",
                           height="numeric",
                           componentOptions="list",
                           state="environment",
                           quiet="logical"
                           )
                         )

#------------------------------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
#------------------------------------------------------------------------------------------------------------------------
setGeneric('getWidget', signature='obj', function(obj) standardGeneric('getWidget'))
setGeneric('shinyOutput', signature='obj', function(obj) standardGeneric('shinyOutput'))
setGeneric('renderWidget', signature='obj', function(obj) standardGeneric('renderWidget'))
setGeneric('getPdbID', signature='obj', function(obj) standardGeneric('getPdbID'))
setGeneric('fit', signature='obj', function(obj) standardGeneric('fit'))
#------------------------------------------------------------------------------------------------------------------------
#' NglWidget
#'
#'
#' @export
#'
#' @param pdbID character
#' @param componentOptions list of lists, specifying named components of the molecular structure
#' @param htmlContainer character, the name of the DOM element, typically a div
#' @param width integer  initial width of the widget.
#' @param height integer initial height of the widget.
#'
#' @return a reference to an htmlwidget.
#'
NglWidget <- function(pdbID, htmlContainer, componentOptions=list(),
                      width = NA_integer_, height = NA_integer_, quiet=TRUE)
{
  printf("--- ~/github/nglShinyS4/R/NglWidget s4 ctor");

  widget <- htmlwidgets::createWidget(
    name = 'NGL',
    options,
    width = width,
    height = height,
    package = 'nglShiny',
    elementId = htmlContainer
    )

  state <- new.env(parent=emptyenv())
  state[["widget"]] <- widget

  obj <- .NglWidget(pdbID=pdbID,
                   htmlContainer=htmlContainer,
                   width=width,
                   height=height,
                   componentOptions=componentOptions,
                   state=state,
                   quiet=quiet)

   obj

} # NglWidget constructor
#----------------------------------------------------------------------------------------------------
setMethod('getWidget', 'NglWidget',

      function(obj){
         return(obj@state$widget)
          })

#----------------------------------------------------------------------------------------------------
#' Standard shiny ui rendering construct
#'
#' @param obj an NglWidget instance
#' @return a reference to an htmlwidget
#'
#' @examples
#' \dontrun{
#'   mainPanel(shinyOutput(nglWidget)), width=10)
#' }
#'
#' @aliases NglWidgetOutput
#' @rdname NglWidgetOutput
#'
#' @export
#'
setMethod('shinyOutput', 'NglWidget',

     function(obj) {
         htmlwidgets::shinyWidgetOutput(obj@htmlContainer, 'NglWidget', obj@width, obj@height,
                                        package = 'NglWidget')
         })
#----------------------------------------------------------------------------------------------------
#' More shiny plumbing -  an NglWidget wrapper for htmlwidget standard rendering operation
#'
#' @param expr an expression that generates an HTML widget.
#' @param env environment in which to evaluate expr.
#' @param quoted logical specifies whether expr is quoted ("useuful if you want to save an expression in a variable").
#'
#' @return not sure
#'
#' @aliases renderWidget
#' @rdname renderWidget
#'
#' @export
#'
setMethod('renderWidget', 'NglWidget',
    function(obj) {
        env = parent.frame()
        htmlwidgets::shinyRenderWidget(getWidget(obj), shinyOutput(obj), env, quoted = TRUE)
        })

#----------------------------------------------------------------------------------------------------
#' Set zoom and center so that the current model nicely fills the display.
#'
#' @param session a Shiny server session object.
#' @param htmlContainer a character string used to identify the NglWidget instance, the id of html element
#'
#' @examples
#' \dontrun{
#'   fit(session)
#'}
#'
#' @aliases fit
#' @rdname fit
#'
#'
#' @export
#'
fit <- function(session, htmlContainer)
{
   session$sendCustomMessage("fit", message=list(htmlContainer=htmlContainer))

} # fit
#----------------------------------------------------------------------------------------------------
setRepresentation <- function(session, rep)
{
   session$sendCustomMessage("setRepresentation", list(rep))

} # setRepresentation
#----------------------------------------------------------------------------------------------------
#' Using the specified representation and colorScheme, display the portion of selection
#'
#' @param session a Shiny server session object.
#' @param representation todo
#' @param selection todo
#' @param colorScheme todo
#' @param name character string, used for subsequent show/hide
#'
#' @examples
#' \dontrun{
#'   showSelection(session, "cartoon", "helix", "residueIndex")
#'}
#'
#' @aliases showSelection
#' @rdname showSelection
#'
#' @export
#'
showSelection <- function(session, representation, selection, name, colorScheme="residueIndex")
{
    session$sendCustomMessage("showSelection",
                              list(representation=representation,
                                   selection=selection,
                                   colorScheme=colorScheme,
                                   name=name))

} # showSelection
#----------------------------------------------------------------------------------------------------
#' hide or show the named selection
#'
#' @param session a Shiny server session object.
#' @param representationName a previously assigned character string
#'
#' @examples
#' \dontrun{
#'   setVisibility(session, "chromaphore", FALSE)
#'}
#'
#' @aliases setVisibility
#' @rdname setVisibility
#'
#' @export
#'
setVisibility <- function(session, representationName, newVisibilityState)
{
    session$sendCustomMessage("setVisibility",
                              list(representationName=representationName,
                                   newState=newVisibilityState))

} # setVisibility
#----------------------------------------------------------------------------------------------------
setColorScheme <- function(session, newColorScheme)
{
   session$sendCustomMessage("setRepresentation", list(newColorScheme))

} # setColorScheme
#----------------------------------------------------------------------------------------------------

