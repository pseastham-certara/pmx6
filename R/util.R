#' @export
#' Creates a new Rmarkdown html tab.
#' 
#' Make sure that Rmd chunk option results='asis' is present.
#' 
#' @param item An object to add into a new Rmd tab
#' @param title Tab title
#' @param header An int indicating Rmd html header level (e.g. 3 means "###")
#' @param numbered A boolean for whether tab should be numbered or not
#' @param subtabs A boolean for whether tab has subtabs below it or not
#' @param pills A boolean for whether tab should be pill-shaped or not
#' @param table A boolean for whether `item` is a table object or not
#'
#' @examples
#' p <- ggplot(x, y, dat=somedata)
#' new_tab(p, title="My Plot", header=3)
new_tab <- function(item, title="Tab", header=2, numbered=F, subtabs=F, pills=F, table=F) {
  # create header hashtags
  HEAD = ""
  for (i in 1:header) { 
    HEAD = paste0(HEAD, "#")
  }
  
  # check if there are any options
  ANYOPTIONS = subtabs | !numbered
  
  # create subtabs and numbering indicators
  OPTIONS = ""
  if (ANYOPTIONS) {
    OPTIONS = paste0(OPTIONS, "{")
    if (!numbered) { OPTIONS = paste0(OPTIONS, "- ") }
    if (subtabs){
      OPTIONS = paste0(OPTIONS, ".tabset .tabset-fade")
      if (pills) { OPTIONS = paste0(OPTIONS, " .tabset-pills") }
    }
    OPTIONS = paste0(OPTIONS, "}")
  }
  
  # print item to html in new tab
  cat("  \n")
  cat(HEAD, " ", title, " ", OPTIONS, "  \n\n")
  if ( is.null(item) ) {
    item
  } else if ( table ){
    return(item)
  } else {
    print(item)
  }
  cat("  \n\n")
}