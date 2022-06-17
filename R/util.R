#' Creates a new Rmarkdown html tab.
#'
#' Make sure that Rmd chunk option results='asis' is present.
#'
#' @param title Tab title
#' @param header An int indicating Rmd html header level (e.g. 3 means "###")
#' @param numbered A boolean for whether tab should be numbered or not
#' @param subtabs A boolean for whether tab has subtabs below it or not
#' @param pills A boolean for whether tab should be pill-shaped or not
#'
#' @examples
#' p <- ggplot(x, y, dat=somedata)
#' new_tab(title="My Plot", header=3)
#' print(p)
#' @export
new_tab <- function(title="Tab", header=2, nonumber=T, subtabs=F, pills=F) {
  cat("  \n\n")

  # create header hashtags
  HEAD = ""
  for (i in 1:header) {
    HEAD = paste0(HEAD, "#")
  }

  # check if there are any options
  ANYOPTIONS = subtabs | nonumber

  # create subtabs and numbering indicators
  OPTIONS = ""
  if (ANYOPTIONS) {
    OPTIONS = paste0(OPTIONS, "{")
    if (nonumber) { OPTIONS = paste0(OPTIONS, "- ") }
    if (subtabs){
      OPTIONS = paste0(OPTIONS, ".tabset .tabset-fade")
      if (pills) { OPTIONS = paste0(OPTIONS, " .tabset-pills") }
    }
    OPTIONS = paste0(OPTIONS, "}")
  }

  cat(HEAD, " ", title, " ", OPTIONS, "  \n\n")

  cat("  \n\n")
}


# Add function for splitting up continuous variable into quartiles cleanly


# Add function for adding number of subjects with that facet variable to title (e.g. "Male (n=23)" instead of just "Male")


# Add function that inserts "CERTARA" logo into html
