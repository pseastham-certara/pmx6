#' @export
#' @description This is a test function
#' @usage testfunc()
#' @param param1
#' @param param2
#' @details here be pirates
#' @section another section
#' @references reference1 (2022), reference2 (also 2022)
testfunc <- function() {
  text <- paste("This is a test function in the pmx6 library.",
                "It was created on April 22, 2022.",
                sep="\n")
  print(text)
}

#' modify plots to log scale (helper function - do not export)
add_log_modifications <- function(p) {
  p <- p + ggplot2::scale_y_log10()
  return(p)
}

#' Creates a mean plot object with LOESS curve representing mean
#'
#' @param df input data frame
#' @param time column variable representing time, typically "TIME"
#' @param dv column variable representing dependent variable, typically "DV"
#' @param scale either "linear" or "semilog"
#' @param xval string for name of x value
#' @param xunit string for unit of x value
#' @param yval string for name of y value
#' @param yunit string for unit of y value
#' @param facet_on facet variable, e.g. "RACE", "SEX", etc
#' @export
mean_plot <- function(df,
                      time="TIME",
                      dv="DV",
                      scale='linear',
                      xval="Actual time since first dose",
                      xunit="hr",
                      yval="Drug concentration",
                      yunit="ng/mL",
                      facet_on="NOFACET") {
  dat <- df

  if (facet_on != "NOFACET") { dat %<>% mutate(FACETVAR = !!sym(facet_on)) }

  p <- ggplot2::ggplot(dat,
                       aes(x=!!sym(time),
                           y=!!sym(dv))) +
                       geom_point(alpha=0.8, size = 2) +
                       geom_smooth(span = 0.5) +
                       labs(x = paste0(xval, "(", xunit, ")"),
                            y = paste0(yval, "(", yunit, ")")) +
                       theme_bw() +
                       expand_limits(y=0) +
                       guides(color = guide_legend(override.aes = list(size = 3, alpha = 1)))

  if (facet_on != "NOFACET") { p <- p + ggplot2::facet_wrap(~FACETVAR) }
  if (scale == 'semilog') { p %<>% add_log_modifications() }

  return(p)
}

# function for correlation matrix - from Prokash's EDA script
lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot2::ggplot(data = data, mapping = mapping) +
       ggplot2::geom_point(colour = "blue") +
       ggplot2::geom_smooth(method = method, color = "red", ...)
  return(p)
}

#' Creates a correlation matrix with covariates
#'
#' @param df input data frame
#' @examples
#' dat <- df %>% dplyr::distinct(USUBJID, .keep_all=TRUE) %>% dplyr::select(AGE, BWT, BHT, BBMI, SEXF)
#' p <- correlation.matrix(dat)
#' print(p)
#' @export
correlation.matrix <- function(df) {
  p <- GGally::ggpairs(dat,
                       lower = list(continuous = wrap(lowerFn, method = "lm")),
                       diag = list(continuous = wrap("barDiag", colour = "blue")),
                       upper = list(continuous = wrap("cor", size = 4)))
  return(p)
}
