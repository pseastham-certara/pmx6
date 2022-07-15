#' @description Test Func
#' @usage testfunc()
#' @param param1 p1
#' @param param2 p2
#' @details here be pirates
#' @section another section
#' @references reference1 (2022), reference2 (also 2022)
#' @export
testfunc <- function() {
  text <- paste("This is a test function in the pmx6 library.",
                "It was created on April 22, 2022.",
                sep="\n")
  print(text)
}


#' Summarize data types
#' @description Summarize dataset column names and data types
#' @param df input data frame
#' @return A \code{flextable} object
#' @note The \code{flextable} package needs to be installed for this to work.
#' @export
peek <- function(df) {
    if (!requireNamespace("flextable", quietly = TRUE)) {
      stop("This function requires package 'flextable'. Please install it and try again.", call.=F)
    }

    dfx = df
    tablefn <- getFromNamespace("flextable", "flextable")
    summ <- sapply(dfx,class)
    summ <- stack(summ)
    summ <- summ[, c("ind", "values")]
    summ <- cbind(rownames(summ), summ)
    colnames(summ) <- c("Column No", "Variable", "Type")

    out <- tablefn(summ)
    out <- flextable::autofit(out)

    out
}

#' Stacked bar plots
#' @description Create stacked barplots to tally variable of interest
#' @param df input data frame
#' @param gval column to group on (x-axis tick labels)
#' @param sval column to summarize on (stacks)
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param fill.lab legend title
#' @return A \code{ggplot} object
#' @export
stackbar <- function(df,
                     gval,
                     sval,
                     xlab="",
                     ylab = "Count",
                     fill.lab = "") {

  dfx <- df
  gval <- rlang::sym(gval)
  sval <- rlang::sym(sval)

  dfx.tmp <- dfx %>%
    dplyr::group_by(!!gval, !!sval) %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(COHORTN)) %>%
    dplyr::group_by(TREATC) %>%
    dplyr::mutate(pos = cumsum(n) - n/2)

  pp = ggplot2::ggplot(data = data.pk.tmp, aes(x=as.factor(TREATC), y=n, fill=COHORTN)) +
    geom_bar(stat="identity", color = 'black')+
    geom_text(aes(y=pos, label=n),
              color="black", size=3.5) +
    scale_fill_brewer(palette="Set3") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90))+
    labs(fill = "Cohort")+xlab("Treatment")+ylab("Number of subjects")


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
#' @section another section
#' @export
correlation.matrix <- function(df) {
  p <- GGally::ggpairs(dat,
                       lower = list(continuous = wrap(lowerFn, method = "lm")),
                       diag = list(continuous = wrap("barDiag", colour = "blue")),
                       upper = list(continuous = wrap("cor", size = 4)))
  return(p)
}





