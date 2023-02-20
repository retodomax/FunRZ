#' plot histograms in pair plot
#'
#' panel function to plot histograms in pair plot
#' @param x Variable for which a histogram should be plotted
#' @return plot a histogram
#' @examples pairs(iris, lower.panel = panel.smooth, diag.panel = panel.hist,
#'       upper.panel = panel.cor)
#' @export
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="#E34A33",...)
}
