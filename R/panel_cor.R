#' plot correlation
#'
#' panel function to plot correlation in pair plot
#' @param x,y Variables to calculate correlation
#' @param digits integer indicating the number of decimal places to be used
#' @param prefix character what should be put before number
#' @param cex.cor character expansion of the plotted correlation
#' @param tex_pos vector in the form c(x,y), defines the position of
#' the correlation. The coordinates are given as
#' normalized device coordinates.
#' @return plot correlation
#' @examples pairs(iris, lower.panel = panel.smooth, diag.panel = panel.hist,
#'       upper.panel = panel.cor)
#' @export
#put correlations on the upper panel
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, text_pos = c(0.5,0.5), ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use="pairwise.complete")
  r2 <- abs(cor(x, y, use="pairwise.complete"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(text_pos[1], text_pos[2], txt, cex = cex.cor * r2)
}
