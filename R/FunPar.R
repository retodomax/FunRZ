#' Preferred par values funktion
#'
#' Set parameters (par) in R to favorit default values. Returns previous par values
#' invisible.
#' @param mar smaller plot margin
#' @param xaxs do not add 4 percent to x axis' lim
#' @param yaxs do not add 4 percent to y axis' lim
#' @param tcl ticks look inside
#' @param las turn y axis labels
#' @param mgp position of axis titles and labels in lines
#' @param lab preferd number of ticks (4 instead of 5)
#' @return invisible original par values
#' @author Reto Zihlmann
#' @seealso par()
#' @examples plot(1:10)           # plotting with original par settings
#' @examples opar <- FunPar()     # set new par settings
#' @examples plot(1:10)
#' @examples par(opar)            # reset original par settings
#' @examples plot(1:10)
#' @export
FunPar <- function(mar = c(3.5,3.5,2,2),
                   xaxs = "i", yaxs = "i",
                   tcl = 0.4, las = 1,
                   mgp = c(2,0.5,0),
                   lab = c(4,4,7),
                   family = "Segoe",
                   cex = 0.9){
  windowsFonts(Segoe = windowsFont("Segoe UI"),
               Segoe_li = windowsFont("Segoe UI Semilight"))
  x <- par(mar = mar,                  # smaller plot margin
           xaxs = xaxs, yaxs = yaxs,   # do not add 4% to each axis lim
           tcl = tcl,                  # ticks look inside
           las = las,                  # turn y axis labels
           mgp = mgp,                  # position of axis titles and labels in lines
           lab = lab,                  # preferd number of ticks (4 instead of 5)
           family = family,            # prefered font family
           cex = cex)                  # prefered character expansion factor (font size)
  invisible(x)
}
