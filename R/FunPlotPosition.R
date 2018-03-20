#' Get absolute plot coordinates from relative plot coordinates
#'
#' The function transformes relative plot coordinates to absolute coordinates for the current plot.
#' @param edge vector c(x,y) with the relative coordinate given in the range of [0,1]
#' @return vector c(x,y) with the absolute coordinates for the current plot
#' @examples plot(1:10, 21:30)
#' @examples text(FunPlotPosition()[1], FunPlotPosition()[2], "Hello")
#' @examples FunPlotPosition()
#' @export
FunPlotPosition <- function(edge = c(0.1,0.9)){
  x_span <- (par("usr")[2]-par("usr")[1])
  y_span <- (par("usr")[4]-par("usr")[3])
  x_cor <- par("usr")[1]+x_span*edge[1]
  y_cor <- par("usr")[3]+y_span*edge[2]
  cor <- c(x_cor, y_cor)
  return(cor)
}
