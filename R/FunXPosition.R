#' Get absolute x coordinates from relative x coordinates
#'
#' The function transformes relative x coordinates to absolute x coordinates for the current plot.
#' @param rel relative x coordinate given in the range of [0,1]
#' @return absolute x coordinate for the current plot
#' @examples plot(1:10, 21:30)
#' @examples FunXPosition(0.5)
#' @export
FunXPosition <- function(rel = 0.1){
  x_span <- (par("usr")[2]-par("usr")[1])
  par("usr")[1]+x_span*rel
}
