#' Get absolute y coordinates from relative y coordinates
#'
#' The function transformes relative y coordinates to absolute y coordinates for the current plot.
#' @param rel relative y coordinate given in the range of [0,1]
#' @return absolute y coordinate for the current plot
#' @examples plot(1:10, 21:30)
#' @examples FunYPosition(0.5)
#' @export
FunYPosition <- function(rel = 0.1){
  y_span <- (par("usr")[4]-par("usr")[3])
  par("usr")[3]+y_span*rel
}
