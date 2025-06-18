#' Create Project Folder Structure
#'
#' Create template folder structure for RStudio project. Existing folders are not overwritten.
#'
#' @return Returns invisible logical vector indicating if folder creation was successful
#' @export
proj_temp <- function() {
  out1 <- dir.create("01_dat")
  out2 <- dir.create("02_R")
  out3 <- dir.create("03_out")
  invisible(c(folder1 = out1, folder2 = out2, folder3 = out3))
}
