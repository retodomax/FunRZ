#' Significance Star Function
#'
#' **Deprecated: Use the base R functions in the example instead.** This function takes any vector of p values and returns the vector in asterisk notation
#' with p<0.05 (`*`), <0.01 (`**`), and <0.001 (`***`)
#'
#' @param pval A p value to be expressed in asterisk notation
#' @return Vector of asterisks
#' @examples
#' p <- c(0.01, 0.02, 0.05, 0.06)
#' FunSigStar(p)
#'
#' ## Can also be achieved with base functions
#' format(symnum(p, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
#'               symbols = c("***", "**", "*", ".", " "), na = FALSE))
#' @export
FunSigStar <- function(pval) {
  stars <- NULL
  for(i in 1:length(pval)){
    if (pval[i] <= 0.001) {
      stars <- c(stars, " ***")
    }
    else if (pval[i] <= 0.01) {
      stars <- c(stars, "  **")
    }
    else if (pval[i] <= 0.05) {
      stars <- c(stars, "   *")
    }
    else {stars <- c(stars, "    ")
    }
  }
  return(stars)
}
