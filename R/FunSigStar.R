#' Significance Star Function
#'
#' Takes any vector of p values and returnes the vector in asterisk notation
#' with p<0.05 (*), <0.01 (**), and <0.001 (***)
#' @param pval A p value to be expressed in asterisk notation
#' @return asterisk
#' @examples FunSigStar(0.01)
#' @examples FunSigStar(c(0.02, 0.05, 0.06))
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
