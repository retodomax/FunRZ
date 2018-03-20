#' Side Boxplots
#'
#' Takes any vector of p values and returnes the vector in asterisk notation
#' with p<0.05 (*), <0.01 (**), and <0.001 (***)
#' @param x variable on x axis
#' @param y variable on y axis
#' @param separator factor which separates the observations in two groups
#' @param col color to fill boxplots
#' @param x_start,y_start startpoint of ploting region for boxplot in normalized
#' device coordinates (see par(fig))
#' @param main title of plot
#' @param ylim lower and upper limit on y axis. Should be the same as in the scatterplot
#' @param xlim lower and upper limit on x axis. Should be the same as in the scatterplot
#' @return sideboxplots
#' @examples df <- data.frame(x = c(rnorm(80, 10,2), rnorm(20, 7, 2)),
#'                  y = c(rnorm(100,80,10)),
#'                  myfac = c(rep(c("a"), 80), rep("b", 20)))
#' @examples opar <- par(fig=c(0,0.8,0,0.8), new = F, mar = c(6,5,3,2)+0.1)
#' @examples plot(df$x, df$y, pch = c(17:16)[df$myfac], col = c("red", "blue")[df$myfac])
#' @examples FunSideBoxplots(x = df$x, y = df$y, separator = df$myfac,
#'                 col = c("red","blue"))
#' @examples par(opar)
#' @export
FunSideBoxplots <- function(x = rnorm(10,10,2), y = rnorm(10,12,2), separator = NULL, col = "white",
                            x_start = 0.55, y_start = 0.65, main = "",
                            ylim = NULL, xlim = NULL){
  par_org <- par(fig=c(0,0.8,x_start,1), new = T); on.exit(par(par_org))
  boxplot(x ~ separator, horizontal = T, axes = F, col = col, xlim = xlim)
  t_test <- t.test(x ~ separator)
  text(x = par("usr")[2] + 0.05*diff(par("usr")[1:2]), y = mean(par("usr")[3:4]),
       labels = gsub(" ", "", FunSigStar(t_test$p.value), fixed = TRUE),
       cex = 2, xpd = T, srt = 270)
  par(fig=c(y_start,1,0,0.8), new = T)
  boxplot(y ~ separator, axes = F, col = col, ylim = ylim)
  t_test <- t.test(y ~ separator)
  text(x = mean(par("usr")[1:2]), y = par("usr")[4],
       labels = gsub(" ", "", FunSigStar(t_test$p.value), fixed = TRUE),
       pos = 3, cex = 2, xpd = T)
  title(main = main, line = -2, outer = T)
}
