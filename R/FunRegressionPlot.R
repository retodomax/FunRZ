#' Plot linear regression
#'
#' Computes a linear model (lm), plots the regression line, the linear equation and the
#' adj R square value.
#' @param x independent variable
#' @param y dependent variable
#' @param edge vector in the form c(x,y), defines the position of
#' the linear equation and the adj R square value. The coordinates are given as
#' normalized device coordinates.
#' @param col color of regression line and text
#' @param regression_line logical, should a regression line be plotted
#' @param x_log_transformed logical, indicate a log transformation of the x axis
#' The formula would be lm(log(x), y). Possible in combination with y_log_transformed.
#' @param y_log_transformed logical, indicate a log transformation of the y axis
#' The formula would be lm(x, log(y)). Possible in combination with x_log_transformed.
#' @return invisible output of lm()
#' @examples df <- data.frame(x = 1:10, y = c(1:9, 11))
#' @examples plot(df$x, df$y)
#' @examples FunRegressionPlot(x = df$x, y = df$y)
#'
#' @examples plot(df$x, log(df$y))
#' @examples FunRegressionPlot(x = df$x, y = log(df$y), y_log_transformed = T)
#'
#' @examples plot(df$x, log(df$y))
#' @examples FunRegressionPlot(x = df$x, y = log(df$y), y_log_transformed = T)
#' @export
FunRegressionPlot <- function(x = rnorm(10,10,2), y = rnorm(10,10,2), edge = c(0.1,0.9),
                              col = "black", regression_line = T,
                              x_log_transformed = F, y_log_transformed = F){
  mod <- lm(y ~ x)
  if(regression_line){
    abline(mod, col = col)
  }
  x_span <- (par("usr")[2]-par("usr")[1])
  y_span <- (par("usr")[4]-par("usr")[3])
  x_cor <- par("usr")[1]+x_span*edge[1]
  y_cor <- par("usr")[3]+y_span*edge[2]
  mod_eq <- c(format(round(mod$coefficients["(Intercept)"],2), nsmall = 2),
              format(round(mod$coefficients[2],2), nsmall = 2))
  mod_r_sq <- format(round(summary(mod)$adj.r.squared,2), nsmall = 2)
  mod_eq_abs <- c(format(round(abs(mod$coefficients["(Intercept)"]),2), nsmall = 2),
                  format(round(abs(mod$coefficients[2]),2), nsmall = 2))
  vorzeichen <- if(sign(mod$coefficients[2]) == -1){
    "-"} else {
      "+"
    }
  if(x_log_transformed & y_log_transformed){
    text(x = x_cor, y = y_cor, pos = 4,
         labels = bquote(y == e^.(mod_eq[1]) * x^.(mod_eq[2])),
         cex = 1.2, col = col)
  } else if(x_log_transformed){
    text(x = x_cor, y = y_cor, pos = 4,
         labels = bquote(y == .(mod_eq[1]) ~ .(vorzeichen) ~ .(mod_eq_abs[2]) ~ log(x)),
         cex = 1.2, col = col)
  } else if(y_log_transformed){
    text(x = x_cor, y = y_cor, pos = 4,
         labels = bquote(y == e^.(mod_eq[1])*e^{.(mod_eq[2])*x}),
         cex = 1.2, col = col)
  } else {
    text(x = x_cor, y = y_cor, pos = 4,
         labels = bquote(y == .(mod_eq[1]) ~ .(vorzeichen)*.(mod_eq_abs[2])*x),
         cex = 1.2, col = col)
  }
  text(x = x_cor, y = y_cor-y_span*0.08, pos = 4,
       labels = bquote(R[adj]^2 == .(mod_r_sq)),
       col = col)
  invisible(mod)
}
