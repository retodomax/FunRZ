#' Residual plot with resampling
#'
#' \code{FunResplot} plots four model diagnostic residual plots to check model
#' assumptions.
#'
#' @param obj model (e.g. object of type \code{lm})
#' @param plots 1=Tukey-Anscombe, 2=Normal, 3=Scale-Location, 4=Leverage.
#' Several can be plotted e.g. \code{plots = c(1,2)}
#' @param method how should resampling residuals be obtained for Tukey-Anscombe plots.
#' rnorm: residuals are drawn from a normal distribution with variance equal
#' the estimated variance of the observed residuals.
#' resampling: residuals are drawn from observed residuals with replacment
#' @param num number of resampling iterations (number of gray lines)
#' @param set_seed boolean. Automatically set a constant seed in the function
#' @param change_mfrow boolean. Automatically adjust \code{mfrow} if
#' \code{length(plots) > 1}
#' @param plot_title boolean. Should there be a title above the figure.
#'
#' @return Nothing is returned. Plots are plotted
#' @author Marcel Dettling
#' @examples
#' ## generate data
#' x <- 1:100
#' y <- rnorm(100, x + 10, 10)
#'
#' ## fit model
#' fit <- lm(y ~ x)
#' plot(y ~ x)
#' abline(fit)
#'
#' ## plot residual plots
#' FunResplot(fit)
#' FunResplot(fit, plots = 1:2, num = 20)
#'
#' ## another good method
#' ## see anova r-script 05_nitrogen
#' nitro.sim <- nitro
#'
#' set.seed(12)
#' opar <- par(mfrow = c(4, 5))
#' for(i in 1:20){
#'   nitro.sim[, "y"] <- simulate(fit)
#'   fit.sim <- update(fit, data = nitro.sim)
#'   plot(fit.sim, which = 1)
#' }
#' par(opar)
#'
#' @export
FunResplot <- function(obj, plots=1:4,
                       method = c("rnorm", "resampling"),
                       num = 100, set_seed = FALSE, change_mfrow = TRUE,
                       plot_title = TRUE) {
  ## Last change: replaced all loess.smooth() with lowess()
  ##              since loess.smooth fits strange smoother in case of small
  ##              number of unique fitted values (not many unique x values)

  ## Set number of frames in the plot
  if(change_mfrow){
    if (length(plots)>=3) opar <- par(mfrow=c(2,2))
    if (length(plots)==2) opar <- par(mfrow=c(1,2))
    if (length(plots)==1) opar <- par(mfrow=c(1,1))
  }

  ## Title vector
  if(plot_title){
    my_title <- c("Tukey-Anscombe Plot with Resampling",
                  "Normal QQ Plot with Resampling",
                  "Scale-Location Plot with Resampling",
                  "Leverage Plot")
  } else {
    my_title <- vector("character", 4)
  }


  ## Set random seed that plots look always the same
  if(set_seed) set.seed(21)

  ## Tukey-Anscombe-Plot with Resampling
  if (1 %in% plots)
  {
    plot(fitted(obj), resid(obj), pch=20,
         xlab="Fitted Values", ylab="Residuals")
    title(my_title[1])
    if(method[1] == "rnorm"){
      for (i in 1:num){
        lines(lowess(fitted(obj),
                     rnorm(length(fitted(obj)), 0, sigma(obj))),
              col="gray")
      }
    }
    if(method[1] == "resampling"){
      for (i in 1:num){
        lines(lowess(fitted(obj),
                     sample(resid(obj, replace=TRUE))),
              col="grey")
      }
    }
    abline(h=0, lty=2)
    points(fitted(obj), resid(obj), pch=20); box()
    lines(lowess(fitted(obj), resid(obj)), col="red")
  }

  ## Normal Plot with Resampling
  if (2 %in% plots)
  {
    qq <- qqnorm(rstandard(obj), pch=20,
                 main= my_title[2],
                 ylab= "Standardized Residuals")
    for (i in 1:num){
      lines(sort(qq$x),
            sort(rnorm(length(qq$y), mean(qq$y), sd(qq$y))),
            col="grey")
    }
    points(qq$x, qq$y, pch=20); box()
    qqline(rstandard(obj), lty=2)
  }

  ## Scale-Location-Plot with Resampling
  if (3 %in% plots)
  {
    plot(fitted(obj), sqrt(abs(rstandard(obj))), pch=20,
         ylab="sqrt(abs(Standardized Residuals))", xlab="Fitted Values",
         ylim=c(0, range(sqrt(abs(rstandard(obj))), na.rm=TRUE)[2]),
         main= my_title[3])
    if(method[1] == "rnorm"){
      for (i in 1:num){
        lines(lowess(fitted(obj),
                     sqrt(abs(rnorm(length(fitted(obj)), 0, sd(rstandard(obj)))))),
              col="grey")
      }
    }
    if(method[1] == "resampling"){
      for (i in 1:num){
        lines(lowess(fitted(obj),
                     sample(sqrt(abs(rstandard(obj))), replace=TRUE)),
              col="grey")
      }
    }
    points(fitted(obj), sqrt(abs(rstandard(obj))), pch=20); box()
    lines(lowess(fitted(obj), sqrt(abs(rstandard(obj)))), col="red")
  }

  ## Leverage Plot (without Resampling, taken from plot.lm()
  if (4 %in% plots)
  {
    plot(obj, which=5, pch=20, caption="")
    title(my_title[4])
  }

  if(change_mfrow){
    par(opar)
  }
}
