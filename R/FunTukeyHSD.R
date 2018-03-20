#' boxplots with cld
#'
#' plot boxplots with compact letter display of pairwise comparisons
#' @param mod model passed to aov() with value ~ factor
#' @param data data.frame with the variables
#' @param upp_mar specify the number of lines on the top of the plot.
#' If there are many letters extra space is necessary
#' @param col vector with color to fill the boxes of the boxplot
#' @param ylab y axis title
#' @param xlab x axis title
#' @param main main title
#' @param orientation of axis labels see par()
#' @param add_legend logical if a legend should be add
#' @param legend_pos position specified as character (right, topleft,...)
#' @param legend string vector of length ≥ 1 to appear in the legend
#' @param fill vector with color to fill boxes in legend
#' @return boxplot with cld
#' @examples df <- data.frame(x = c(rnorm(80, 10,2), rnorm(20, 7, 2)),
#' myfac = c(rep(c("a","b", "c", "d"), 20), rep("e", 20)))
#' @examples FunTukeyHSD(mod = x ~ myfac, data = df, upp_mar = 5, col = c(rep("red", 3), "blue", "blue"),
#'             ylab = "y axis", las = 2, add_legend = T, legend = c("treatment", "control"),
#'             fill = c("blue", "red"))
#' @export
FunTukeyHSD <- function(mod = y ~ x, data = dat, upp_mar = 4,
                        col = NA, ylab = "", xlab = "", main = "", las = 3,
                        add_legend = F, legend_pos = "topright", legend = NULL,
                        fill = "white"){
  mod_hsd <- aov(mod, data = data)
  require(multcomp)
  args <- list("Tukey")
  names(args) <- as.character(mod[3])
  cmp <- do.call(mcp, args)
  tuk <- glht(mod_hsd, linfct = cmp)
  tuk.cld <- cld(tuk)
  par_org <- par(oma = c(0,0,2,0),mar = c(6,5,upp_mar,2)+0.1); on.exit(par(par_org))
  plot(tuk.cld, col = col, ylab = ylab, xlab = xlab, main = "", las = las)
  title(main = main, line = 0, outer = T)
  if(add_legend){
    legend(legend_pos, legend = legend, fill = fill)
  }
}
