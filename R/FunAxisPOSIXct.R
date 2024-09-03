#' Axis Function for POSIXct
#'
#' Make axis, ticks, lables and axistitle in one step for timeseries.
#' Attention: axis.POSIXct is currently only implemented for the x axis.
#' @param xtitle x axis title as string
#' @param xline space between x axis title and plotting region in mex
#' @param xpadj space between x axis title and plotting region may be
#' disturbed if you use bquote(). Adapt space with e.g. padj = 0.1,
#' padj = 0 means right or top alignment, and padj = 1 means left or bottom alignment.
#' @param xcextitle x axis title character extension (font size)
#' @param xcexlab x axis labels character extension (font size)
#' @param xat the points at which x axis tick-marks are to be drawn.
#' @param xlabels logical or character vector of labels to be placed at the tickpoints.
#' @param xtick a logical value specifying whether tickmarks and an axis line should
#' be drawn.
#' @param xtick_lwd line width of ticks on the x axis
#' @param x_mtext list of arguments passed to bottom x axis' mtext()
#' e.g. outer, adj, col, font, ...
#' @param x_axis_1 list of arguments passed to bottom x axis' axis()
#' e.g. las, pos, outer, font, lty, lwd, lwd.ticks, col, col.ticks, hadj, ...
#' @param x_axis_2 list of arguments passed to top x axis' axis()
#' @param xmt logical, should minor ticks on x axis be plotted
#' @param xmtn number of intervals in which to divide the area between major tick marks on the X-axis.
#' Set to 1 to suppress minor tick marks.
#' @param xmtratio ratio of lengths of minor x tick marks to major x tick marks.
#' The length of major tick marks is retrieved from par("tck").
#' @param xmtbothsides should minor ticks be plotted on both x axis (bottom and top)
#' @param xmt_args list of arguments passed to x axis' axis() to plot minor ticks.
#' e.g. lwd, lty, col, ...
#' @param y... each argument for the y axis
#' @param box_lwd line width of box arround the plot
#' @param box_args list of arguments passed to box() to plot box arround plot.
#' e.g. which, lty, ...
#' @param xtimevec vector with all date-times to produce the plot (x argument of the plot function)
#' @param format character vector specifying the way to print the date-time e.g %m.%y. See strptime.
#' @return axis, ticks, labels and axistitle
#' @author Reto Zihlmann
#' @seealso box, mtext, axis
#' @examples
#' library(lubridate)
#' x <- now() + days(0:7)
#' y <- rnorm(8)
#' plot(x,y, type = "l", axes = F, ann = F)
#' FunAxisPOSIXct(xtimevec = x, format = "%d.%m")
#' @export
FunAxisPOSIXct <- function(xtitle = "x axis", ytitle = "y axis",
                    xline = 2, yline = 2, xpadj = 0, ypadj = 0,
                    xcextitle = par("cex"), ycextitle = par("cex"),
                    xcexlab = par("cex"), ycexlab = par("cex"),
                    xat = NULL, yat = NULL,
                    xlabels = TRUE, ylabels = TRUE,
                    xtick = TRUE, ytick = TRUE,
                    xtick_lwd = 1.5, ytick_lwd = 1.5,
                    x_mtext = list(), y_mtext = list(),
                    x_axis_1 = list(), y_axis_1 = list(),
                    x_axis_2 = list(), y_axis_2 = list(),
                    xmt = T, ymt = T,
                    xmtn = 2, ymtn = 2,
                    xmtratio = 0.5, ymtratio = 0.5,
                    xmtbothsides = T, ymtbothsides = T,
                    xmt_args = list(), ymt_args = list(),
                    box_lwd = 1.5, box_args = list(),
                    xtimevec, format = ""){
  # draw box
  do.call(box, c(list(lwd = box_lwd), box_args))

  # define arguments
  x_mtext_args <- c(list(text = xtitle, side = 1, line = xline,
                         padj = xpadj, cex = xcextitle), x_mtext)
  y_mtext_args <- c(list(text = ytitle, side = 2, line = yline,
                         padj = ypadj, cex = ycextitle, las = 0), y_mtext)
  x_axis_1_args <- c(list(x = xtimevec, format = format, side = 1, lwd.ticks = xtick_lwd, cex.axis = xcexlab,
                          at = xat, labels = xlabels, tick = xtick), x_axis_1)
  y_axis_1_args <- c(list(side = 2, lwd.ticks = ytick_lwd, cex.axis = ycexlab,
                          at = yat, labels = ylabels, tick = ytick), y_axis_1)
  x_axis_2_args <- c(list(x = xtimevec, side = 3, lwd.ticks = xtick_lwd, labels = F,
                          at = xat, tick = xtick), x_axis_2)
  y_axis_2_args <- c(list(side = 4, lwd.ticks = ytick_lwd, labels = F,
                          at = yat, tick = ytick), y_axis_2)

  # apply mtext and axis with defined arguments
  do.call(mtext, x_mtext_args)
  do.call(mtext, y_mtext_args)
  tick.pos <- do.call(axis.POSIXct, x_axis_1_args)
  do.call(axis, y_axis_1_args)
  do.call(axis.POSIXct, x_axis_2_args)
  do.call(axis, y_axis_2_args)

  # define FunMt (minor ticks function)
  FunMt <- function(usr = 1:2, axp = "xaxp", mtn = xmtn,
                    side = 1, mt_args = xmt_args, mtratio = xmtratio,
                    mtbothsides = xmtbothsides, oposit = 3){
    range <- par("usr")[usr]
    tick.pos <- par(axp)
    distance.between.minor <- (tick.pos[2] - tick.pos[1])/tick.pos[3]/mtn
    possible.minors <- tick.pos[1] - (0:100) * distance.between.minor
    low.candidates <- possible.minors >= range[1]
    low.minor <- if (any(low.candidates))
      min(possible.minors[low.candidates]) else tick.pos[1]
    possible.minors <- tick.pos[2] + (0:100) * distance.between.minor
    hi.candidates <- possible.minors <= range[2]
    hi.minor <- if (any(hi.candidates))
      max(possible.minors[hi.candidates]) else tick.pos[2]
    axis.args <- c(list(side = side,
                        seq(low.minor, hi.minor, by = distance.between.minor),
                        labels = FALSE,
                        tcl = par("tcl") * mtratio), mt_args)
    do.call(axis, axis.args)
    if (mtbothsides) {
      axis.args.oposit <- axis.args
      axis.args.oposit[[1]] <- oposit
      do.call(axis, axis.args.oposit)
    }
  }

  # define FunMt_POSIXct (minor ticks function for POSIXct axis)
  FunMt_POSIXct <- function(usr = 1:2, axp = "xaxp", mtn = xmtn,
                    side = 1, mt_args = xmt_args, mtratio = xmtratio,
                    mtbothsides = xmtbothsides, oposit = 3){
    range <- par("usr")[usr]
    tick.pos <- as.numeric(tick.pos)
    tick.pos <- c(tick.pos[c(1, length(tick.pos))], length(tick.pos) - 1)
    distance.between.minor <- (tick.pos[2] - tick.pos[1])/tick.pos[3]/mtn
    possible.minors <- tick.pos[1] - (0:100) * distance.between.minor
    low.candidates <- possible.minors >= range[1]
    low.minor <- if (any(low.candidates))
      min(possible.minors[low.candidates]) else tick.pos[1]
    possible.minors <- tick.pos[2] + (0:100) * distance.between.minor
    hi.candidates <- possible.minors <= range[2]
    hi.minor <- if (any(hi.candidates))
      max(possible.minors[hi.candidates]) else tick.pos[2]
    axis.args <- c(list(side = side,
                        seq(low.minor, hi.minor, by = distance.between.minor),
                        labels = FALSE,
                        tcl = par("tcl") * mtratio), mt_args)
    do.call(axis, axis.args)
    if (mtbothsides) {
      axis.args.oposit <- axis.args
      axis.args.oposit[[1]] <- oposit
      do.call(axis, axis.args.oposit)
    }
  }

  # apply FunMt
  if(xmt){
    FunMt_POSIXct()
  }
  if(ymt){
    FunMt(usr = 3:4, axp = "yaxp", mtn = ymtn,
          side = 2, mt_args = ymt_args, mtratio = ymtratio,
          mtbothsides = ymtbothsides, oposit = 4)
  }
}
