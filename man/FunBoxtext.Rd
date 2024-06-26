% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FunBoxtext.R
\name{FunBoxtext}
\alias{FunBoxtext}
\title{Add text with background box to a plot}
\usage{
FunBoxtext(
  x,
  y,
  labels = NA,
  col.text = NULL,
  col.bg = NA,
  border.bg = NA,
  adj = NULL,
  pos = NULL,
  offset = 0.5,
  padding = c(0.5, 0.5),
  cex = 1,
  font = graphics::par("font")
)
}
\arguments{
\item{x}{numeric vector of x-coordinates where the text labels should be
written. If the length of \code{x} and \code{y} differs, the shorter one
is recycled.}

\item{y}{numeric vector of y-coordinates where the text labels should be
written.}

\item{labels}{a character vector specifying the text to be written.}

\item{col.text}{the colour of the text}

\item{col.bg}{color(s) to fill or shade the rectangle(s) with. The default
\code{NA} means do not fill, i.e., draw transparent rectangles.}

\item{border.bg}{color(s) for rectangle border(s). The default \code{NA}
omits borders.}

\item{adj}{one or two values in \link{0, 1} which specify the x (and optionally
y) adjustment of the labels.}

\item{pos}{a position specifier for the text. If specified this overrides
any adj value given. Values of 1, 2, 3 and 4, respectively indicate
positions below, to the left of, above and to the right of the specified
coordinates.}

\item{offset}{when \code{pos} is specified, this value gives the offset of
the label from the specified coordinate in fractions of a character width.}

\item{padding}{factor used for the padding of the box around
the text. Padding is specified in fractions of a character width. If a
vector of length two is specified then different factors are used for the
padding in x- and y-direction.}

\item{cex}{numeric character expansion factor; multiplied by
code{par("cex")} yields the final character size.}

\item{font}{the font to be used}
}
\value{
Returns the coordinates of the background rectangle(s). If
multiple labels are placed in a vactor then the coordinates are returned
as a matrix with columns corresponding to xleft, xright, ybottom, ytop.
If just one label is placed, the coordinates are returned as a vector.
}
\description{
\code{boxtext} places a text given in the vector \code{labels}
onto a plot in the base graphics system and places a coloured box behind
it to make it stand out from the background.
}
\examples{
## Create noisy background
plot(x = runif(1000), y = runif(1000), type = "p", pch = 16,
col = "#40404060")
boxtext(x = 0.5, y = 0.5, labels = "some Text", col.bg = "#b2f4f480",
    pos = 4, font = 2, cex = 1.3, padding = 1)
}
\author{
Ian Kopacka
}
