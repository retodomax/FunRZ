% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/join_many_to_many_inspect.R
\name{join_many_to_many_inspect}
\alias{join_many_to_many_inspect}
\title{Inspect which rows cause a many-to-many warning}
\usage{
join_many_to_many_inspect(x, y, by = NULL, show_rows = c("none", "x", "y"))
}
\arguments{
\item{x}{see \code{\link[dplyr:mutate-joins]{dplyr::inner_join()}}}

\item{y}{see \code{\link[dplyr:mutate-joins]{dplyr::inner_join()}}}

\item{by}{see \code{\link[dplyr:mutate-joins]{dplyr::inner_join()}}}

\item{show_rows}{character, show the rows which cause the problem in table \code{x} or \code{y}. If \code{none} it returns a counting table how often a certain joining-key is observed in \code{x} and \code{y}.}
}
\value{
tbl with number of multiple observed joining keys, or rows which cause the problem in table \code{x} or \code{y}
}
\description{
Inspect which rows cause a many-to-many warning
}
\examples{
x <- tibble(key = c(1, 1:3),
value = letters[key])
y <- tibble(key = c(1,1,1,1,2),
            value2 = 1:5)
x \%>\% inner_join(y)
join_many_to_many_inspect(x = x, y = y, show_rows = "none")
join_many_to_many_inspect(x = x, y = y, show_rows = "x")
}
