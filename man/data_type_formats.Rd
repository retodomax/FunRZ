% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_objects.R
\docType{data}
\name{data_type_formats}
\alias{data_type_formats}
\title{Data Type Formatting}
\format{
An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 9 rows and 3 columns.
}
\usage{
data_type_formats
}
\description{
Data Types and how they should be formatted in data archive pipeline (column_rename_fun, ...)
}
\examples{
library(tidyverse)
data_type_formats \%>\%
  mutate(rn = row_number()) \%>\%
  pivot_longer(cols = c(data_type, readr_abbr),
               names_to = "variable") \%>\%
  mutate(variable = factor(variable)) \%>\%
  ggplot(aes(y = rn, x = variable, fill = col)) +
  geom_label(aes(label = value)) +
  scale_y_reverse() +
  scale_fill_identity() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  )
}
\seealso{
\code{\link[=create_rename_table]{create_rename_table()}}, \code{\link[=column_rename_fun]{column_rename_fun()}}, \code{\link[=create_excel_archive]{create_excel_archive()}}
}
\keyword{datasets}
