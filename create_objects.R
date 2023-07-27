#' ---
#' project: FunRZ    ######################################################
#' title:   Create Objects
#' author:  Reto Zihlmann <retozihlmann@outlook.com>
#' date:    2023-06-19 14:16:16
#' output:  github_document   #############################################
#' ---


# Packages ----------------------------------------------------------------

library(magrittr,warn.conflicts = F); library(tidyverse,warn.conflicts = F)
oldtheme <- theme_set(theme_bw())


# Create objects ----------------------------------------------------------

data_type_formats <- tribble(
  ~data_type, ~col,      ~readr_abbr,
  "lgl",      "#fb9a99", "l",
  "int",      "#1f78b4", "i",
  "dbl",      "#a6cee3", "d",
  "chr",      "#daf5c4", "c",
  "fct",      "#9ce660", "f",
  "ord",      "#33a02c", "f",
  "date",     "#fdbf6f", "D",
  "dttm",     "#ff7f00", "T",
  # "drtn",     "#cab2d6", "?", ## difftime, better use numeric
  "time",     "#6a3d9a", "t"
)


# Save objects ------------------------------------------------------------




# save(data_type_formats, file="data/data_type_formats.RData")
