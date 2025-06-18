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


## Data Type Formats ########################
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

## DSM colors ###############################
dsm_col <- tibble::tribble(
  ~ R, ~G, ~B,
  195, 230, 225,
  118, 208, 194,
  68, 180, 161,
  39, 126, 110,
  8,  67,  55,

  203, 231, 245,
  137, 199, 232,
  72, 162, 207,
  48, 114, 151,
  11,  60,  86,

  238, 219, 247,
  217, 176, 234,
  189, 105, 215,
  145,  56, 176,
  88,  22, 109,

  245, 208, 201,
  230, 127, 124,
  211,  66,  82,
  157,  40,  53,
  109,  14,  23,

  245, 216, 184,
  255, 170,  77,
  240, 139,  31,
  204, 116,  20,
  153,  79,   0,

  237, 242, 174,
  203, 217,  56,
  178, 191,   0,
  114, 122,   6,
  53,  59,   9,

  240, 240, 240,
  204, 188, 184,
  179, 151, 143,
  137, 121, 116,
  77,  60,  53
)
dsm_col <- dsm_col %>%
  rowwise() %>%
  mutate(hex = rgb(R, G, B, maxColorValue = 255)) %>%
  ungroup() %>%
  mutate(color = fct_inorder(rep(c("green", "blue", "purple", "red", "orange", "yellow", "brown"), each = 5)),
         hue = rep(1:5, 7))
# with(FunRZ::dsm_col, plot(x = as.numeric(factor(color)), y = hue, bg = hex,
#                           cex = 9, pch = 22, xlim = c(0,8), ylim = c(0,6)))

## RCol #####################################
### RCol_v1:
### https://coolors.co/palette/606c38-283618-fefae0-dda15e-bc6c25
# RColOld <- c(darkMossGreen = "#606c38ff",
#           pakistanGreen = "#283618ff",
#           cornsilk = "#fefae0ff",
#           earthYellow = "#dda15eff",
#           tigersEye = "#bc6c25ff")

### RCol_v3
RCol <- tibble::tibble(
  hex = c("#edf0e3", "#cad3ab", "#a7b673", "#7c8c49", "#4b542c",
          "#f8eadc", "#e9c296", "#da9950", "#af6e25", "#694216",
          "#fdf6d6", "#fae485", "#f6d233", "#cca809", "#7a6505",
          "#f5e1df", "#e1a59e", "#cd695d", "#a23f32", "#61261e",
          "#e6e8ee", "#b4bacb", "#828ca8", "#57627d", "#343b4b"),
  color = fct_inorder(rep(c("green", "orange", "yellow", "red", "blue"), each = 5)),
  hue = rep(1:5, 5)) %>%
  rowwise() %>%
  mutate(R = col2rgb(hex)[1],
         G = col2rgb(hex)[2],
         B = col2rgb(hex)[3]) %>%
  ungroup() %>%
  select(R, G, B, hex, color, hue)

# png("RCol_v3.png", width = 11.4, height = 11.4, units = "cm", res = 300)
# par(mar = c(5,5,1,1))
# with(RCol, plot(x = as.numeric(factor(color)), y = hue, bg = hex,
#                 cex = 9, pch = 22, xlim = c(0.6,5.4), ylim = c(0.6,5.4),
#                 xaxt = "n", xlab = "color"))
# with(RCol, text(x = as.numeric(factor(color)), y = hue-0.3, labels = hex, cex = 0.3))
# axis(1, at = 1:5, labels = c("green", "orange", "yellow", "red", "blue"))
# dev.off()

# https://coolors.co
# https://mdigi.tools/color-shades
# https://leonardocolor.io/theme.html


# Save objects ------------------------------------------------------------

# save(data_type_formats, file="data/data_type_formats.RData")
# save(dsm_col, file = "data/dsm_col.RData")
# save(RCol, file = "data/RCol.RData")
