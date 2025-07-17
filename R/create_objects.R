
#' Data Type Formatting
#'
#' Data Types and how they should be formatted in data archive pipeline (column_rename_fun, ...)
#' @seealso [create_rename_table()], [column_rename_fun()], [create_excel_archive()]
#' @export
#' @examples
#' library(tidyverse)
#' data_type_formats %>%
#'   mutate(rn = row_number()) %>%
#'   pivot_longer(cols = c(data_type, readr_abbr),
#'                names_to = "variable") %>%
#'   mutate(variable = factor(variable)) %>%
#'   ggplot(aes(y = rn, x = variable, fill = col)) +
#'   geom_label(aes(label = value)) +
#'   scale_y_reverse() +
#'   scale_fill_identity() +
#'   theme(
#'     axis.title.y = element_blank(),
#'     axis.text.y  = element_blank(),
#'     axis.ticks.y = element_blank()
#'   )
data_type_formats <- tibble::tribble(
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




### RCol_v1:
### https://coolors.co/palette/606c38-283618-fefae0-dda15e-bc6c25
# RColOld <- c(darkMossGreen = "#606c38ff",
#           pakistanGreen = "#283618ff",
#           cornsilk = "#fefae0ff",
#           earthYellow = "#dda15eff",
#           tigersEye = "#bc6c25ff")
# https://coolors.co
# https://mdigi.tools/color-shades
# https://leonardocolor.io/theme.html


#' Reto Colors (v3)
#'
#' Reto colors grouped by color (gree, orange, yellow, red, blue) and hue (1-5)
#' @export
#' @examples
#' png("RCol_v3.png", width = 11.4, height = 11.4, units = "cm", res = 300)
#' par(mar = c(5,5,1,1))
#' with(RCol, plot(x = as.numeric(factor(color)), y = hue, bg = hex,
#'                 cex = 9, pch = 22, xlim = c(0.6,5.4),
#'                 ylim = c(0.6,5.4), xaxt = "n", xlab = "color"))
#' with(RCol, text(x = as.numeric(factor(color)),
#'                 y = hue-0.3, labels = hex, cex = 0.3))
#' axis(1, at = 1:5, labels = c("green", "orange", "yellow", "red", "blue"))
#' dev.off()
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
