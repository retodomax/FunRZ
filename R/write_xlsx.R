#' Write data to an xlsx file
#'
#' Write a data frame or list of data frames to an xlsx file.
#'
#' @param x  see [openxlsx2::write_xlsx()]
#' @param file see [openxlsx2::write_xlsx()]
#' @param as_table see [openxlsx2::write_xlsx()]
#' @param col_widths see [openxlsx2::write_xlsx()]
#' @param sensitivity  sensitivity Sensitivity label, one of the following: "public", "internal" (default), "confidential", "secret"
#' @param ...  see [openxlsx2::write_xlsx()]
#'
#' @return the `wbWorkbook` object, invisibly
#' @export
#'
#' @examples write_xlsx(x = list(iris = iris, mtcars = mtcars), file = "~/Downloads/test_to_delete_write.xlsx")
write_xlsx <- function(x, file, as_table = FALSE, col_widths = "auto", sensitivity = "internal", ...) {
  oopt <- options(openxlsx2.na.strings = "")
  on.exit(options(oopt))
  wb <- openxlsx2::write_xlsx(x, as_table = FALSE, col_widths = col_widths, ...) %>%
    openxlsx2::wb_set_base_font(font_name = "Calibri") %>%
    FunRZ::wb_add_sensitivity(sensitivity = sensitivity)
  if(is.data.frame(x)){
    x <- list(x)
  }
  for (i in seq_len(length(x))) {
    dims_dat <- openxlsx2::wb_dims(rows = 1, cols = seq_len(ncol(x[[i]])))
    wb <- wb %>%
      openxlsx2::wb_add_fill(sheet = i, dims = dims_dat, color = openxlsx2::wb_color("#D9E1F2")) %>%
      openxlsx2::wb_add_border(sheet = i, dims = dims_dat, inner_vgrid = "thin") %>%
      openxlsx2::wb_freeze_pane(sheet = i, first_row = TRUE)
  }
  openxlsx2::wb_save(wb, file = file, overwrite = TRUE)
}
