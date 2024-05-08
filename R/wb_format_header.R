#' Format Header of Excel Table
#'
#' @param wb A `wbWorkbook` object
#' @param dat data which were included in Excel sheet, used to extract dimensions of header
#' @param sheet A name or index of a worksheet
#'
#' @return
#' @export
#'
#' @examples wb_workbook(creator = "Reto Zihlmann") %>%
#'   wb_add_worksheet(sheet = "README", tab_color = "green") %>%
#'   wb_add_data(sheet = 1, x = dat, with_filter = FALSE) %>%
#'   wb_set_col_widths(sheet = 1, cols = 1:ncol(dat), widths = "auto") %>%
#'   wb_format_header(dat = dat)
wb_format_header <- function(wb, sheet = openxlsx2::current_sheet(), dat) {
  dims_dat <- openxlsx2::wb_dims(rows = 1, cols = seq_len(ncol(dat)))
  wb %>%
    openxlsx2::wb_add_fill(sheet = sheet, dims = dims_dat, color = openxlsx2::wb_color("#D9E1F2")) %>%
    openxlsx2::wb_add_border(sheet = sheet, dims = dims_dat, inner_vgrid = "thin") %>%
    openxlsx2::wb_freeze_pane(sheet = sheet, first_row = TRUE)
}
