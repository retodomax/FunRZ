
#' Paste error message components together with glue
abort_glue <- function(...){rlang::abort(glue::glue(...))}

#' Print tibbles within an error message
tibble_print <- function(df){paste(capture.output(print(df)), collapse = "\n")}


#' Assert data type
#'
#' data_type is a special column in the data archive pipeline. This function
#' allows to check if it contains only allowed abbreviation (and optional no NA)
#' @param x character vector to check
#' @param any.missing [\code{logical(1)}] \cr Are vectors with missing values allowed? Default is TRUE.
assert_data_type <- function(x, any.missing = TRUE){
  not_supported <- setdiff(x, FunRZ::data_type_formats$data_type)
  if(!any.missing){
    if(any(is.na(not_supported))) {rlang::abort("data_type contains NA")}
  }
  not_supported <- not_supported[!is.na(not_supported)]
  if(length(not_supported) > 0){
    abort_glue("The following data types are not supported:",
               {paste(not_supported, collapse = ", ")})
  }
}



## adding styles is done separately, because the style should only be
## added once (otherwise we get a warning),
## wherease the `style_logical()` function can potentially be
## applied multiple times within one workbook
add_styles <- function(wb){
  wb <- wb %>%
    openxlsx2::wb_add_dxfs_style(name = "negStyle",
                                 font_color = openxlsx2::wb_color(hex = "#9C0006"),
                                 bg_fill = openxlsx2::wb_color(hex = "#FFC7CE")) %>%
    openxlsx2::wb_add_dxfs_style(name = "posStyle",
                                 font_color = openxlsx2::wb_color(hex = "#006100"),
                                 bg_fill = openxlsx2::wb_color(hex = "#C6EFCE"))
  for(i in seq_along(data_type_formats$col)){
    dat_type_i <- data_type_formats$data_type[i]
    style_name_i <- paste0("data_type_", dat_type_i)
    wb <- wb %>%
      openxlsx2::wb_add_dxfs_style(name = style_name_i,
                                   bg_fill = openxlsx2::wb_color(hex = data_type_formats$col[i]))
  }
  wb
}


#' Style Excel Cells TRUE/FALSE
#'
#' @param wb A workbook object
#' @param sheet A name or index of a worksheet
#' @param cols Columns to apply conditional formatting to
#' @param rows Rows to apply conditional formatting to
#' @export
style_logical <- function(wb, sheet, cols, rows){
  mycol <- openxlsx2::int2col(cols[1])  ## TODO: like this it seems not possible to add multiple columns!!!
  dims_dat <- openxlsx2::wb_dims(rows = rows, cols = cols)
  wb %>%
    openxlsx2::wb_add_conditional_formatting(sheet = sheet, dims = dims_dat,
                                             rule = paste0(mycol, rows[1], '==FALSE'),
                                             style = "negStyle") %>%
    openxlsx2::wb_add_conditional_formatting(sheet = sheet, dims = dims_dat,
                                             rule = paste0(mycol, rows[1], '==TRUE'),
                                             style = "posStyle")
}



#' Style Excel Cell containing Data Type
#'
#' @param wb A workbook object
#' @param sheet A name or index of a worksheet
#' @param cols Columns to apply conditional formatting to
#' @param rows Rows to apply conditional formatting to
#' @export
style_data_type <- function(wb, sheet, cols, rows){
  mycol <- openxlsx2::int2col(cols[1]) ## TODO: like this it seems not possible to add multiple columns!!!
  dims_dat <- openxlsx2::wb_dims(rows = rows, cols = cols)
  for(i in seq_along(data_type_formats$col)){
    dat_type_i <- data_type_formats$data_type[i]
    style_name_i <- paste0("data_type_", dat_type_i)
    wb <- wb %>%
      openxlsx2::wb_add_conditional_formatting(sheet = 1, dims = dims_dat,
                                               rule = paste0(mycol, rows[1], '=="', dat_type_i,'"'),
                                               style = style_name_i)
  }
  wb
}




#' Write sheet containing column headers
#'
#' @param column_headers column_headers tibble
#' @param tabname Name of the Tab which is added to xlsx sheet
#' @param for_rename_table Boolean, is the tab for a rename table (containing has_convention, keep, ...) or not
#' @export
write_column_headers_sheet <- function(column_headers, tabname = "column_headers",
                                       for_rename_table = FALSE) {
  oopt <- options(openxlsx2.na.strings = "")
  on.exit(options(oopt))
  colnm <- colnames(column_headers)

  ## Write to excel
  options("openxlsx2.datetimeFormat" = "yyyy-mm-dd hh:mm:ss")
  options("openxlsx2.dateFormat" = "yyyy-mm-dd")
  wb <- openxlsx2::wb_workbook(creator = "Reto Zihlmann") %>%
    openxlsx2::wb_set_base_font(font_name = "Calibri") %>%
    openxlsx2::wb_add_worksheet(sheet = tabname, tab_color = "lightgray") %>%
    openxlsx2::wb_add_data(sheet = 1, x = column_headers) %>%
    wb_format_header(sheet = 1, dat = column_headers) %>%
    wb_add_sensitivity()

  ## Table Styling
  ### column: table
  table_col <- RColorBrewer::brewer.pal(n = 8, name = "Pastel2")
  my_tables <- column_headers$table %>% as_factor()
  row_nr <- my_tables %>% table() %>% cumsum()
  row_col <- tibble(table = levels(my_tables),
                    col = rep(table_col, length.out = length(levels(my_tables))),
                    from = c(1, row_nr[-length(row_nr)]+1) + 1,
                    to = row_nr + 1)
  for(i in seq_along(row_col$col)){
    dims_dat <- openxlsx2::wb_dims(rows = row_col$from[i]:row_col$to[i],
                                   cols = which(colnm == "table"))
    wb <- wb %>%
      openxlsx2::wb_add_fill(sheet = 1,
                             dims = dims_dat,
                             color = openxlsx2::wb_color(row_col$col[i]))
  }

  ## add styles
  wb <- wb %>% add_styles()

  ### column: data_type
  wb <- wb %>%
    style_data_type(sheet = 1, cols = which(colnm == "data_type"),
                    rows = 2:(nrow(column_headers)+1))

  ### Some stylings are only necessary if it is for rename_table
  ### (which contains `has_convention`, `keep`, ...)
  if(for_rename_table){
    ### column: has_convention
    wb <- style_logical(wb, sheet = 1, cols = which(colnm == "has_convention"),
                  rows = 2:(nrow(column_headers)+1))
    ### column: current_data_type
    wb <- style_data_type(wb, sheet = 1, cols = which(colnm == "current_data_type"),
                    rows = 2:(nrow(column_headers)+1))
    ### column: keep
    style_logical(wb, sheet = 1, cols = which(colnm == "keep"),
                  rows = 2:(nrow(column_headers)+1))

    ## conditional colour of row if
    ## 1) second appearance of variable
    ## 2) has_convention == TRUE
    ## 3) keep == FALSE
    colrange1 <- which(colnm == "data_type"):which(colnm == "additional_description")
    colrange2 <- which(colnm == "data_type"):which(colnm == "axis_legend")
    colrange3 <- which(colnm == "variable"):which(colnm == "factor_levels")
    dim1 <- openxlsx2::wb_dims(rows = 3:(nrow(column_headers)+1), cols = colrange1)
    dim2 <- openxlsx2::wb_dims(rows = 2:(nrow(column_headers)+1), cols = colrange2)
    dim3 <- openxlsx2::wb_dims(rows = 2:(nrow(column_headers)+1), cols = colrange3)
    wb <- wb %>%
      openxlsx2::wb_add_dxfs_style(name = "style_second_appearence",
                                   bg_fill = openxlsx2::wb_color(hex = "#948A54")) %>%
      openxlsx2::wb_add_dxfs_style(name = "style_has_convention",
                                   bg_fill = openxlsx2::wb_color(hex = "#494529")) %>%
      openxlsx2::wb_add_dxfs_style(name = "style_keep_false",
                                   bg_fill = openxlsx2::wb_color(hex = "#404040")) %>%
      openxlsx2::wb_add_conditional_formatting(sheet = tabname, dims = dim1,
                                               rule = 'ISNUMBER(MATCH($E3, $E$2:$E2, 0))',
                                               ## Value in Column E needs to have a match
                                               ## in the values above
                                               style = "style_second_appearence") %>%
      openxlsx2::wb_add_conditional_formatting(sheet = tabname, dims = dim2,
                                               rule = '$F2=TRUE',
                                               ## Value in Column E needs to have a match
                                               ## in the values above
                                               style = "style_has_convention") %>%
      openxlsx2::wb_add_conditional_formatting(sheet = tabname, dims = dim3,
                                               rule = 'AND($D2=FALSE, NOT(ISBLANK($D2)))',
                                               ## Value in Column E needs to have a match
                                               ## in the values above
                                               style = "style_keep_false")
  }

  ## Freeze top row and set column width
  wb <- wb %>%
    openxlsx2::wb_set_col_widths(sheet = 1, cols = 1:ncol(column_headers), widths = "auto") %>%
    openxlsx2::wb_set_col_widths(sheet = 1, cols = which(colnm == "variable"), widths = 20) %>%
    openxlsx2::wb_set_col_widths(sheet = 1, cols = which(colnm == "factor_levels"), widths = 30) %>%
    openxlsx2::wb_set_col_widths(sheet = 1, cols = which(colnm == "description"), widths = 50) %>%
    openxlsx2::wb_set_col_widths(sheet = 1, cols = which(colnm == "axis_legend"), widths = 30)

  ## Return
  wb
}


#' Create Rename Table
#'
#' @param l list containing tibbles
#' @param file path, filename and suffix of the file to create
#' @param overwrite Boolian, should an existing file be overwritten?
#' @import lubridate
#' @import forcats
#' @import stringr
#' @import dplyr
#' @import purrr
#' @import readr
#' @import tidyr
#' @import tibble
#' @import ggplot2
#' @export
create_rename_table <- function(l, file = "column_headers.xlsx", overwrite = FALSE) {
  column_headers <- tibble(table = rep(names(l), times = lengths(l)),
                           orig_name = map(l, names) %>% unlist(use.names = FALSE),
                           suggested_name = janitor::make_clean_names(orig_name, allow_dupes = TRUE),
                           keep = TRUE,
                           variable = NA_character_,
                           has_convention = FALSE,
                           current_data_type = map(l, function(x) map_chr(x, vctrs::vec_ptype_abbr)) %>%
                             unlist(use.names = FALSE),
                           data_type = NA_character_,
                           unit = NA_character_,
                           description = NA_character_,
                           axis_legend = NA_character_,
                           factor_levels = NA_character_,
                           additional_description = NA_character_)

  if(file.exists(file) & !overwrite){
    abort_glue("The file already exists. Set `overwrite = TRUE`",
               "if you want to overwrite it")
  }
  if(file.exists(file)){
    ### update column_headers based on existing `column_headers.xlsx`

    ## Here it might be a bad idea to use read_excel_tmp() because
    ## we later want to overwrite this file, so it's better if it is closed...
    ## We wrap the call in `tryCatch` to return a better error message
    ## in case the file is still open.
    prev_version <- tryCatch(
      expr = readxl::read_excel(path = file),
      error = function(err){
        msg <- conditionMessage(err)
        if (grepl("zip file '.*' cannot be opened", msg)) {
          new_msg <- gsub("zip file ('.*') cannot be opened",
                          paste("Column header file cannot be overwritten",
                                "because it is opened\n",
                                "First close the file \\1"),
                          msg)
          rlang::abort(new_msg)
        } else {stop(err)}
      })
    ## here quickly test if some `table` and `orig_name` combination in the
    ## previous column_headers table is no longer there in the new `column_headers`
    ## object. If this is the case, save a copy (with "_archive_" and date-time in name)
    ## such that not unintentionally some descriptions are lost...
    rows_which_would_be_deleted <- prev_version %>%
      select(table, orig_name) %>%
      anti_join(column_headers, by = join_by(table, orig_name)) %>% nrow()
    if(rows_which_would_be_deleted > 0){
      file.copy(from = file,
                to = paste0(str_sub(file, end=-6), "_archive_",
                            format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"))
    }

    column_headers <- column_headers %>%
      rows_update(prev_version %>% select(-c(suggested_name, current_data_type)),
                  by = c("table", "orig_name"), unmatched = "ignore")
    ## `suggested_name` and `current_data_type` needs to be updated
    ## (and not taken from `prev_version`) => they are removed from `prev_version`
  }

  ## write column_headers
  wb <- write_column_headers_sheet(column_headers = column_headers,
                                   for_rename_table = TRUE)

  ## Save
  openxlsx2::wb_save(wb = wb, file = file, overwrite = TRUE)
}
