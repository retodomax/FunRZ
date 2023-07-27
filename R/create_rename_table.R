#' ---
#' project: FunRZ    ######################################################
#' title:   create_rename_table
#' author:  Reto Zihlmann <retozihlmann@outlook.com>
#' date:    2023-06-19 14:24:22
#' output:  github_document   #############################################
#' ---




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




#' Style Excel Cells TRUE/FALSE
#'
#' @param wb A workbook object
#' @param sheet A name or index of a worksheet
#' @param cols Columns to apply conditional formatting to
#' @param rows Rows to apply conditional formatting to
#' @export
style_logical <- function(wb, sheet, cols, rows){
  mycol <- openxlsx::int2col(cols[1])
  negStyle <- openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  posStyle <- openxlsx::createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
  openxlsx::conditionalFormatting(wb = wb, sheet = sheet, cols = cols, rows = rows,
                                  rule = paste0(mycol, rows[1], '==FALSE'),
                                  style = negStyle)
  openxlsx::conditionalFormatting(wb = wb, sheet = sheet, cols = cols, rows = rows,
                                  rule = paste0(mycol, rows[1], '==TRUE'),
                                  style = posStyle)
}



#' Style Excel Cell containing Data Type
#'
#' @param wb A workbook object
#' @param sheet A name or index of a worksheet
#' @param cols Columns to apply conditional formatting to
#' @param rows Rows to apply conditional formatting to
#' @export
style_data_type <- function(wb, sheet, cols, rows){
  mycol <- openxlsx::int2col(cols[1])

  data_type_style <- data_type_formats %>%
    rowwise() %>%
    mutate(style = list(openxlsx::createStyle(bgFill = col)))
  format_function <- function(data_type, style){
    openxlsx::conditionalFormatting(wb = wb, sheet = sheet, cols = cols, rows = rows,
                                    rule = paste0(mycol, rows[1], '=="', data_type,'"'),
                                    style = style)
  }
  map2(.x = data_type_style$data_type, .y = data_type_style$style,
       .f = format_function)
  invisible()
}





#' Write sheet containing column headers
#'
#' @param column_headers column_headers tibble
#' @param tabname Name of the Tab which is added to xlsx sheet
#' @param for_rename_table Boolean, is the tab for a rename table (containing has_convention, keep, ...) or not
#' @export
write_column_headers_sheet <- function(column_headers, tabname = "column_headers",
                                       for_rename_table = FALSE) {
  colnm <- colnames(column_headers)

  ## Write to excel
  options("openxlsx.datetimeFormat" = "yyyy-mm-dd hh:mm:ss")
  options("openxlsx.dateFormat" = "yyyy-mm-dd")
  wb <- openxlsx::createWorkbook(creator = "Reto Zihlmann")
  openxlsx::addWorksheet(wb, tabname, tabColour = "lightgray")
  headerStyle <- openxlsx::createStyle(fgFill = "#D9E1F2", borderStyle = "thin",
                                       border = c("top", "bottom", "left", "right"))
  openxlsx::writeData(wb, 1, column_headers, headerStyle = headerStyle)

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
    sty <- openxlsx::createStyle(fgFill = row_col$col[i])
    openxlsx::addStyle(wb = wb, sheet = 1, style = sty,
                       rows = row_col$from[i]:row_col$to[i],
                       cols = which(colnm == "table"))
  }

  ### column: data_type
  style_data_type(wb, sheet = 1, cols = which(colnm == "data_type"),
                  rows = 2:(nrow(column_headers)+1))

  ### Some stylings are only necessary if it is for rename_table
  ### (which contains `has_convention`, `keep`, ...)
  if(for_rename_table){
    ### column: has_convention
    style_logical(wb, sheet = 1, cols = which(colnm == "has_convention"),
                  rows = 2:(nrow(column_headers)+1))
    ### column: current_data_type
    style_data_type(wb, sheet = 1, cols = which(colnm == "current_data_type"),
                    rows = 2:(nrow(column_headers)+1))
    ### column: keep
    style_logical(wb, sheet = 1, cols = which(colnm == "keep"),
                  rows = 2:(nrow(column_headers)+1))

    ## conditional colour or row if
    ## 1) second appearance of variable
    ## 2) has_convention == TRUE
    ## 3) keep == FALSE
    style_second_appearence <- openxlsx::createStyle(bgFill = "#948A54")
    style_has_convention <- openxlsx::createStyle(bgFill = "#494529")
    style_keep_false <- openxlsx::createStyle(bgFill = "#404040")
    colrange <- which(colnm == "data_type"):which(colnm == "additional_description")
    openxlsx::conditionalFormatting(wb, tabname,
                                    cols = colrange,
                                    rows=3:(nrow(column_headers)+1),
                                    rule='ISNUMBER(MATCH($E3, $E$2:$E2, 0))',
                                    ## Value in Column E needs to have a match
                                    ## in the values above
                                    style = style_second_appearence)
    colrange <- which(colnm == "data_type"):which(colnm == "axis_legend")
    openxlsx::conditionalFormatting(wb, tabname,
                                    cols = colrange,
                                    rows=2:(nrow(column_headers)+1),
                                    rule='$F2=TRUE',
                                    style = style_has_convention)
    colrange <- which(colnm == "variable"):which(colnm == "factor_levels")
    openxlsx::conditionalFormatting(wb, tabname,
                                    cols = colrange,
                                    rows=2:(nrow(column_headers)+1),
                                    rule='AND($D2=FALSE, NOT(ISBLANK($D2)))',
                                    ## Column D has to be FALSE but NOT empty
                                    style = style_keep_false)
  }

  ## Freeze top row and set column width
  openxlsx::freezePane(wb, sheet = 1, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet = 1, cols = 1:ncol(column_headers), widths = "auto")
  openxlsx::setColWidths(wb, sheet = 1, cols = which(colnm == "variable"), widths = 20)
  openxlsx::setColWidths(wb, sheet = 1, cols = which(colnm == "factor_levels"), widths = 30)
  openxlsx::setColWidths(wb, sheet = 1, cols = which(colnm == "description"), widths = 50)
  openxlsx::setColWidths(wb, sheet = 1, cols = which(colnm == "axis_legend"), widths = 30)

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
  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
}
