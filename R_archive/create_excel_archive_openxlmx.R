#' Create Excel Archive
#'
#' @param l list containing tibbles
#' @param filename path and filename (without suffix) of the file to create
#' @param convention_table_path path to file containing conventions (e.g. \code{C:/Users/858782/DSM/BovaerAnimalTrials - General/01_Admin/Animal_Trial_Column_Names_RZ.xlsx})
#' @param column_headers_path path to file created by \code{\link{create_rename_table}}
#' @param ERD Boolean, should Entity-Relationship-Diagram be drawn and added as tab?
#' @param pk list defining the primary keys per table as character string
#' @param connectors character vector defining the connections between tables
#' @param erd_height height of the ERD plot in pixels (manually adjust if necessary)
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
create_excel_archive <- function(l, filename = "data_archive",
                                 convention_table_path = NULL,
                                 column_headers_path = NULL,
                                 ERD = TRUE,
                                 pk = NULL,
                                 connectors = NULL,
                                 erd_height = 800){
  # convention table: "C:/Users/858782/DSM/BovaerAnimalTrial - General/01_Admin/Animal_Trial_Column_Names_RZ.xlsx"

  ## Import data ##############################
  column_headers <- NULL
  convention_table <- NULL
  if(!is.null(column_headers_path)){
    col_types <- c(rep("text", 3), "logical", "text", "logical", rep("text", 7))
    column_headers <- read_excel_tmp(path = column_headers_path, col_types = col_types)
  }
  if(!is.null(convention_table_path)){
    col_types <- c("numeric", rep("text", 7))
    convention_table <- read_excel_tmp(path = convention_table_path, col_types = col_types)
  }

  ## Checks ###################################
  check_data_pipeline_arguments(l = l, convention_table = convention_table,
                                column_headers = column_headers,
                                check_l_ch_consistency = FALSE)

  ## Create readme table ######################
  readme <- tibble(table = rep(names(l), times = lengths(l)),
                   variable = map(l, names) %>% unlist(use.names = FALSE),
                   # https://tibble.tidyverse.org/articles/types.html
                   data_type = map(l, function(x) map_chr(x, vctrs::vec_ptype_abbr)) %>%
                     unlist(use.names = FALSE),
                   unit = NA_character_,
                   description = NA_character_,
                   factor_levels = map(l, function(x) map(x, levels)) %>%
                     flatten() %>%
                     map(function(x) paste(x, collapse = ", ")) %>%
                     unlist(use.names = FALSE),
                   axis_legend = NA_character_,
                   orig_name = NA_character_)

  ## 2) check for inconsistencies (variables from different tables with same name
  ##    but different data type or levels/order of levels)
  inconsistent_variables <- readme %>%
    distinct(variable, data_type, unit, description,
             factor_levels, axis_legend, orig_name, .keep_all = TRUE) %>%
    group_by(variable) %>%
    summarize(n = n()) %>%
    filter(n > 1) %>%
    pull(variable)
  inconsistent_variables_tibble <- readme %>%
    filter(variable %in% inconsistent_variables) %>% arrange(variable)
  n_var_inconsistent <- length(inconsistent_variables)
  if(n_var_inconsistent > 0){
    singular_plural <- ifelse(n_var_inconsistent > 1, "variables are", "variable is")
    abort_glue("The following ", singular_plural,
               " used multiple times with inconsistent definition:\n",
               {paste(inconsistent_variables,
                      collapse = ", ")}, "\n\n",
               "The following table shows how it should be defined:\n",
               tibble_print(inconsistent_variables_tibble))
  }

  ## 3) Join with convention table (Animal_Trial_Column_Names_RZ.xlsx)
  ## and give warning if data_type does not fit with convention
  ## - match by variable
  ## - data_type should be identical (or missing in convention_table)
  ## - unit, description, axis_legend, orig_name should be patched (NA replaced)
  ## factor levels AND order do not necessarily have to match with conventions...
  ## and can be overruled
  # "C:/Users/858782/DSM/BovaerAnimalTrials - General/01_Admin/Animal_Trial_Column_Names_RZ.xlsx"
  if(!is.null(convention_table_path)){
    unconventional_variables_tibble <- convention_table %>%
      select(variable, data_type) %>%
      ## first patch missing factor levels of convention table (if it's missing,
      ## then there is no convention and they can be removed with anti_join)
      rows_patch(readme %>%
                   select(variable, data_type) %>%
                   distinct(),
                 by = "variable", unmatched = "ignore") %>%
      filter(variable %in% readme$variable) %>%
      anti_join(readme, by = join_by(variable, data_type))
    unconventional_variables <- unconventional_variables_tibble %>% pull(variable)
    if(length(unconventional_variables) > 0){
      singular_plural <- ifelse(length(unconventional_variables) > 1,
                                "variables are", "variable is")
      abort_glue("The following ", singular_plural,
                 " inconsistent with the convention:\n",
                 {paste(unconventional_variables,
                        collapse = ", ")}, "\n\n",
                 "The following table shows how they should be defined:\n",
                 tibble_print(unconventional_variables_tibble))
    }
    readme <- readme %>%
      rows_patch(convention_table %>% select(variable, unit, description, axis_legend),
                 by = "variable", unmatched = "ignore")
  }

  ## 4) Add unit, description, axis_legend, orig_name from `column_headers.xlsx`
  ##    For description make sure to also paste additional_description to it
  if(!is.null(column_headers_path)){
    column_headers_unique <- column_headers %>%
      filter(keep) %>%
      filter(!duplicated(variable)) %>%
      select(variable, unit, description, axis_legend, additional_description)
    paste2 <- function(x, sep = "") {paste(x[!is.na(x)], collapse = sep)}
    readme <- readme %>%
      mutate(additional_description = NA_character_) %>%
      rows_patch(column_headers_unique,
                 by = c("variable"), unmatched = "ignore") %>%
      mutate(additional_description = if_else(!is.na(additional_description),
                                              paste0("; Additional: ", additional_description),
                                              "")) %>%
      rowwise() %>%
      mutate(description = paste2(c(description, additional_description)),
             description = if_else(description == "", NA, description)) %>%
      ungroup() %>%
      select(-additional_description) %>%
      rows_patch(column_headers %>% filter(keep) %>% select(table, variable, orig_name),
                 by = c("table", "variable"), unmatched = "ignore")
  }

  ## Write Excel ##############################
  options("openxlsx.datetimeFormat" = "yyyy-mm-dd hh:mm:ss")
  options("openxlsx.dateFormat" = "yyyy-mm-dd")

  ## README tab
  wb <- write_column_headers_sheet(column_headers = readme, tabname = "README")

  ## ERD tab
  if(ERD){
    if(is.null(pk)){
      pk <- tibble(table = character(), variable = character(), key = character())
    }
    pk <- tibble(table = rep(names(pk), times = lengths(pk)),
                 variable = unname(unlist(pk)),
                 key = "PK")
    if(!is.null(connectors)){
      connectors <- paste(connectors, ': ""')
    }
    paste2 <- function(x) {paste(x[!is.na(x)], collapse = " ")}
    erd_dat <- readme %>%
      left_join(pk, by = join_by(table, variable)) %>%
      select(table, variable, data_type, key) %>%
      rowwise() %>%
      mutate(string = paste2(c(variable, data_type, key))) %>%
      group_by(table) %>%
      summarise(s = paste0("\t", paste(string, collapse = "\n\t"))) %>%
      mutate(s = paste0(table, " {\n", s, "\n}"))
    erd_out <- paste0("erDiagram\n%% Tables\n",
                      paste(erd_dat$s, collapse = "\n"),
                      "\n%% Connections\n",
                      paste(connectors, collapse = '\n'))
    destfile <- tempfile(fileext = ".png")

    DiagrammeR::mermaid(erd_out, height = erd_height) %>%
      htmltools::html_print(viewer = NULL) %>%
      gsub("\\\\", "/", .) %>%
      webshot2::webshot(file = destfile, zoom = 2,
                        selector = c("rect", "path", "g"),
                        expand = 15, quiet = TRUE)
    ## `zoom` will make resolution better
    ## `selector` which css selectors define the area of interest (to include in the webshot)
    ## `expand` adds margin arround image
    ## `quiet` makes sure that there is no message printed

    ## If there are issues with DiagrammR after a new version of R, make sure to
    ## attach a newer version mermaid to DiagrammR:
    ## https://github.com/rich-iannone/DiagrammeR/issues/457#issuecomment-1109995343

    openxlsx::addWorksheet(wb = wb, sheetName = "ERD", tabColour = "lightgray")
    ## defalut dimensions of DiagrammeR::mermaid() is
    ##  - 975 x 516
    ##  - 1950 x 1032                   (with zoom = 2)
    ##  - 1950 x (erd_height + 16)*2    (with manually setting height)
    openxlsx::insertImage(wb = wb, sheet = "ERD", file = destfile,
                          width = 1950/130, height = (erd_height + 16)*2 /130)
  }

  ## All remaining tabs
  tab_col <- RColorBrewer::brewer.pal(n = 8, name = "Pastel2")
  tab_col <- rep(tab_col, length.out = length(l))
  for(i in seq_along(l)){
    sheet_nr <- i + 1 + as.numeric(ERD)
    openxlsx::addWorksheet(wb = wb, sheetName = names(l)[i], tabColour = tab_col[i])
    # openxlsx::writeData(wb, sheet_nr, l[[i]], headerStyle = headerStyle)
    openxlsx::writeDataTable(wb = wb, sheet = sheet_nr, x = l[[i]],
                             tableStyle = "TableStyleLight1", withFilter = FALSE)
    openxlsx::freezePane(wb, sheet = sheet_nr, firstRow = TRUE)
    openxlsx::setColWidths(wb, sheet = sheet_nr, cols = 1:lengths(l)[i], widths = "auto")
  }

  ## Save
  filename <- paste0(filename, "_",
                     format(Sys.time(), format = "%Y%m%d"), ".xlsx")
  openxlsx::saveWorkbook(wb, file = filename, overwrite = TRUE)
  invisible(readme)
}

## To Do:
# * Allow to manually edit archive.xlsx file (README part) and paste changes later to newer version!!
#    - VERY IMPORTANT feature
# * Allow to provide several column_header files to create_excel_archive()
#    - e.g. in Reading (calf, beef) we had two column_headers tables (README part)
#      but only one can be provided...
#      Second part: (would be nice it AT LEAST
#      some of the descriptions can be manually filled and are not lost
#      each time the archive is newly generated...)
#       => this is the first bullet point in ToDo
# * Variables which are of type "Date" have in Excel a custom number format which is (yyyy-mm-dd hh:mm:ss)
#   However for dates this would be better to be yyyy-mm-dd
#   Strangely this is not automatically done (even if there is line 145: `options("openxlsx.dateFormat" = "yyyy-mm-dd")`)
# * Check if it is possible to set "Confidential" label automatically (e.g. check XML source of excel set to "Confidencial")
# * Make better documentation
# * Make tests (based on R_stat_varia::test_data_pipeline.R
# * Add CDISC variable names to convention_names (based on Sudhars document)
# * Function which allows to read from excel_archive.xlsx
# * Make it possible to return ERD markup code (maybe separate function, which is called within `create_excel_archive`?)
# * Make it possible to return ERD as png
# * Maybe: Ways to improve the ordering of tables in ERD
#    - E.g. take Reading Calf/Beef
#    - It would be nice if the two gas tables are next to each other
#    - this can be achieved by reordering `erd_dat` such that the tables do not
#    - occure in alphabetical order...
#    - Easiest to find out if you extract the ERD code
# * Add Code for dbdiagram.io
