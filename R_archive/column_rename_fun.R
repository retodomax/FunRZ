#' Check arguments of the data pipeline: l, convention_table and column_headers
#'
#' @param l list containing all tibbles
#' @param convention_table tibble with conventional variables
#' @param column_headers tibble with column header descriptions
#' @param check_l_ch_consistency [\code{logical(1)}] \cr If consistency between l and column_headers should be checked
#' @export
check_data_pipeline_arguments <- function(l, convention_table, column_headers, check_l_ch_consistency = TRUE) {
  ## Check l ##################################
  assert_list(l, names = "unique") # l is list with unique names
  walk(l, assert_tibble) # all elements of l are tibbles

  ## Check convention_table ###################
  assert_data_type(convention_table$data_type, any.missing = FALSE)
  assert_character(convention_table$variable, unique = TRUE)

  ## Check column_headers #####################
  assert_logical(column_headers$keep, any.missing = FALSE)

  ## only check rows where `keep == TRUE`
  ch_keep <- column_headers %>% filter(keep)
  ## check if duplicated variables have all description set to NA
  dups_without_na <- ch_keep %>%
    filter(duplicated(variable)) %>%
    filter(if_any(c(data_type, unit, description, axis_legend, factor_levels,
                    additional_description), ~ !is.na(.x))) %>%
    select(table, variable, data_type, unit, description, axis_legend,
           factor_levels, additional_description)
  if(nrow(dups_without_na) > 0){
    abort_glue("The following variables in column_headers have a second entry",
               " with non-NA definition:\n\n", tibble_print(dups_without_na))
  }
  ## fill missing rows for duplicated variables
  ch_keep <- ch_keep %>%
    group_by(variable) %>%
    fill(has_convention, data_type, unit, description, axis_legend,
         factor_levels, additional_description, .direction = "downup") %>%
    ungroup()
  ## Check filled df
  assert_data_type(ch_keep$current_data_type)
  assert_data_type(ch_keep$data_type)
  assert_character(ch_keep$variable, any.missing = FALSE)
  assert_logical(ch_keep$has_convention, any.missing = FALSE)
  ch_keep %>% filter(!has_convention) %>% pull(data_type) %>%
    assert_data_type(., any.missing = FALSE)
  ## Checks in combo with convention_table
  var <- ch_keep %>% filter(has_convention) %>% pull(variable)
  diff_var <- setdiff(var, convention_table$variable)
  if(length(diff_var) > 0){
    abort_glue("The following variables have `convention_table == TRUE` but were",
               " not found in convention table:\n", paste(diff_var, collapse = ", "))
  }
  var <- ch_keep %>% filter(!has_convention) %>% pull(variable)
  diff_var <- intersect(var, convention_table$variable)
  if(length(diff_var) > 0){
    abort_glue("The following variables have `convention_table == FALSE` but have",
               " a variable with same name in convention table:\n",
               paste(diff_var, collapse = ", "))
  }
  ## Checks in combo with l
  ## This is not necessary if column_headers is already outdated compared to l
  ## (which is the case for `create_excel_archive()`)
  if(check_l_ch_consistency){
    ## check tables
    table_diff <- union(setdiff(names(l), column_headers$table), setdiff(column_headers$table, names(l)))
    if(length(table_diff) > 0){
      rlang::abort("Tables defined in column_headers table and in l are not the same.\n",
                   "Update column_headers table!")}
    ## check variables within tables
    ch_extracted <- tibble(table = rep(names(l), times = lengths(l)),
                           orig_name = map(l, names) %>% unlist(use.names = FALSE))
    ch_l_diff <- column_headers %>%
      select(table, orig_name) %>%
      symdiff(ch_extracted)
    if(nrow(ch_l_diff) > 0){
      abort_glue("l and the column_header table are not consistent and differ by",
                 " the following variables:\n\n",
                 tibble_print(ch_l_diff), "\n\n Update column_headers table!")
    }
  }
}







#' Column Rename Function
#'
#' @param l  list containing tibbles
#' @param column_headers_path path to file created by \code{\link{create_rename_table}}
#' @param convention_table_path path to file containing conventions (e.g. \code{C:/Users/858782/DSM/BovaerAnimalTrials - General/01_Admin/Animal_Trial_Column_Names_RZ.xlsx})
#' @param verbose_factor_levels Boolian, should automatically determined factor levels be printed to console?
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
column_rename_fun <- function(l, column_headers_path, convention_table_path = NULL, verbose_factor_levels = TRUE){
  # C:/Users/858782/DSM/BovaerAnimalTrials - General/01_Admin/Animal_Trial_Column_Names_RZ.xlsx

  ## Import data ##############################
  col_types <- c(rep("text", 3), "logical", "text", "logical", rep("text", 7))
  column_headers <- read_excel_tmp(path = column_headers_path, col_types = col_types)
  ## To Do: build in option if there is no convention_table for a specific dataset
  col_types <- c("numeric", rep("text", 7))
  convention_table <- read_excel_tmp(path = convention_table_path, col_types = col_types)

  ## Inconsistency checks #####################
  ## l and column_headers must be consistent (same tables defined)
  ## note that the order of arguments in setdiff is important...
  ## https://stackoverflow.com/a/1844702/6152316
  table_diff <- union(setdiff(names(l), column_headers$table), setdiff(column_headers$table, names(l)))
  if(length(table_diff) > 0){
    rlang::abort("Tables defined in column_headers table and in l are not the same.\n",
                 "Update column_headers table!")}
  ## the check if all variables are the same within one table is done
  ## in the "Select, rename, change data_type" chapter.

  ## Is any variable set as `keep = TRUE` but has no defined variable name
  undefined <- column_headers %>%
    filter(keep & is.na(variable)) %>%
    select(table, orig_name, suggested_name, keep, variable)
  if(nrow(undefined) > 0){
    abort_glue("The following variables have no variable name ",
               "defined in the `column_headers.xlsx` file:\n",
               tibble_print(undefined))
  }
  ## Is any variable defined in two inconsistent ways?

  #### Now check if inconsistent definition
  dups <- column_headers %>%
    filter(keep) %>%
    select(variable, has_convention, data_type, unit, description, axis_legend) %>%
    group_by(variable) %>%
    distinct()
  inconsistent_variables <- dups %>%
    summarise(n = n()) %>%
    filter(n > 1) %>%
    pull(variable)
  dups <- dups %>%
    filter(variable %in% inconsistent_variables) %>%
    arrange(variable) %>%
    ungroup()
  n_var_inconsistent <- length(inconsistent_variables)
  if(n_var_inconsistent > 0){
    singular_plural <- ifelse(n_var_inconsistent > 1, "variables are", "variable is")
    abort_glue("The following ", singular_plural,
               " used multiple times with inconsistent definition:\n",
               {paste(inconsistent_variables,
                      collapse = ", ")}, "\n\n",
               "The following table shows how they are defined:\n",
               tibble_print(dups))
  }
  # Has a variable additional definition even if it has a convention definition?
  double_def <- column_headers %>%
    filter(has_convention) %>%
    ## here the check for factor levels is currently excluded
    ## i.e. factor levels can be different then what was used in the convention_table
    filter(if_any(c(data_type:axis_legend), ~ !is.na(.x)))
  double_def_var <- double_def %>% pull(variable)
  n_var_double <- length(double_def_var)
  if(n_var_double > 0){
    singular_plural <- ifelse(n_var_double > 1, "variables are", "variable is")
    abort_glue("The following ", singular_plural,
               " have an additional definition even if they have a convention:\n",
               {paste(double_def_var,
                      collapse = ", ")}, "\n\n",
               "The following table shows how they are defined:\n",
               tibble_print(double_def))
  }
  ## Has any variable `has_convention == TRUE` but the variable name is not included
  ## in convention table?
  unmatched_convention_var <- column_headers %>%
    filter(has_convention) %>%
    pull(variable) %>%
    setdiff(convention_table$variable)
  n_unmatched <- length(unmatched_convention_var)
  if(n_unmatched > 0){
    singular_plural <- ifelse(n_unmatched > 1, "variables are", "variable is")
    abort_glue("The following ", singular_plural,
               " not defined in the convention table:\n",
               {paste(unmatched_convention_var,
                      collapse = ", ")})
  }
  ## if `convention_table = FALSE` and `keep = TRUE` then `variable` and
  ## `data_type` should not be NA
  not_sufficiently_specified <- column_headers %>%
    filter(keep & !has_convention) %>%
    filter(is.na(variable) | is.na(data_type)) %>%
    select(table, orig_name, keep, variable, has_convention, data_type)
  n_not_specified <- nrow(not_sufficiently_specified)
  if(n_not_specified > 0){
    singular_plural <- ifelse(n_not_specified > 1, "variables are", "variable is")
    abort_glue("The following ", singular_plural,
               " not sufficiently specified. `data_type` and `variable`",
               " should not be NA if `convention_table` = FALSE:\n",
               tibble_print(not_sufficiently_specified))
  }

  ## data_type should be of known set
  bad_data_type <- column_headers %>%
    filter(!is.na(data_type) & keep) %>%
    filter(!(data_type %in% data_type_formats$data_type)) %>%
    select(table, orig_name, keep, variable, has_convention, data_type)
  n_unknown_data_types <- nrow(bad_data_type)
  if(n_unknown_data_types > 0){
    singular_plural <- ifelse(n_unknown_data_types > 1, "variables are", "variable is")
    abort_glue("The following ", singular_plural,
               " defined with an unknown data type:\n",
               tibble_print(bad_data_type),
               "\n\nAllowed data types are: ",
               {paste(data_type_formats$data_type, collapse = ", ")})
  }

  ## Patch duplicated values ##################
  column_headers <- column_headers %>%
    group_by(variable) %>%
    fill(has_convention, data_type, unit, description, axis_legend,
         factor_levels, additional_description, .direction = "downup") %>%
    ungroup()


  ## Join with convention table ###############
  column_headers <- column_headers %>%
    rows_patch(convention_table %>%
                 select(variable, data_type, unit, description, axis_legend, factor_levels) %>%
                 mutate(has_convention = TRUE),
               by = c("variable", "has_convention"), unmatched = "ignore") %>%
    left_join(data_type_formats %>% select(data_type, readr_abbr), by = "data_type")



  ## Select, rename, change data_type #########
  tables <- names(l)
  for(i in seq_along(tables)){
    tables_i <- tables[i]
    l_i <- l[[tables_i]]
    ch_i <- column_headers %>% filter(table == tables_i)
    ###### This check is now implemented in `check_data_pipeline_arguments()`
    # ## Check if all variables are there
    # if(!all(ch_i$orig_name == names(l_i))){rlang::abort("Variables in table ", l_i,
    #                                                     " and column_headers table ",
    #                                                     "are not the same. Update ",
    #                                                     "column_headers table!")}
    ## remove unnecessary variables and rename
    ch_i_keep <- ch_i %>% filter(keep)
    var_rename <- var_to_keep <- ch_i_keep %>% pull(orig_name)
    names(var_rename) <- ch_i_keep %>% pull(variable)
    l_i <- l_i %>%
      select(all_of(var_to_keep)) %>%
      rename(all_of(var_rename))
    ## change data_type
    abbr_type_convert <- ch_i_keep$readr_abbr %>% str_replace("f", "c") %>% paste(collapse = "")
    l_i <- l_i %>%
      mutate(across(everything(), as.character)) %>% ## type_convert is only applied to character columns
      type_convert(col_types = abbr_type_convert)
    l[[i]] <- l_i
  }

  ## factor levels (do this after rename to make sure to pool same variables with different names)
  extract_unique_levels <- function(my_var){
    l %>%
      map(.f = function(x) select(x, any_of(my_var))) %>%
      unlist() %>% unique() %>%
      str_sort(numeric = TRUE)
    # str_sort() detects numbers and sorts by number instead of alphabetical
    # e.g. c("a10", "a1", "a5") becomes c("a1", "a5", "a10") instead of c("a1", "a10", "a5")
  }
  ch_fac <- column_headers %>%
    select(table, variable, keep, data_type, factor_levels) %>%
    filter(keep, data_type %in% c("fct", "ord")) %>%
    rowwise() %>%
    mutate(factor_levels_extr = list(extract_unique_levels(variable))) %>%
    mutate(factor_levels_def = ifelse(!is.na(factor_levels), strsplit(factor_levels, ", "), NA)) %>%
    mutate(factor_levels_use = ifelse(!is.na(factor_levels), list(factor_levels_def), list(factor_levels_extr)))
  levels_to_print <- ch_fac %>%
    filter(is.na(factor_levels)) %>%
    select(variable, factor_levels_extr) %>%
    distinct() %>%
    mutate(factor_levels_extr = paste(factor_levels_extr, collapse = ", ")) %>%
    rename(`factor level order (extracted and sorted)` = factor_levels_extr) %>%
    ungroup()
  if(verbose_factor_levels){
    rlang::inform(glue::glue("The following variables have no factor levels predefined.\n",
                             "By default, their levels were set in the following order\n",
                             tibble_print(levels_to_print)))
  }
  for(i in seq_along(tables)){
    tables_i <- tables[i]
    l_i <- l[[tables_i]]
    ch_i <- column_headers %>% filter(table == tables_i)
    ch_fac_i <- ch_fac %>% filter(table == tables_i)
    for(j in seq_along(ch_fac_i$variable)){
      var_j <- ch_fac_i$variable[j]
      l_i[[var_j]] <- factor(l_i[[var_j]], levels = ch_fac_i$factor_levels_use[[j]],
                             ordered = ch_fac_i$data_type[j] == "ord")
    }
    l[[i]] <- l_i
  }
  l
}
