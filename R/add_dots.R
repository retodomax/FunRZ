#' Add Dots (...) below display Table
#'
#' This function has become a bit too complex, maybe it's easier to just say 'we print the first few rows of the dataset'
#'
#' @param input_table output from `kableExtra::kbl()` or `gt::gt()` or `knitr::kable()`
#'
#' @examples
#' iris %>%
#'  head() %>%
#'  kableExtra::kbl() %>%
#'  add_dots()
#' @return Object of class kableExtra or output is directly printed as html?
#' @export
add_dots <- function(input_table){

  myclass <- class(input_table)[1]
  ## Test if input class is correct
  if(!(myclass %in% c("gt_tbl", "kableExtra", "knitr_kable"))){
    stop("Input has to be of class gt_tbl, kableExtra or knitr_kable")
  }
  ## Test if we are in a Quarto document

  if(myclass %in% "gt_tbl") {

    ## Old Way: did work interactively but not in Quarto Documents (because raw html code will be printed)
    # ## 1) call initial "print()" lines
    # html_tbl <- gt:::as.tags.gt_tbl(input_table)
    #
    # ## 2) Transform output
    #
    # ### a) extract
    # to_modif <- html_tbl$children[[2]] %>% as.character()
    #
    # ### b) modify
    # ## extract html code of last table row
    # last_row <- stringi::stri_extract_last(str = to_modif,
    #                                        regex = "  <tr>(.|\n)*?</tr>\n")
    # ## replace all content with empty string (content is located between '\">' and '</td>\n')
    # last_row <- str_replace_all(last_row, '(?<=\\">).*(?=</td>)', '')
    # ## replace empty string of first column with `...`
    # last_row <- str_replace(last_row, '(?<=\\">)(?=</td>\n)', ' ... ')
    # ## add last row below previous table
    # modified_html <- str_replace(to_modif,
    #                              pattern = "(?<=</tr>\n)(?=  </tbody>)",
    #                              replacement = last_row)
    #
    # ### c) recreate html_tbl
    # html_tbl$children[[2]] <- htmltools::HTML(modified_html)
    #
    # ## 3) Run final "print()" lines
    # invisible(print(html_tbl, browse = interactive()))

    ## New approach: directly modify gt_tbl object
    ## 1) add last row to data
    mydat <- input_table$`_data`
    mydat <- mydat %>%
      mutate(across(1, ~ as.character(.x))) %>%
      bind_rows(mydat[1,] %>% mutate(across(everything(), ~ NA),
                                     across(1, ~ "...")))
    input_table$`_data` <- mydat

    ## 2) add last row to stub
    mystub <- input_table$`_stub_df`
    mystub <- mystub %>%
      bind_rows(mystub[nrow(mystub),]) %>% ## replicate last row
      mutate(rownum_i = row_number())
    input_table$`_stub_df` <- mystub

    ## 3) output
    input_table %>%
      sub_missing(missing_text = "")



  } else if(myclass %in% "kableExtra") {

    tbl_class <- class(input_table)
    tbl_att <- attributes(input_table)
    ## extract html code of last table row
    last_row <- stringi::stri_extract_last(str = input_table,
                                           regex = "  <tr>(.|\n)*?</tr>\n")
    ## replace all content with empty string (content is located between ';:>' and '</td>\n')
    last_row <- str_replace_all(last_row, '(?<=;">) .*(?=</td>\n)', '')
    ## replace empty string of first column with `...`
    last_row <- str_replace(last_row, '(?<=;">)(?=</td>\n)', ' ... ')
    ## add last row below previous table
    new_kable_table <- str_replace(input_table,
                                   pattern = "(?<=</tr>\n)(?=</tbody>)",
                                   replacement = last_row)

    class(new_kable_table) <- tbl_class
    attributes(new_kable_table) <- tbl_att
    new_kable_table

  } else if(myclass %in% "knitr_kable"){

    stop("Not yet implemented for knitr::kable() output")
    ## Quite difficult...
    ## knitr::kable() has argument format which is automatically determined in knitr documents
    ## can be latex, html or something else

  }
}
