#' Add Dots (...) below kable Table
#'
#' @param kable_table output from `kableExtra::kbl()`
#'
#' @examples
#' iris %>%
#'  head() %>%
#'  kbl() %>%
#'  add_dots()
#' @return Object of class kableExtra
#' @export
add_dots <- function(kable_table){
  tbl_class <- class(kable_table)
  tbl_att <- attributes(kable_table)
  ## extract html code of last table row
  last_row <- stringi::stri_extract_last(str = kable_table,
                                         regex = "  <tr>(.|\n)*?</tr>\n")
  ## replace all content with empty string (content is located between ';:>' and '</td>\n')
  last_row <- str_replace_all(last_row, '(?<=;">) .*(?=</td>\n)', '')
  ## replace empty string of first column with `...`
  last_row <- str_replace(last_row, '(?<=;">)(?=</td>\n)', ' ... ')
  ## add last row below previous table
  new_kable_table <- str_replace(kable_table,
                                 pattern = "(?<=</tr>\n)(?=</tbody>)",
                                 replacement = last_row)

  class(new_kable_table) <- tbl_class
  attributes(new_kable_table) <- tbl_att
  new_kable_table
}
