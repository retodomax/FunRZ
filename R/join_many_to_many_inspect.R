#' Inspect which rows cause a many-to-many warning
#'
#' @param x see [dplyr::inner_join()]
#' @param y see [dplyr::inner_join()]
#' @param by see [dplyr::inner_join()]
#' @param show_rows character, show the rows which cause the problem in table `x` or `y`. If `none` it returns a counting table how often a certain joining-key is observed in `x` and `y`.
#'
#' @return tbl with number of multiple observed joining keys, or rows which cause the problem in table `x` or `y`
#' @export
#'
#' @examples
#' x <- tibble(key = c(1, 1:3),
#' value = letters[key])
#' y <- tibble(key = c(1,1,1,1,2),
#'             value2 = 1:5)
#' x %>% inner_join(y)
#' join_many_to_many_inspect(x = x, y = y, show_rows = "none")
#' join_many_to_many_inspect(x = x, y = y, show_rows = "x")
join_many_to_many_inspect <- function(x, y, by = NULL, show_rows = c("none", "x", "y")) {
  assertCharacter(show_rows[1], pattern = "(^none$)|^x$|^y$")
  ## 1) Find key columns by which we join
  x_names <- tbl_vars(x)
  y_names <- tbl_vars(y)
  if (is_null(by)) {
    by2 <- dplyr:::join_by_common(x_names, y_names, error_call = rlang::caller_env())
  } else {
    by2 <- dplyr:::as_join_by(by, error_call = rlang::caller_env())
  }
  ## 2) Count number of rows per key
  xn <- x %>%
    select(by2$x) %>%
    group_by(pick(everything())) %>%
    summarise(n_x = n(), .groups = "drop")
  yn <- y %>%
    select(by2$y) %>%
    group_by(pick(everything())) %>%
    summarise(n_y = n(), .groups = "drop")
  ## 3) Output number of rows
  n_xy <- xn %>%
    full_join(yn, by = by2)
  if(show_rows[1] == "none"){
    n_xy
  } else if(show_rows[1] == "x") {
    n_xy %>%
      filter(n_x > 1) %>%
      select(-c(n_x, n_y)) %>%
      left_join(x)
  } else if(show_rows[1] == "y") {
    n_xy %>%
      filter(n_y > 1) %>%
      select(-c(n_x, n_y)) %>%
      left_join(y, by = by2) %>%
      rename_with(\(k) by2$y, .cols = all_of(by2$x))
  }
}


# # Testing -----------------------------------------------------------------
#
# ## Ex 1 #####################################
# x <- tibble(key = c(1, 1:3),
#             value = letters[key])
# y <- tibble(key = c(1,1,1,1,2),
#             value2 = 1:5)
# x %>% inner_join(y)
#
# join_many_to_many_inspect(x = x, y = y, show_rows = "none")
# join_many_to_many_inspect(x = x, y = y, show_rows = "x")
# join_many_to_many_inspect(x = x, y = y, show_rows = "y")
#
# ## Ex 2 #####################################
# x2 <- tibble(primary_key = c(1, 1:3),
#              value = letters[primary_key])
# y2 <- tibble(foreign_key = c(1,1,1,1,2),
#              value2 = 1:5)
# by2 <- join_by(primary_key == foreign_key)
# x2 %>% inner_join(y2, by = by2)
#
# join_many_to_many_inspect(x = x2, y = y2, by = by2, show_rows = "none")
# join_many_to_many_inspect(x = x2, y = y2, by = by2, show_rows = "x")
# join_many_to_many_inspect(x = x2, y = y2, by = by2, show_rows = "y")
#
# ## Ex 3 #####################################
# x3 <- tibble(pkey1 = c("study 1", "study 1", "study 1", "study 2"),
#              pkey2 = c("contr 1", "contr 1", "contr 2", "contr 1"),
#              value = letters[1:4])
# y3 <- tibble(fkey1 = c("study 1", "study 1", "study 2", "study 2"),
#              fkey2 = c("contr 1", "contr 2", "contr 1", "contr 1"),
#              value2 = 1:4)
# by3 <- join_by(pkey1 == fkey1, pkey2 == fkey2)
# x3 %>% inner_join(y3, by = by3)
#
# join_many_to_many_inspect(x = x3, y = y3, by = by3, show_rows = "none")
# join_many_to_many_inspect(x = x3, y = y3, by = by3, show_rows = "x")
# join_many_to_many_inspect(x = x3, y = y3, by = by3, show_rows = "y")
