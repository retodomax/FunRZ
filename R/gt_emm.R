emm_labels <- list(# In emmeans
  # <specs> (var1, var2, ...)
  emmean = "EMM",
  response = "Response", # in case of backtransformation
  # In Contrast
  contrast = "Contrast",
  estimate = "Estimate",
  ratio = "Ratio", # in case of backtransformation of log
  SE = "SE",
  df = "df",
  lower.CL = "lower CL",
  upper.CL = "upper CL",
  t.ratio = "t-statistic",
  z.ratio = "z-statistic",
  p.value = "p-value")

#' Print emmeans output as gt table
#'
#' @param object emmeans ouput (object of class `emmGrid`, `emm_list`, `summary_emm` or `summary_eml`)
#' @param significant_digits How many significant digits to be printed for columns EMM, Response, Estimate, Ratio, SE and CI
#' @param output_first_only True by default. If input contains emmean and contrast, then only emmean is printed.
#' If FALSE, list of gt objects is returned. Quarto will print the list of gt tables as raw html code
#' (unless `output: asis` is specified).
#'
#' @return gt object or list of gt objects
#' @export
#'
#' @examples
#' fit <- lm(breaks ~ wool * tension, data = warpbreaks)
#' myem <- emmeans::emmeans(fit, pairwise ~ wool | tension, infer = c(TRUE, TRUE))
#' gt_emm(myem)
#' gt_emm(myem$contrasts)
gt_emm <- function(object, significant_digits = 3, output_first_only = TRUE, signif_stars = FALSE) {

  ## check and format input
  if(!inherits(object, c("emmGrid", "emm_list", "summary_emm", "summary_eml"))){
    stop("Object must be of class emmGrid, emm_list, summary_emm or summary_eml")
  }
  s_obj <- summary(object)
  if(inherits(s_obj, "summary_emm")){
    s_obj <- list(s_obj)
  }

  ## loop trough object (emmean and contrast)
  out <- vector("list", length = length(s_obj))
  for (i in seq_along(s_obj)) {
    myem <- s_obj[[i]]
    ## extract attributes
    notes <- attr(myem, "mesg")
    byv <- attr(myem, "by.vars")

    ## transform columns
    myem_t <- as_tibble(myem)
    if (!is.null(myem_t$p.value)) {
      p_orig <- myem_t$p.value
      p <- format(round(p_orig, 3), nsmall = 3)
      p[myem_t$p.value < 0.001] <- "<0.001"
      myem_t$p.value <- p
      ## format.pval() looks good at first sight but there are > 3 digits if e.g. p = 0.0011
      # myem_t$p.value <- format.pval(myem_t$p.value, digits = 3, eps = 0.001, scientific = FALSE)

      if (signif_stars) {
        myem_t$stars <- symnum(p_orig, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                               symbols = c("***", "**", "*", ".", " "), na = FALSE) %>%
          format()

      }
    }
    if (!is.null(byv)){
      for(j in byv){
        myem_t[[j]] <- paste(j, "=", myem_t[[j]])
      }
    }

    ## determine number of digits to print for emmean, etc.
    priority <- c("emmean", "response", "estimate", "ratio")
    available <- intersect(priority, names(myem_t))
    if (length(available) == 0) {
      stop("None of the following columns are available: emmeans, response, estimate, ratio\nAt least one is expected")
    }
    myresponse <- myem_t[[available[1]]]
    first_significant_digit <- floor(log10(abs(myresponse)))
    mydecimals <- max(-first_significant_digit + significant_digits - 1, 0)

    ## create gt table
    mygt <- myem_t %>%
      labelled::set_variable_labels(.labels = emm_labels, .strict = FALSE) %>%
      gt::gt(groupname_col = byv, row_group.sep = ", ") %>%
      gt::fmt_number(columns = any_of(c("emmean", "response", "estimate", "ratio", "SE", "lower.CL", "upper.CL")),
                     decimals = mydecimals, sep_mark = "\U2009") %>%
      gt::fmt_number(columns = any_of(c("t.ratio", "z.ratio")), decimals = 2, sep_mark = "\U2009") %>%
      gt::tab_source_note(source_note = notes) %>%
      FunRZ::gt_theme_sci() %>%
      gt::tab_style(style = gt::cell_text(weight = "bold"),
                    locations = gt::cells_row_groups())
    if (!is.null(myem_t$p.value)) {
      mygt <- gt::cols_align(mygt, "right", columns = p.value)
      if (signif_stars) {
        mygt <- gt::cols_label(mygt, stars = "")
      }
    }
    if (!is.null(myem_t$df)) {
      decimals_df <- ifelse(all(zapsmall(myem_t$df - round(myem_t$df)) == 0), 0, 2)
      mygt <- gt::fmt_number(mygt, columns = df, decimals = decimals_df, sep_mark = "\U2009")
    }
    out[[i]] <- mygt
  }
  if(output_first_only){
    return(out[[1]])
  } else {
    return(out)
  }
}
