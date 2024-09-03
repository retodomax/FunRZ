#' Add scientific theme to gt
#'
#' @param dat The gt table data object. This is the gt table object that is commonly created through use of the gt() function.
#'
#' @return object of class `gt_tbl`
#' @export
#'
#' @examples
#' iris %>%
#'  head() %>%
#'  gt() %>%
#'  gt_theme_sci()
gt_theme_sci <- function(data){
  data |>
    tab_options(
      table.background.color = 'gray97',
      column_labels.padding = px(3),
      data_row.padding = px(1),
      row_group.padding = px(2),
      summary_row.padding = px(2),
      grand_summary_row.padding = px(2),
      table.border.top.style = 'solid',
      table.border.top.color = 'gray20',
      table.border.top.width = px(1),
      table.border.bottom.style = 'none',
      heading.border.bottom.style = 'none',
      column_labels.vlines.style = 'none',
      column_labels.border.top.style = 'none',
      column_labels.border.bottom.style = 'solid',
      column_labels.border.bottom.width = px(1),
      row_group.border.top.style = 'none',
      row_group.border.bottom.style = 'none',
      table_body.hlines.style = 'none',
      table_body.border.top.style = 'solid',
      table_body.border.top.color = 'gray20',
      table_body.border.top.width = px(1),
      table_body.border.bottom.style = 'solid',
      table_body.border.bottom.color = 'gray20',
      table_body.border.bottom.width = px(1),
      stub.border.style = 'none',
      stub_row_group.border.style = 'none',
      summary_row.border.style = 'none',
      grand_summary_row.border.style = 'none',
      footnotes.border.bottom.style = 'none',
      source_notes.border.bottom.style = 'none',
    ) |>
    sub_missing(missing_text = "")
  ## This part has to be removed for pdf/docx
  # tab_stub_indent(rows = everything(), indent = 2)
}
