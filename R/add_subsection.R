#' Insert Subsection
#'
#' Like the Ctrl+Shift+R command but adds two `##` in front and less `-` after
#' @import shiny
#' @import miniUI
#' @export

add_subsection <- function() {
  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
  ui <- miniUI::miniPage(
    gadgetTitleBar("Subsection"),
    miniContentPanel(
      textInput("subsection_text", label = "")
    )
  )

  server <- function(input, output, session) {
    # Listen for 'done' events. When we're finished, we'll
    # insert the input text as subsection
    observeEvent(input$done, {
      subsectionText <- paste0(stringr::str_pad(paste0("## ", input$subsection_text, " "),
                                         45, "right", pad = "#"), "\n")
      rstudioapi::insertText(subsectionText)
      stopApp()
    })

  }

  viewer <- dialogViewer("Subsection", width = 500, height = 400)
  runGadget(ui, server, viewer = viewer)

}
