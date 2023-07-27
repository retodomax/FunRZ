#' Insert Section
#'
#' Like the Ctrl+Shift+R command but also works in RMarkdown/Quarto code chunks
#' @import shiny
#' @import miniUI
#' @export

add_section <- function() {
  # Our ui will be a simple gadget page
  ui <- miniUI::miniPage(
    gadgetTitleBar("Section"),
    miniContentPanel(
      textInput("section_text", label = "")
    )
  )

  server <- function(input, output, session) {
    # Listen for 'done' events. When we're finished, we'll
    # insert the input text as subsection
    observeEvent(input$done, {
      subsectionText <- paste0(stringr::str_pad(paste0("# ", input$section_text, " "),
                                                75, "right", pad = "-"), "\n\n")
      rstudioapi::insertText(subsectionText)
      stopApp()
    })

  }

  viewer <- dialogViewer("Section", width = 500, height = 400)
  runGadget(ui, server, viewer = viewer)

}
