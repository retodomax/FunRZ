#' Open corresponding html of a Rmd file
#'
#' Opens corresponding html file of an open Rmd file in Viewer pane
#' without recompiling it
#' @export
open_corresponding_html <- function() {
  file_path <- rstudioapi::getActiveDocumentContext()$path # extract current file path
  html_path <- gsub(pattern = "(.Rmd$|.qmd$)", replacement = ".html", x = file_path, ignore.case = TRUE) # replace Rmd with html
  if(!file.exists(html_path)){stop("No html of same name found in same directory")}
  file.copy(html_path, file.path(tempdir(), "test.html"), overwrite = TRUE) # copy to temp folder
  myViewer <- getOption("viewer") # extract Viewer function
  myViewer(file.path(tempdir(), "test.html")) # open temp file in Viewer
}

## How to execute R code with shortcut
# https://rstudio.github.io/rstudioaddins/

## How to open html in Viewer pane
# https://stackoverflow.com/a/35660129/6152316

## How to make it available as shortcut
# RStudio > Tools > Addins > Browse Addins > Keyboard Shortcuts...
# Then define shortcut for 'Open curresponding html' (Ctrl + Shift + L)
