#' Restart R session
#'
#' Like the Ctrl+Shift+F10 command but additionally clears console and
#' moves focus to source editor
#' @export
restart_r_session <- function(){
  rstudioapi::restartSession(command = "cat('\014'); invisible(capture.output(rstudioapi::executeCommand('activateSource')))")
}

# cat('\014')
## cleans console
## https://stackoverflow.com/a/16084793/6152316

# rstudioapi::executeCommand('activateSource'))
## puts focus on source
## https://docs.rstudio.com/ide/server-pro/rstudio_ide_commands/rstudio_ide_commands.html


# invisible(capture.output(...))
## makes sure that executeComman() does not return NULL
## https://stackoverflow.com/a/34208658/6152316


# ONLY ONE PROBLEM LEFT: rstudioapi::restartSession() does not work as expected at the moment :(
# Keep track of these two issues
# - https://github.com/rstudio/rstudio/issues/2841
# - https://github.com/rstudio/rstudioapi/issues/111
