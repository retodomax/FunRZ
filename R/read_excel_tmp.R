#' Read Excel from Temporary Copy
#'
#' @param path see [readxl::read_excel()]
#' @param sheet see [readxl::read_excel()]
#' @param range see [readxl::read_excel()]
#' @param col_names see [readxl::read_excel()]
#' @param col_types see [readxl::read_excel()]
#' @param na see [readxl::read_excel()]
#' @param trim_ws see [readxl::read_excel()]
#' @param skip see [readxl::read_excel()]
#' @param n_max see [readxl::read_excel()]
#' @param guess_max see [readxl::read_excel()]
#' @param progress see [readxl::read_excel()]
#' @param .name_repair see [readxl::read_excel()]
#'
#' @return A tibble
#' @export
read_excel_tmp <- function(path, sheet = NULL, range = NULL, col_names = TRUE,
                           col_types = NULL, na = "", trim_ws = TRUE,
                           skip = 0, n_max = Inf, guess_max = min(1000, n_max),
                           progress = readxl::readxl_progress(),
                           .name_repair = "unique"){
  destfile <- tempfile(fileext = ".xlsx")
  ## file.copy() and Command Prompt command `copy` do not work (permission issue)
  ### This approach based on powershell's `Copy-Item` does work..
  mycmd <- paste0("powershell -command \"Copy-Item '",
                  gsub("/", "\\\\", path),
                  "' -Destination '",
                  destfile,
                  "'\"")
  error_code <- system(mycmd)
  if(error_code != 0) {stop("Powershell's `Copy-Item` was not able to copy-paste the file")}
  readxl::read_excel(path = destfile, sheet = sheet, range = range,
                     col_names = col_names, col_types = col_types, na = na,
                     trim_ws = trim_ws, skip = skip, n_max = n_max,
                     guess_max = guess_max, progress = progress,
                     .name_repair = .name_repair)
}

# Test --------------------------------------------------------------------

# #### Test function
# path_onedrive <- "C:/Users/858782/DSM/BovaerAnimalTrial - General/01_Admin/test_to_delete.xlsx"
# openxlsx::write.xlsx(iris, file = path_onedrive)
# ## open the excel to see if the function below still works...
# read_excel_tmp(path_onedrive)
# file.remove(path_onedrive)
