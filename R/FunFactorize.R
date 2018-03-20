#' Factorize columns
#'
#' Factorize columns which were not identified as factors
#' @param DataSetNames Name of the dataframe where some columns should be factorized
#' @param Variablenames string vector, Name of the columns to be factorized
#' @return Dataframe with factorized columns
#' @examples df <- data.frame(x = 1:10, y = letters[1:10])
#' @examples str(df)
#' @examples df$y <- as.character(df$y)
#' @examples str(df)
#' @examples str(FunFactorize(df, "y"))
#' @export
FunFactorize <- function(DataSetName, VariableNames){
  VariableNumbers <- match(VariableNames, colnames(DataSetName))
  for(CGIndex in VariableNumbers){
    DataSetName[[CGIndex]] <- as.factor(as.character(DataSetName[[CGIndex]]))
  }
  return(DataSetName)
}
