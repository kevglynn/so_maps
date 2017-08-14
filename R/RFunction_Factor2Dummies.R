#' Factors into dummies variables 
#' 
#' Convert factor variable into multiple dummy variables
#' 
#'
#' @param df - dataframe
#' @return data frame without any factor variable (each original factor is replaced by a 
#' set of dummy variables.
#' @examples 
#' \code{RFunction_Factor2Dummies(iris)} - replaces factor (\emph{Species}) into 3 dummies 
#' variables: \emph{Speciessetosa, Speciesversicolor, Speciesvirginica}
#' 

contr.onehot = function (n, contrasts, sparse = FALSE) {
  contr.sum(n = n, contrasts = FALSE, sparse = sparse)
}

#' @export
RFunction_Factor2Dummies <- function(df){
  options(contrasts = c("contr.onehot", "contr.onehot"))
  return(as.data.frame(model.matrix(~ . - 1, data = df)))
}

