#' Data winsorization
#' 
#' Make the data robust with the help of winsorization, i.e., by shrinking outlying observations to the border of the main part of the data.
#' 
#' @param x a numeric vector to be winsorized.
#' @param minval the lower border, all values being lower than this will be replaced by this value.
#' @param maxval the upper border, all values being larger than this will be replaced by this value.
#' @param probs if minval or maxval is NULL use the given probabilities of quantile
#' @return A vector of the same length as the original vector containing the winsorized data.
#' 
#' @examples 
#' set.seed(42)
#' data <- rnorm(50)
#' summary(data)
#' summary(winsorize(data,probs=c(0.05, 0.95)))
#' summary(winsorize(data,probs=c(0.05, 1)))
#' @export
winsorize <- function(x, minval = NULL, maxval = NULL,
                      probs=c(0.05, 0.95)){
  
  if(is.null(minval) || is.null(maxval)){
    quantiles <- quantile(x=x, probs=probs, na.rm = TRUE)
    if(is.null(minval)) minval <- quantiles[1]
    if(is.null(maxval)) maxval <- quantiles[2]
  }
  
  x[x<minval] <- minval
  x[x>maxval] <- maxval
  
  return(x)
}
