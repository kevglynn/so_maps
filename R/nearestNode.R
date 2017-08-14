#' Identify nearest node to click location.
#' 
#' After clicking into the interactive visualisation identify the nearest node 
#' based on euclidean distance.
#'
#' @param somobj an object of class "kohonen" or "somgrid"
#' @param click x/y location of the user click
#' @return Node number with smallest distance to clicked location.
#' @examples 
#' som_model <- som(as.matrix(iris[,1:4]), 
#' grid=somgrid(5,5,"hexagonal"))
#' nearestNode(somobj = som_model, click = c(2.5,1))
#' 
#' nearestNode(somobj = somgrid(5,5,"hexagonal"), click = c(2.5,1))
#' @export
nearestNode <- function(somobj, click){
  
  if(class(somobj) == "kohonen"){
    somgrid <- somobj$grid$pts
  } else if (class(somobj) == "somgrid"){
    somgrid <- somobj$pts
  } else {
    stopp("somobj has to be of class kohonen or somgrid")
  }
  
  return(which.min(sqrt((somgrid[,1] - click[1])^2 + (somgrid[,2] - click[2])^2)))
}

