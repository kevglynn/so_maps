#' Plot one of many hexagonal evaluation plots.
#' 
#' hexagonalPlotEval allows to plot different evaluation plots in a hexagonal
#' layout.
#' 
#' @param somobj an object of class "kohonen" or "somgrid".
#' @param type choose an evaluation plot. Possible values are "counts", "quality", "dist.neighbours" and "heatmap".
#' @param feature if \code{type = "heatmap"} the variable for the heatmap has to be defined.
#' @return Plot a evaluation hexagonal plot.
#' 
#' @export
hexagonalPlotEval <- function(somobj, type, feature, ...){
  if(type == "counts"){
    values <- rep(NA, nrow(somobj$grid$pts))
    freq <- table(somobj$unit.classif)
    values[as.integer(names(freq))] <- freq
    hexagonalPlot(somobj,values, ...)
  }
  if(type == "quality"){
    values <- rep(NA, nrow(somobj$grid$pts))
    hits <- as.integer(names(table(somobj$unit.classif)))
    values[hits] <- sapply(split(somobj$distances, somobj$unit.classif), mean)
    hexagonalPlot(somobj,values, ...)
  }
  if(type == "dist.neighbours"){
    nhbrdist <- unit.distances(somobj$grid, somobj$toroidal)
    nhbrdist[nhbrdist > 1.05] <- NA
    if (somobj$method == "som") {
      for (i in 2:nrow(nhbrdist)) {
        for (j in 1:(i - 1)) {
          if (!is.na(nhbrdist[i,j]))
            nhbrdist[i,j] <- nhbrdist[j,i] <- dist(somobj$codes[c(i,j),])
        }
      }
    }
    values <- colSums(nhbrdist, na.rm = TRUE)
    hexagonalPlot(somobj,values, ...)
  }
  if(type == "heatmap"){
    values <- somobj$codes[,feature]
    hexagonalPlot(somobj,values, ...)
  }
}
