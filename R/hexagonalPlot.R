#' Plot a polygon heatmap.
#' 
#' This function creates a hexagon layouted heatmap.
#' 
#' @param somobj an object of class "kohonen" or "somgrid".
#' @param values vector of values for the heatmap starting from the left bottom.
#' @param border the color to draw the border. Use \code{border = NA} to omit borders.
#' @param border.lwd linewidth of the border.
#' @param colors vector of colors used for mapping the values to colors.
#' @param segment do the values contain segments?
#' @param addlegend add a legend to the plot.
#' @param alpha transparency of the color – 1.0 is opaque and 0 is transparent.
#' @return Visualize a heatmap in a hexagonal layout.
#' 
#' @examples 
#' library(kohonen)
#' ttt <- som(data = as.matrix(iris[,1:4]), somgrid(5,7, "hexagonal"))
#' sampledata <- runif(35, min = 0, max = 7.5)
#' hexagonalPlot(ttt, sampledata)
#' 
#' @import fields
#' @export
hexagonalPlot <- function (somobj, values, border = "gray30", border.lwd = 2, colors, main = NULL, addlegend = TRUE, alpha = 1.0, segment = FALSE, ...) {
  
  require(fields)
  
  ### Colors -------------------------------------------------------------------
  
  # create default colors if parameter is missing
  if (missing(colors)) {
    colors <- fields::designer.colors(n=256, col=c("#6363FF", "#6373FF", "#63A3FF", "#63E3FF", "#63FFFB", "#63FFCB",
                                            "#63FF9B", "#63FF6B", "#7BFF63", "#BBFF63", "#DBFF63", "#FBFF63", 
                                            "#FFD363", "#FFB363", "#FF8363", "#FF7363", "#FF6364"),
                              alpha = alpha)
  } 
  
  ### Grid ---------------------------------------------------------------------
  
  # export grid information
  if(class(somobj) == "kohonen"){
    somgrid <- somobj$grid
  } else if (class(somobj) == "somgrid"){
    somgrid <- somobj
  } else {
    stopp("somobj has to be of class kohonen or somgrid")
  }
  
  if(segment == FALSE){
    # set default color
    colorMapping <- rep("#FFFFFF", length(values))
    # set colors according to values
    bins <- seq(min(values[!is.na(values)]), max(values[!is.na(values)]), length.out = length(colors))
    for (i in 1:length(values)) {
      if (!is.na(values[i])) {
        colorMapping[i] <- colors[which.min(abs(bins-values[i]))] 
      }
    }
  } else {
    colorMapping <- colors
  }
  

  
  ### Visualisation ------------------------------------------------------------
  
  # change plotting area
  oldpar <- par(xaxs = "i", yaxs = "i", mar = c(0.4, 2, 2, 7))
  on.exit(oldpar)
  
  # create empty plot area
  plot(NA, type = "n", axes = FALSE, asp = 1, 
       xlim = c(0, somgrid$xdim+addlegend), # +1 for additional space between plot and legend
       ylim = c(0, somgrid$ydim),
       main = main)
  
  # plot hexagons
  par(xpd = TRUE)
  i <- 1
  for (xdim in 1:somgrid$xdim) {
    for (ydim in 1:somgrid$ydim) {
      hexagon(somgrid$pts[i,"x"], somgrid$pts[i,"y"], col = colorMapping[i], border = border, lwd = border.lwd, ...)
      i <- i + 1
    }
  }
    par(xpd = FALSE)
  
  # add legend
  if(addlegend){
    image.plot(legend.only = TRUE, col = colors, 
               zlim=c(min(values, na.rm=TRUE), max(values, na.rm=TRUE)))
  }
}
