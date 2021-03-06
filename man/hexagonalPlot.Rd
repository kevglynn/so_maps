% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hexagonalPlot.R
\name{hexagonalPlot}
\alias{hexagonalPlot}
\title{Plot a polygon heatmap.}
\usage{
hexagonalPlot(somobj, values, border = "gray30", border.lwd = 2, colors,
  main = NULL, addlegend = TRUE, alpha = 1, segment = FALSE, ...)
}
\arguments{
\item{somobj}{an object of class "kohonen" or "somgrid".}

\item{values}{vector of values for the heatmap starting from the left bottom.}

\item{border}{the color to draw the border. Use \code{border = NA} to omit borders.}

\item{border.lwd}{linewidth of the border.}

\item{colors}{vector of colors used for mapping the values to colors.}

\item{addlegend}{add a legend to the plot.}

\item{alpha}{transparency of the color – 1.0 is opaque and 0 is transparent.}

\item{segment}{do the values contain segments?}
}
\value{
Visualize a heatmap in a hexagonal layout.
}
\description{
This function creates a hexagon layouted heatmap.
}
\examples{
library(kohonen)
ttt <- som(data = as.matrix(iris[,1:4]), somgrid(5,7, "hexagonal"))
sampledata <- runif(35, min = 0, max = 7.5)
hexagonalPlot(ttt, sampledata)

}
