% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hexagon.R
\name{hexagon}
\alias{hexagon}
\title{Plot a hexagon shaped polygon.}
\usage{
hexagon(x, y, unitcell = 1, col = NA, border = "black", ...)
}
\arguments{
\item{x}{x coordinate of the center.}

\item{y}{y coordinate of the center.}

\item{unitcell}{radius of the hexagon}

\item{col}{the color for filling the hexagon The default, NA, is to leave the hexagon unfilled.}

\item{border}{the color to draw the border. Use \code{border = NA} to omit borders.}
}
\value{
Plotted hexagon.
}
\description{
This function provides the basis for the hexagonal heatmaps.
}
\examples{
plot(1:5)
hexagon(2,2, col = "blue")
}
