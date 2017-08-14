#' Plot a hexagon shaped polygon.
#' 
#' This function provides the basis for the hexagonal heatmaps.
#' 
#' @param x x coordinate of the center.
#' @param y y coordinate of the center.
#' @param unitcell radius of the hexagon
#' @param col the color for filling the hexagon The default, NA, is to leave the hexagon unfilled.
#' @param border the color to draw the border. Use \code{border = NA} to omit borders.
#' @return Plotted hexagon.
#' 
#' @examples 
#' plot(1:5)
#' hexagon(2,2, col = "blue")
#' @export
hexagon <- function (x, y, unitcell = 1, col = NA, border = "black", ...) {
  polygon(x = c(x - 0.5 * unitcell, 
                x - 0.5 * unitcell, 
                x, 
                x + 0.5 * unitcell, 
                x + 0.5 * unitcell, 
                x), 
          y = c(y - 1/3 * sqrt(3)/2 * unitcell, 
                y + 1/3 * sqrt(3)/2 * unitcell, 
                y + 2/3 * sqrt(3)/2 * unitcell, 
                y + 1/3 * sqrt(3)/2 * unitcell, 
                y - 1/3 * sqrt(3)/2 * unitcell, 
                y - 2/3 * sqrt(3)/2 * unitcell), 
          col = col, border = border, ...)
}
