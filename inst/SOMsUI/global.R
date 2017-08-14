if(!require(shiny)) {install.packages("shiny", dependencies = TRUE);require(shiny)}
if(!require(shinydashboard)) {install.packages("shinydashboard", dependencies = TRUE);require(shinydashboard)}
if(!require(shinyjs)) {install.packages("shinyjs", dependencies = TRUE);require(shinyjs)}
if(!require(shinyBS)) {install.packages("shinyBS", dependencies = TRUE);require(shinyBS)}
if(!require(rhandsontable)) {devtools::install_github("jrowen/rhandsontable");require(shinyBS)}
if(!require(dplyr)) {install.packages("dplyr", dependencies = TRUE);require(dplyr)}
if(!require(devtools)) {install.packages("devtools", dependencies = TRUE);require(devtools)}
if(!require(kohonen)) {install.packages("kohonen", dependencies = TRUE);require(kohonen)}
if(!require(data.table)) {install.packages("data.table", dependencies = TRUE);require(data.table)}
if(!require(fields)) {install.packages("fields", dependencies = TRUE);require(fields)}
if(!require(ggplot2)) {install.packages("ggplot2", dependencies = TRUE);require(ggplot2)}
library(TBSOM)
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#ffbb0e', '#a02c66','#000000')
pretty_palette.alpha <- adjustcolor(pretty_palette, alpha.f = 0.6)

theme_set(theme_bw())