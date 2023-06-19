#' @import sf
#' @import nhdplusTools
#' @import ggplot2
#' @importFrom sf st_read st_make_valid


setwd("C:/Users/Kent Codding/Desktop/ERG Summer 2023")
FIPS <- st_read("UScounties") #read county FIPS map provided by esri
polygons <- st_make_valid(FIPS) #get rid of 1 anomaly
