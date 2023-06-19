if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
}
if (requireNamespace("sf", quietly = TRUE)) {
  library(sf)
}
if (requireNamespace("nhdplusTools", quietly = TRUE)) {
  library(nhdplusTools)
}
if (requireNamespace("here", quietly = TRUE)) {
  library(here)
}

data_folder <- here::here("data", "UScounties")
FIPS <- st_read(data_folder) #read county FIPS map provided by esri
polygons <- st_make_valid(FIPS) #get rid of 1 anomaly
