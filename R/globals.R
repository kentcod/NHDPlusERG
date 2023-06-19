
if (requireNamespace("here", quietly = TRUE)) {
  library(here)
}

data_folder <- here::here("data", "UScounties")
FIPS <- st_read(data_folder) #read county FIPS map provided by esri
polygons <- st_make_valid(FIPS) #get rid of 1 anomaly
