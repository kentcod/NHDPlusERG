packages <- c("ggplot2", "sf", "nhdplusTools", "here")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
lapply(packages, library, character.only = TRUE)

data_folder <- here::here("data", "UScounties")
FIPS <- st_read(data_folder) #read county FIPS map provided by esri
polygons <- st_make_valid(FIPS) #get rid of 1 anomaly
