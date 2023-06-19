packages <- c("ggplot2", "sf", "nhdplusTools")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
lapply(packages, library, character.only = TRUE)

setwd("C:/Users/Kent Codding/Desktop/ERG Summer 2023")
FIPS <- st_read("UScounties") #read county FIPS map provided by esri
polygons <- st_make_valid(FIPS) #get rid of 1 anomaly
