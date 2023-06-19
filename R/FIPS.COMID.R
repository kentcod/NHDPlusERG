
#COMID to Most Common FIPS function
#user directions: enter COMID, function typically takes 10-15 seconds to run
#' @title COMID to Most Common FIPS
#' @author Kent Codding
#' @description
#' Obtaining a county code for environmental assessment allows further assessment of other databases that use county codes. For example, these county codes can be used to extract census data: a major factor when considering environmental harms.
#' The plant AES Cayuga LLC (COMID = 21980363) on Cuyaga Lake borders both Tompkins (36109) and Senaca County (36099). About 2.6 Million different COMIDs exist within the continental U.S. but only 3141 counties exist in the U.S. Therefore, the vast majority of hydrological features will reside in only one county. However, features like AES Cayuga sometimes lie across the borders of two counties. This function returns the most important FIPS county code for further analysis: 36109 in this case as more coordinates within the NHDPlus dataset lie in Tompkins county.
#' @usage determines county code for a given COMID
#' @param COMID as a numeric value
#'
#' @return FIPS county code that the COMID lies in
#'
#' @examples COMID.FIPS(21980363) will return 36109
#
COMID.FIPS <- function(COMID){
  suppressWarnings({
    options(digits = 10)
    flowline <- navigate_nldi(list(featureSource = "comid", featureID = COMID)) #use navigate_nldi function from NHDplus tools to pull spatial data from COMID
    test <- unlist(flowline$origin$geometry) #unlist coordinates into 100 length vector
    df <- data.frame(X = test[1:(length(test)/2)], Y = test[(length(test)/2 + 1):length(test)]) #add coords to X, Y df
    sf_df <- st_as_sf(df, coords = c("X", "Y"), crs = st_crs(FIPS)) #coerce dataframe into spatial df with same geom as polygon layer
    overlay <<- st_intersection(sf_df, polygons) #perform spatial overlay with point coords, FIPS
    if(length(overlay$FIPS) == 0){ #checks to make sure points exist in a county (and not on River or Lake between county boundaries)
      overlay <<- st_join(sf_df, polygons, join = st_nearest_feature) #finds nearest county
    }
    codes <- as.numeric(overlay$FIPS) #create numeric vector of FIPS codes
    counts <- table(codes) #create a table showing instances of each code
    mode <- as.numeric(names(counts))[which.max(counts)] #find which name (FIPS codes) has most counts
    return(mode)
  })
}

#COMID to all FIPS function
#user directions: enter COMID, function typically takes 10-15 seconds to run
#' @title COMID to All FIPS
#' @author Kent Codding
#' @description
#' Obtaining a county code for environmental assessment allows further assessment of other databases that use county codes. For example, these county codes can be used to extract census data: a major factor when considering environmental harms.
#' The plant AES Cayuga LLC (COMID = 21980363) on Cuyaga Lake borders both Tompkins (36109) and Senaca County (36099). About 2.6 Million different COMIDs exist within the continental U.S. but only 3141 counties exist in the U.S. Therefore, the vast majority of hydrological features will reside in only one county. However, features like AES Cayuga sometimes lie across the borders of two counties. This function returns all FIPS county codes found at each point for further analysis: 36109 and 36099 in this case as 6 coordinates assigned to this COMID within the NHDPlus dataset lie in both Tompkins and Seneca county.
#' @usage determines county code for a given COMID
#' @param COMID as a numeric value
#'
#' @return FIPS county code that the COMID lies in
#'
#' @examples COMID.FIPS(21980363) will return 36109 36109 36109 36109 36099 36099
#
COMID.all.FIPS <- function(COMID){
  suppressWarnings({
    flowline <- navigate_nldi(list(featureSource = "comid", featureID = COMID)) #use navigate_nldi function from NHDplus tools to pull spatial data from COMID
    test <- unlist(flowline$origin$geometry) #unlist coordinates into 100 length vector
    df <- data.frame(X = test[1:(length(test)/2)], Y = test[(length(test)/2 + 1):length(test)]) #add coords to X, Y df
    sf_df <- st_as_sf(df, coords = c("X", "Y"), crs = st_crs(FIPS)) #coerce dataframe into spatial df with same geom as polygon layer
    overlay <<- st_intersection(sf_df, polygons) #perform spatial overlay with point coords, FIPS
    if(length(overlay$FIPS) == 0){ #checks to make sure points exist in a county (and not on River or Lake between county boundaries)
      overlay <<- st_join(sf_df, polygons, join = st_nearest_feature) #finds nearest county
    }
    return(as.numeric(overlay$FIPS))
  })
}

#COMID to first 10 FIPS function
#user directions: enter COMID, function typically takes 10-15 seconds to run
#' @title COMID to first 10 FIPS with coords
#' @author Kent Codding
#' @description
#' Obtaining a county code for environmental assessment allows further assessment of other databases that use county codes. For example, these county codes can be used to extract census data: a major factor when considering environmental harms.
#' The plant AES Cayuga LLC (COMID = 21980363) on Cuyaga Lake borders both Tompkins (36109) and Senaca County (36099). About 2.6 Million different COMIDs exist within the continental U.S. but only 3141 counties exist in the U.S. Therefore, the vast majority of hydrological features will reside in only one county. However, features like AES Cayuga sometimes lie across the borders of two counties. This function returns the first 10 FIPS county codes found at each point for further analysis: 36109 and 36099 in this case as 6 coordinates assigned to this COMID within the NHDPlus dataset lie in both Tompkins and Seneca county. Additionally, this function returns coordinates and a bounding box from NHDPlus for the given COMID. This can be used for spatial analysis.
#' @usage determines county code for a given COMID and where it is spatially
#' @param COMID as a numeric value
#'
#' @return FIPS county code that the COMID lies in
#'
#' @examples COMID.FIPS(21980363) will return 36109 36109 36109 36109 36099 36099
#
COMID.FIPSgeo <- function(COMID){
  suppressWarnings({
    flowline <- navigate_nldi(list(featureSource = "comid", featureID = COMID)) #use navigate_nldi function from NHDplus tools to pull spatial data from COMID
    test <- unlist(flowline$origin$geometry) #unlist coordinates into vector
    df <- data.frame(X = test[1:(length(test)/2)], Y = test[(length(test)/2 + 1):length(test)]) #add coords to X, Y df
    sf_df <- st_as_sf(df, coords = c("X", "Y"), crs = st_crs(FIPS)) #coerce dataframe into spatial df with same geom as polygon layer
    overlay <<- st_intersection(sf_df, polygons) #perform spatial overlay with point coords, FIPS
    if(length(overlay$FIPS) == 0){ #checks to make sure points exist in a county (and not on River or Lake between county boundaries)
      overlay <<- st_join(sf_df, polygons, join = st_nearest_feature) #finds nearest county
    }
    return(overlay)
  })
}
#COMID to first 10 FIPS function
#user directions: enter COMID, function typically takes 10-15 seconds to run
#' @title COMID and FIPS map
#' @author Kent Codding
#' @description
#' after running 1 of the 3 COMID to FIPS functions, simply enter "overlay" as the sole argument to return the map of COMID within its county code
#' @usage
#' visualize where COMID is
#' @param overlay as sf object
#'
#' @return map with points that represent NHDPlus coordinates for that given COMID
#'
#' @examples map.FIPS(overlay)
#
map.FIPS <- function(overlay){
  minx <- min(st_coordinates(overlay$geometry)[, 1])-0.2
  maxx <- max(st_coordinates(overlay$geometry)[, 1])+0.2
  miny <- min(st_coordinates(overlay$geometry)[, 2])-0.2
  maxy <- max(st_coordinates(overlay$geometry)[, 2])+0.2
  latitude <- st_coordinates(overlay$geometry)[, 1]
  longitude <- st_coordinates(overlay$geometry)[, 2]
  polyx <- sapply(polygons$geometry, st_coordinates)
  # iterate avg for each x and y coordinate for all 3141 elements of list; this will be used for polygon label coords
  xcent <- c() #create empty vectors to append centroids
  ycent <- c()
  for (i in seq_along(polyx)) {
    xcent <- append(xcent, polyx[[i]][, 1] %>% mean) #calculate mean of each index of polyx
    ycent <- append(ycent, polyx[[i]][, 2] %>% mean)
  }
  ggplot(data = polygons) +
    geom_sf() +
    geom_sf(data = overlay, color = "blue") +
    geom_text(data = polygons, aes(x = xcent, y = ycent, label = FIPS), color = "red", fontface = "bold") +
    coord_sf(xlim = c(minx, maxx), ylim = c(miny, maxy)) +
    xlab("Latitude") +
    ylab("Longitude")
}
#COMID to first 10 FIPS function
#user directions: enter COMID, function typically takes 10-15 seconds to run
#' @title COMID and FIPS flowline map
#' @author Kent Codding
#' @description
#' after running 1 of the 3 COMID to FIPS functions, simply enter "overlay" as the sole argument to return the map of COMID within its county code
#' @usage
#' visualize where COMID is
#' @param overlay as sf object
#'
#' @return zoomed in map of county codes that represent NHDPlus coordinates for that given COMID. COMIDs with larger spatial extent typically show a flowline in the form of FIPS codes
#'
#' @examples map.FIPS(overlay)
#
map.FIPSflow <- function(overlay){
  #zoomed in (shows flowline)
  minx <- min(st_coordinates(overlay$geometry)[, 1])-0.002
  maxx <- max(st_coordinates(overlay$geometry)[, 1])+0.002
  miny <- min(st_coordinates(overlay$geometry)[, 2])-0.002
  maxy <- max(st_coordinates(overlay$geometry)[, 2])+0.002
  latitude <- st_coordinates(overlay$geometry)[, 1]
  longitude <- st_coordinates(overlay$geometry)[, 2]
  polyx <- sapply(polygons$geometry, st_coordinates)

  # iterate avg for each x and y coordinate for all 3141 elements of list; this will be used for polygon label coords
  xcent <- c() #create empty vectors to append centroids
  ycent <- c()
  for (i in seq_along(polyx)) {
    xcent <- append(xcent, polyx[[i]][, 1] %>% mean) #calculate mean of each index of polyx
    ycent <- append(ycent, polyx[[i]][, 2] %>% mean)
  }
  ggplot(data = polygons) +
    geom_sf() +
    geom_text(data = polygons, aes(x = xcent, y = ycent, label = FIPS), color = "red", fontface = "bold") +
    geom_text(data = overlay, aes(x = latitude, y = longitude, label = FIPS), color = "blue", size = 2.5) +
    coord_sf(xlim = c(minx, maxx), ylim = c(miny, maxy)) +
    xlab("Latitude") +
    ylab("Longitude")
}

#' @title zoomed out COMID and FIPS map
#' @author Kent Codding
#' @description
#' this function is just a zoomed out version of map.FIPS
map.FIPSzoomout <- function(overlay){
  #zoomed out (counties)
  minx <- min(st_coordinates(overlay$geometry)[, 1])-0.5
  maxx <- max(st_coordinates(overlay$geometry)[, 1])+0.5
  miny <- min(st_coordinates(overlay$geometry)[, 2])-0.5
  maxy <- max(st_coordinates(overlay$geometry)[, 2])+0.5
  latitude <- st_coordinates(overlay$geometry)[, 1]
  longitude <- st_coordinates(overlay$geometry)[, 2]
  polyx <- sapply(polygons$geometry, st_coordinates)

  # iterate avg for each x and y coordinate for all 3141 elements of list; this will be used for polygon label coords
  xcent <- c() #create empty vectors to append centroids
  ycent <- c()
  for (i in seq_along(polyx)) {
    xcent <- append(xcent, polyx[[i]][, 1] %>% mean) #calculate mean of each index of polyx
    ycent <- append(ycent, polyx[[i]][, 2] %>% mean)
  }
  ggplot(data = polygons) +
    geom_sf() +
    geom_sf(data = overlay, color = "blue") +
    geom_text(data = polygons, aes(x = xcent, y = ycent, label = FIPS), color = "red", fontface = "bold") +
    coord_sf(xlim = c(minx, maxx), ylim = c(miny, maxy)) +
    xlab("Latitude")+
    ylab("Longitude")
}
