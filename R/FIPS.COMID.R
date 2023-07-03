
#COMID to Most Common FIPS function
#user directions: enter COMID, function typically takes 10-15 seconds to run
#' @title COMID to Most Common FIPS
#' @author Kent Codding
#' @description
#' Obtaining a county code for environmental assessment allows further assessment of other databases that use county codes. For example, these county codes can be used to extract census data: a major factor when considering environmental harms.
#' The plant AES Cayuga LLC (COMID = 21980363) on Cuyaga Lake borders both Tompkins (36109) and Senaca County (36099). About 2.6 Million different COMIDs exist within the continental U.S. but only 3141 counties exist in the U.S. Therefore, the vast majority of hydrological features will reside in only one county. However, features like AES Cayuga sometimes lie across the borders of two counties. This function returns the most important FIPS county code for further analysis: 36109 in this case as more coordinates within the NHDPlus dataset lie in Tompkins county.
#' @usage determines county code for a given COMID
#' @param COMID as a numeric value
#' @seealso [COMID.all.FIPS()][COMID.FIPSgeo][COMID.demo()]
#' @return FIPS county code that the COMID lies in
#' @examples COMID.FIPS(21980363) will return 36109
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
#' @seealso [COMID.FIPS()][COMID.FIPSgeo()][COMID.all.demo()]
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
#' @seealso [COMID.all.FIPS()][COMID.FIPS()][COMID.demo()]
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
#' @title COMID and FIPS map
#' @author Kent Codding
#' @description
#' after running 1 of the 3 COMID to FIPS functions, simply enter "overlay" as the sole argument to return the map of COMID within its county code
#' @usage
#' visualize where COMID is
#' @param overlay as sf object
#' @seealso [COMID.FIPS()][map.FIPSflow()][map.FIPSzoomout()]
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
#' @seealso [COMID.FIPS()][map.FIPSflow()][map.FIPSzoomout()]
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
#' @seealso [map.FIPS()][map.FIPSflow()][COMID.FIPS()]
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
#user directions: enter overlay after running any of the three COMID to FIPS functions
#' @title FIPS to ACS Demographics
#' @author Kent Codding
#' @description
#' This function uses the county code to extract census data: a major factor when considering environmental harms.
#' Demographics can be further assessed through an environmental justice lens to consider any communities disproportionately affected by pollutants at the location of certain COMIDs.
#' @usage returns demographics for a given FIPS code
#' @param overlay as an sf object
#' @seealso [COMID.FIPS()][COMID.demo()]
#' @return a named numeric vector of American Indian, Asian, Black, Latino, Other, White, and Total population found within given FIPS
#' @examples FIPS.demo(overlay)
FIPS.demo <- function(code){
  suppressWarnings({
    demolist <- list(FIPS = code)
    state <- demolist$FIPS %/% 1000 #integer division first 2 digits
    county <- demolist$FIPS %% 1000 #remainder function gives last 3 digits
    American.Indian <- get_acs(geography = 'county', variables = "B02001_004", state = state, county = county)
    demolist$American.Indian <- American.Indian$estimate
    Asian <- get_acs(geography = 'county', variables = "B02001_005", state = state, county = county)
    demolist$Asian <- Asian$estimate
    Black <- get_acs(geography = 'county', variables = "B02001_003", state = state, county = county)
    demolist$Black <- Black$estimate
    Latino <- get_estimates(geography = 'county', product = "characteristics", breakdown = "HISP", state = state, county = county)$value[3] #obtains value for hispanic population 2019
    demolist$Latino <- Latino
    Other <- get_acs(geography = 'county', variables = "B02001_007", state = state, county = county)
    demolist$Other <- Other$estimate
    White <- get_acs(geography = 'county', variables = "B02001_002", state = state, county = county)
    demolist$White <- White$estimate
    Total <- get_acs(geography = 'county', variables = "B02001_001", state = state, county = county)
    demolist$Total <- Total$estimate
    return(unlist(demolist))
  })
}
#' @title COMID to ACS Demographics
#' @author Kent Codding
#' @description
#' This function uses the FIPS county code that contains the most NHDPlus COMID xy coordinate points to extract census data: a major factor when considering environmental harms.
#' Demographics can be further assessed through an environmental justice lens to consider any communities disproportionately affected by pollutants at the location of certain COMIDs.
#' @usage returns demographics for a given COMID
#' @param COMID as a numeric object
#' @seealso [COMID.FIPS()][FIPS.demo()][COMID.all.demo()]
#' @return a list of named numeric vectors of American Indian, Asian, Black, Latino, Other, White, and Total population found within given FIPS
#' @examples FIPS.demo(21980363) will return a named vector of demographic populations for 36109: Tompkins county
COMID.demo <- function(COMID){
     suppressWarnings({
        flowline <- navigate_nldi(list(featureSource = "comid", featureID = COMID)) #use navigate_nldi function from NHDplus tools to pull spatial data from COMID
        test <- unlist(flowline$origin$geometry) #unlist coordinates into 100 length vector
        df <- data.frame(X = test[1:(length(test)/2)], Y = test[(length(test)/2 + 1):length(test)]) #add coords to X, Y df
        sf_df <- st_as_sf(df, coords = c("X", "Y"), crs = st_crs(FIPS)) #coerce dataframe into spatial df with same geom as polygon layer
        overlay <<- st_intersection(sf_df, polygons) #perform spatial overlay with point coords, FIPS
        if(length(overlay$FIPS) == 0){ #checks to make sure points exist in a county (and not on River or Lake between county boundaries)
              overlay <<- st_join(sf_df, polygons, join = st_nearest_feature) #finds nearest county
          }
        counts <- table(as.numeric(overlay$FIPS)) #create a table showing instances of each code
        mode <- as.numeric(names(counts))[which.max(counts)] #find which name (FIPS codes) has most counts
        demolist <- list(FIPS = mode)
        state <- demolist$FIPS %/% 1000 #integer division first 2 digits
        county <- demolist$FIPS %% 1000 #remainder function gives last 3 digits
        American.Indian <- get_acs(geography = 'county', variables = "B02001_004", state = state, county = county)
        demolist$American.Indian <- American.Indian$estimate
        Asian <- get_acs(geography = 'county', variables = "B02001_005", state = state, county = county)
        demolist$Asian <- Asian$estimate
        Black <- get_acs(geography = 'county', variables = "B02001_003", state = state, county = county)
        demolist$Black <- Black$estimate
        Latino <- get_estimates(geography = 'county', product = "characteristics", breakdown = "HISP", state = state, county = county)$value[3] #obtains value for hispanic population 2019
        demolist$Latino <- Latino
        Other <- get_acs(geography = 'county', variables = "B02001_007", state = state, county = county)
        demolist$Other <- Other$estimate
        White <- get_acs(geography = 'county', variables = "B02001_002", state = state, county = county)
        demolist$White <- White$estimate
        Total <- get_acs(geography = 'county', variables = "B02001_001", state = state, county = county)
        demolist$Total <- Total$estimate
        return(unlist(demolist))
     })
}
#user directions: enter COMID
#' @title COMID to all ACS Demographics
#' @author Kent Codding
#' @description
#' This function uses any FIPS county code that contains NHDPlus COMID xy coordinate points to extract census data: a major factor when considering environmental harms.
#' Demographics can be further assessed through an environmental justice lens to consider any communities disproportionately affected by pollutants at the location of certain COMIDs.
#' @usage returns demographics for a given COMID
#' @param COMID as a numeric object
#' @seealso [COMID.all.FIPS()][FIPS.demo()][COMID.demo()]
#' @return a list of named numeric vectors of American Indian, Asian, Black, Latino, Other, White, and Total population found within given FIPS
#' @examples FIPS.demo(21980363) will return a list of census data for 36109: Tompkins county and 36099: Seneca county
COMID.all.demo <- function(COMID){
  suppressWarnings({
    flowline <- navigate_nldi(list(featureSource = "comid", featureID = COMID)) #use navigate_nldi function from NHDplus tools to pull spatial data from COMID
    test <- unlist(flowline$origin$geometry) #unlist coordinates into 100 length vector
    df <- data.frame(X = test[1:(length(test)/2)], Y = test[(length(test)/2 + 1):length(test)]) #add coords to X, Y df
    sf_df <- st_as_sf(df, coords = c("X", "Y"), crs = st_crs(FIPS)) #coerce dataframe into spatial df with same geom as polygon layer
    overlay <<- st_intersection(sf_df, polygons) #perform spatial overlay with point coords, FIPS
    if(length(overlay$FIPS) == 0){ #checks to make sure points exist in a county (and not on River or Lake between county boundaries)
      overlay <<- st_join(sf_df, polygons, join = st_nearest_feature) #finds nearest county
    }
    counts <- table(as.numeric(overlay$FIPS)) #create a table showing instances of each code
    first <- as.numeric(names(counts)[1]) #find which name (FIPS codes) is listed first
    demolist <- list(FIPS = first)
    state <- demolist$FIPS %/% 1000 #integer division first 2 digits
    county <- demolist$FIPS %% 1000 #remainder function gives last 3 digits
    American.Indian <- get_acs(geography = 'county', variables = "B02001_004", state = state, county = county)
    demolist$American.Indian <- American.Indian$estimate
    Asian <- get_acs(geography = 'county', variables = "B02001_005", state = state, county = county)
    demolist$Asian <- Asian$estimate
    Black <- get_acs(geography = 'county', variables = "B02001_003", state = state, county = county)
    demolist$Black <- Black$estimate
    Latino <- get_estimates(geography = 'county', product = "characteristics", breakdown = "HISP", state = state, county = county)$value[3] #obtains value for hispanic population 2019
    demolist$Latino <- Latino
    Other <- get_acs(geography = 'county', variables = "B02001_007", state = state, county = county)
    demolist$Other <- Other$estimate
    White <- get_acs(geography = 'county', variables = "B02001_002", state = state, county = county)
    demolist$White <- White$estimate
    Total <- get_acs(geography = 'county', variables = "B02001_001", state = state, county = county)
    demolist$Total <- Total$estimate
      if(is.na(counts[2]) == TRUE){
        return(unlist(demolist))
      }
      else{
        second <- as.numeric(names(counts)[2]) #find which name (FIPS codes) is listed second
        demolist2 <- list(FIPS = second)
        state <- demolist2$FIPS %/% 1000 #integer division first 2 digits
        county <- demolist2$FIPS %% 1000 #remainder function gives last 3 digits
        American.Indian <- get_acs(geography = 'county', variables = "B02001_004", state = state, county = county)
        demolist2$American.Indian <- American.Indian$estimate
        Asian <- get_acs(geography = 'county', variables = "B02001_005", state = state, county = county)
        demolist2$Asian <- Asian$estimate
        Black <- get_acs(geography = 'county', variables = "B02001_003", state = state, county = county)
        demolist2$Black <- Black$estimate
        Latino <- get_estimates(geography = 'county', product = "characteristics", breakdown = "HISP", state = state, county = county)$value[3] #obtains value for hispanic population 2019
        demolist2$Latino <- Latino
        Other <- get_acs(geography = 'county', variables = "B02001_007", state = state, county = county)
        demolist2$Other <- Other$estimate
        White <- get_acs(geography = 'county', variables = "B02001_002", state = state, county = county)
        demolist2$White <- White$estimate
        Total <- get_acs(geography = 'county', variables = "B02001_001", state = state, county = county)
        demolist2$Total <- Total$estimate
        return(list(unlist(demolist), unlist(demolist2)))
      }
  })
}
