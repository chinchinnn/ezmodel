#Test Function
testfxn <- function(x){
  return(paste(x, "World"))
}

#Import Required Packages
importPkgs <- function(){
  packages = c('shiny','shinydashboard', 'shinyWidgets', 'shinycssloaders','shinythemes', 'shinyjs', 'shinyBS',
               'tidyverse', 'sp','maps','maptools', 'gstat', 'rgeos', 'sf', 'raster', 'rgdal',
               'geofacet', 'ggmap', 'dendextend', 'heatmaply','tmap','leaflet',
               'DT', 'GWmodel',
               'corrplot', 'rlang')
  for (p in packages){
    if (!require(p, character.only = T)) {
      install.packages(p)
    }  
    library(p,character.only = T)
  }
}

#Calculate field of Distance of HDB to nearest FEATURE, Calculate count of FEATURES within X m radius
process_variables <- function(user_hdb, var_sf, x, var_name){
  d2n <- st_nn(user_hdb, var_sf, returnDist = TRUE) %>%
    .$dist %>%
    as.vector()
  d2nColName <- paste0("dist2nearest_", var_name)
  user_hdb <- mutate(user_hdb,  d2nColName = d2n)
  remove(d2n)
  
  withinradius <- st_nn(user_hdb, var_sf, maxdist = x, k = 999)
  
  temp <- c()
  for (i in 1:length(withinradius)){
    temp <- append(temp, length(withinradius[[i]]))
  }
  wrColName <- paste0("within", x, "radius_", var_name)
  hdb <- mutate(hdb, wrColName = temp)
  remove(temp)
  remove(withinradius)
  
  return(user_hdb)
}

#selectedData
selectedData <- c()

