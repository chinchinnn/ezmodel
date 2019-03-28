#Import Required Packages
# packages = c('shiny', 'shinydashboard', 'shinyWidgets', 'shinycssloaders', 'shinythemes', 'shinyjs', 
#              'shinyBS', 'tidyverse', 'sp','maps','maptools', 'gstat', 'rgeos', 'sf', 'raster', 'rgdal',
#              'geofacet', 'ggmap', 'dendextend', 'heatmaply','tmap','leaflet',
#              'DT', 'GWmodel', 'nngeo',
#              'corrplot', 'rlang')
# for (p in packages){
  # if (!require(p, character.only = T)) {
  #   install.packages(p)
  # }
#   library(p,character.only = T)
# }

#INDIVIDUAL LINES USED FOR DEPLOYING ONTO SHINYAPPS.IO; USE ABOVE LINES IN RSTUDIO IF PACKAGES NOT INSTALLED
library('shiny')
library('tidyverse')
library('sp')
library('maps')
library('maptools')
library('gstat')
library('rgeos')
library('sf')
library('raster')
library('rgdal')
library('geofacet')
library('ggmap')
library('dendextend')
library('heatmaply')
library('tmap')
library('leaflet')
library('DT')
library('GWmodel')
library('nngeo')
library('corrplot')
library('rlang')
library('shinydashboard')
library('shinyWidgets')
library('shinycssloaders')
library('shinythemes')
library('shinyjs')
library('shinyBS')
#Load Preloaded Data
##-------------------------------------------------------HDB RESALE DATA----------------------------------------
hdb <- read_csv("data/HDB_DATA.csv") %>%
  select("RESALE_PRICE", everything())
##-------------------------------------------------------MPSUBZONE----------------------------------------------
mpsz <- st_read("data/spatial", layer = "MP14_SUBZONE_WEB_PL", crs = 3414)
mpsz_sp <- as(mpsz, "Spatial")
##-------------------------------------------------------RAFFLES PLACE PARK-------------------------------------
rpp <- matrix(c("Raffles Place Park", 1.2841836, 103.8515103), ncol = 3)
colnames(rpp) <- c("name", "lat", "long")
rpp <- as_tibble(rpp) %>%
  st_as_sf(coords = c("long", "lat"), crs = "+init=epsg:4326") %>%
  st_transform(crs = 3414)
##-------------------------------------------------------SPORTS FACILITIES--------------------------------------
sport <- st_read("data/spatial", layer = "sports-facilities", crs = 3414)
##-------------------------------------------------------PARKS
park <- st_read("data/spatial", layer = "nparks-park", crs = 3414)
##-------------------------------------------------------FOOD CENTRES-------------------------------------------
foodctr <- st_read("data/spatial", layer = "food-centres", crs = 3414)
##-------------------------------------------------------MRT/LRT STATIONS
mrt <- st_read("data/spatial", layer = "MRTLRTStnPtt", crs = 3414)
##-------------------------------------------------------PRESCHOOLS---------------------------------------------
presch <- st_read("data/spatial", layer = "PRESCHOOL", crs = 4326) %>% 
  st_transform(crs = 3414) %>%
  st_zm()
##-------------------------------------------------------PRI and SEC SCHOOLS------------------------------------
sch <- read_csv("data/spatial/schoolLatLng.csv")
prisch <- sch %>%
  filter(mainlevel_code == "PRIMARY") %>%
  st_as_sf(coords = c("X", "Y"), crs = "+init=epsg:4326") %>%
  st_transform(crs = 3414)
secsch <- sch %>%
  filter(mainlevel_code == "SECONDARY" | mainlevel_code == "MIXED LEVEL") %>%
  st_as_sf(coords = c("X", "Y"), crs = "+init=epsg:4326") %>%
  st_transform(crs = 3414)



##END OF LOADING PRELOADED DATA

##------------------------------------------------FUNCTIONS------------------------------------------------

##Calculate field of Distance of HDB to nearest FEATURE, Calculate count of FEATURES within X m radius
process_variables <- function(user_hdb, var_sf, x, var_name){
  
  ##GET DIST 2 NEAREST FEATURE
  d2n <- st_nn(user_hdb, var_sf, returnDist = TRUE, k = 1) %>% 
    .$dist %>%
    as.vector()
  
  d2nColName <- paste0("DIST2NEAREST_", var_name)
  result <- data_frame("a" = d2n)
  result <- rename(result, UQ(rlang::sym(d2nColName)) := "a")
  
  ##GET NO. OF FEATURES WITHIN RADIUS
  withinradius <- st_nn(user_hdb, var_sf, maxdist = x, k = nrow(var_sf))
  temp <- c()
  for (i in 1:length(withinradius)){
    temp <- append(temp, length(withinradius[[i]]))
  }
  wrColName <- paste0("WITHIN", x, "RADIUS_", var_name)
  result <- cbind(result, "b" = temp)
  result <- rename(result, UQ(rlang::sym(wrColName)) := "b")

  return(result)
}

dist2nearest_only <- function(user_hdb, var_sf, x, var_name){

  d2n <- st_nn(user_hdb, var_sf, returnDist = TRUE, k = 1) %>% 
    .$dist %>%
    as.vector()
  
  d2nColName <- paste0("DIST2NEAREST_", var_name)
  result <- data_frame("a" = d2n)
  result <- rename(result, UQ(rlang::sym(d2nColName)) := "a")

  return(result)
}

#non-lm-Var Columns
nonlmVars <- c("MONTH", "TOWN", "FLAT_TYPE", "BLOCK", "STREET_NAME",
               "STOREY_RANGE", "FLAT_MODEL", "LEASE_COMMENCE_DATE",
               "FULL_ADDRESS", "YEAR", "X", "Y"
               #"geometry"
               )
preloadDataNames <- c("CBD_RafflesPlacePark",
                      "MRT_LRT_Stations",
                      "Preschools",
                      "Primary_Schools",
                      "Secondary_Schools",
                      "Food_Centres",
                      "Parks",
                      "Sports_Facilities")
