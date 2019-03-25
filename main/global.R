#Import Required Packages
packages = c('shiny', 'shinydashboard', 'shinyWidgets', 'shinycssloaders', 'shinythemes', 'shinyjs', 
             'shinyBS', 'tidyverse', 'sp','maps','maptools', 'gstat', 'rgeos', 'sf', 'raster', 'rgdal',
             'geofacet', 'ggmap', 'dendextend', 'heatmaply','tmap','leaflet',
             'DT', 'GWmodel', 'nngeo',
             'corrplot', 'rlang')
# for (p in packages){
  # if (!require(p, character.only = T)) {
  #   install.packages(p)
  # }
#   library(p,character.only = T)
# }
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
presch <- st_read("data/spatial", layer = "PRESCHOOL", crs = 3414)
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

#non-lm-Var Columns
nonlmVars <- c("MONTH", "TOWN", "FLAT_TYPE", "BLOCK", "STREET_NAME",
               "STOREY_RANGE", "FLAT_MODEL", "LEASE_COMMENCE_DATE",
               "FULL_ADDRESS", "YEAR", "X", "Y"
               #"geometry"
               )
