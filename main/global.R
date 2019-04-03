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
library('tmaptools')
library('spdep')
library('classInt')
library('grid')
library('gridExtra')
library('lattice')
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
##-------------------------------------------------------SHOPPING MALLS------------------------------------
shopping <- read_csv("data/spatial/shoppingMallsXY.csv") %>%
  st_as_sf(coords = c("X", "Y"), crs = "+init=epsg:3414") %>%
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
                      "Sports_Facilities",
                      "Shopping_Malls")

viewDatadesc <- data_frame("Name" = c("DIST2NEAREST_feature", "WITHINxRADIUS_feature"),
                           "Description" = c("Distance of HDB apartment to nearest feature.",
                                             "Number of features within x radius of HDB apartment.")
                           )
gwrDataOutputdesc <- data_frame("Name" = c("yhat", "Intercept", "_Coef", "_TV", "_PV"),
                                "Description" = c("Estimated y values based on the GWR model.",
                                                  "Model's intercept estimate.",
                                                  "Mode's coefficient estimate for the given independent variable.",
                                                  "The t-value of the relevant estimate.",
                                                  "The p-value of the relevant estimate.")
                                )
mixedgwrDataOutputdesc <- data_frame("Suffix" = c("_Coef", "_L", "_F"),
                                     "Description" = c("Coefficient estimate of observation, based on Mixed GWR model.",
                                                       "Variable identified as local/non-global.",
                                                       "Variable identified as fixed/global.")
                                     )

bvColors <- c("#f3f3f3","#c2f0ce","#8ae1ae",
              "#eac5dd","#9ec5d3","#7ec5b1",
              "#e6a2d0","#bb9fce","#7a8eae")

bivariate_choropleth <- function (
  
  # Function parameters
  bivmap_dataset,         # A SpatialPoligonDataFrame
  bivmap_vars,            # A vector of characters containing the name of the two variables
  bivmap_labels=NA,       # A vector of characters containing the labels for the two variables, to use in the legend
  bivmap_style='quantile',# Classification type for the bins
  bivmap_scale=FALSE      # Use a scale bar
  
) {
  
  # Create the bivatiate map
  bivmap <- get_bivariate_choropleth(
    # Passs parameters on
    # except labels
    bivmap_dataset,
    bivmap_vars,
    bivmap_style,
    bivmap_scale
  )
  
  if (is.na(bivmap_labels)){
    bivmap_labels <- bivmap_vars
  }
  
  # Print map
  suppressWarnings(print( bivmap ))
  
  # Create the square legend
  vp <- viewport(x=.9, y=.15, width=.3, height=.3)
  pushViewport(vp)
  print(levelplot(
    matrix(1:9, nrow=3), 
    axes=FALSE, 
    col.regions=bvColors,
    xlab=list(label=bivmap_labels[1],cex=0.8), 
    ylab=list(label=bivmap_labels[2],cex=0.8), 
    cuts=8, 
    colorkey=FALSE,
    scales=list(draw=0)),
    newpage=FALSE)
  
  # Pop viewport
  popViewport()
}

# This function actually creates the bivariate map using tmap

get_bivariate_choropleth <- function (
  
  # Function parameters
  bivmap_dataset,         # A SpatialPoligonDataFrame
  bivmap_vars,            # A vector of characters containing the name of the two variables
  bivmap_style='quantile',# Classification type for the bins
  bivmap_scale=FALSE      # Use a scale bar
  
) {
  
  
  # Extract the two specified colums
  # excluding rows with na and infinite values

  bivmap_sdf <- bivmap_dataset[, bivmap_vars]
  
  # Renaming the variables to simplify the code below
  colnames(bivmap_sdf@data) <- c("xvar","yvar")
  
  # Create the 3-class categorization per each variable
  bivmap_sdf$xcat <- findCols(classIntervals( bivmap_sdf$xvar, n=3, bivmap_style))
  cat(bivmap_vars[1], "breaks (x-axis):\n")
  print(classIntervals( bivmap_sdf$xvar, n=3, bivmap_style))
  #
  bivmap_sdf$ycat <- findCols(classIntervals( bivmap_sdf$yvar, n=3, bivmap_style))
  cat(bivmap_vars[2], "breaks (y-axis):\n")
  print(classIntervals( bivmap_sdf$yvar, n=3, bivmap_style))
  
  # Combine the above into one 9-class categorization
  bivmap_sdf$bicat <- bivmap_sdf$xcat + (3 * (bivmap_sdf$ycat - 1))
  
  bivmap_sdf$bicol <- bvColors[bivmap_sdf$bicat]
  bivmap_sdf$bicol <- ifelse(is.na(bivmap_sdf$bicol), "#bdbdbd", bivmap_sdf$bicol)
  
  # Double-check created datasets if necessary
  #View(bivmap_sdf@data)
  #View(cbind(bivmap_sdf@data, bivmap_dataset@data))
  
  # Create the map
  bivmap <- tm_shape(bivmap_sdf) + 
    # Fill
    tm_fill(
      "bicol") +
    # Remove frame
    tm_layout(frame=FALSE) +
    # Add rhe legend
    tm_legend(scale=0.75)
  
  if (bivmap_scale) {
    bivmap <- bivmap  +
      # Add scale bar
      tm_scale_bar(
        width=0.30,
        position=c("left","bottom"))
  }
  
  # Return bivariate map
  bivmap
  
}