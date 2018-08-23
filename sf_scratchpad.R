library(tidyverse)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(purrr)
library(rgeos)
library(maptools)
library(rgdal)
library(sf)
library(viridis)
library(rmapshaper)
library(httr)
library(jsonlite)
library(broom)

options(scipen = 999)

setwd("C:/Users/Stephen/Desktop/R/sf")
setwd("H:/R/sf")

# sf docs
# https://r-spatial.github.io/sf/
# https://r-spatial.github.io/sf/articles/sf3.html#affine-transformations
# https://geocompr.robinlovelace.net/transform.html

# read in census tiger line shapefile for states
# data can be downloaded manually using tiger web interface, or using tiger ftp (like urban institute), or tigris
# for best maps, use 5m tiger line

# this page has link to ftp (blocked by dhs) and web interface (limited download options)
# https://www.census.gov/geo/maps-data/data/tiger-line.html

# this census tiger ftp is not blocked by dhs
# https://www2.census.gov/geo/tiger/GENZ2017/shp/

# tiger gui download interface: https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
# non-tiger cartographic shapefiles (less detailed than tiger): https://www.census.gov/cgi-bin/geo/shapefiles/index.php


#######################################################################################


# download zip folder with shapefile for state 
# https://github.com/UrbanInstitute/urbnmapr/blob/master/data-raw/ccdf-map.R
url <- "http://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_state_5m.zip"
temp <- tempfile(fileext = ".zip")
download.file(url, temp)
unzip(temp, exdir = "states_tiger_shapefiles_ftp")

# read in shapefile as sf
states_sf <- st_read("states_tiger_shapefiles_ftp/cb_2017_us_state_5m.shp")
states_sf
glimpse(states_sf)

# read in shapefile manually downloaded from census tiger web interface (it's 5k resolution, so not very detailed)
# states_sf <- st_read("state_tiger_shapefiles/tl_2017_us_state.shp")
# states_sf
# glimpse(states_sf)

# set coordinate reference system to US National Atlas Equal Area (EPSG 2153)
# when we plot the map at the end, we'll use USA Contiguous Albers Equal Area Conic (EPSG: 102003)
# even though the map will ultimately use 102003, we need to capture hawaii/alaska using 2163 to avoid distortions
# because i think 102003 focuses on projecting conus only
states_sf <- states_sf %>% 
        st_transform(2163) %>%
        identity()


######################################################################################


# download zip folder with shapefile for counties
# https://github.com/UrbanInstitute/urbnmapr/blob/master/data-raw/ccdf-map.R
url <- "http://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_county_5m.zip"
temp <- tempfile(fileext = ".zip")
download.file(url, temp)
unzip(temp, exdir = "counties_tiger_shapefiles_ftp")

# read in shapefile as sf
counties_sf <- st_read("counties_tiger_shapefiles_ftp/cb_2017_us_county_5m.shp")
counties_sf
glimpse(counties_sf)

# set coordinate reference system to US National Atlas Equal Area (EPSG 2153)
# when we plot the map at the end, we'll use USA Contiguous Albers Equal Area Conic (EPSG: 102003)
# even though the map will ultimately use 102003, we need to capture hawaii/alaska using 2163 to avoid distortions
# because i think 102003 focuses on projecting conus only
counties_sf <- counties_sf %>% 
        st_transform(2163) %>%
        identity()


########################################################################


# cbsa points
# download zip folder with shapefile for cbsa
# https://github.com/UrbanInstitute/urbnmapr/blob/master/data-raw/ccdf-map.R
url <- "http://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_county_5m.zip"
temp <- tempfile(fileext = ".zip")
download.file(url, temp)
unzip(temp, exdir = "cbsa_tiger_shapefiles_ftp")

# read in shapefile as sf
cbsa_sf <- st_read("cbsa_tiger_shapefiles_ftp/cb_2017_us_cbsa_5m.shp")
cbsa_sf
glimpse(cbsa_sf)
class(cbsa_sf)

# split city/state into two variables
cbsa_sf <- cbsa_sf %>% mutate(city = str_replace(string = NAME, pattern = regex(",.*"), replacement = ""),
                              state = str_replace(string = NAME, pattern = regex("^.*, "), replacement = ""))
cbsa_sf %>% as_tibble() %>% distinct(state) %>% data.frame()
cbsa_sf %>% as_tibble() %>% distinct(city) 

# set coordinate reference system to US National Atlas Equal Area (EPSG 2153)
# when we plot the map at the end, we'll use USA Contiguous Albers Equal Area Conic (EPSG: 102003)
# even though the map will ultimately use 102003, we need to capture hawaii/alaska using 2163 to avoid distortions
# because i think 102003 focuses on projecting conus only
cbsa_sf <- cbsa_sf %>% 
        st_transform(2163) %>%
        identity()


###########################################################################


# world shapefiles from naturalearth - this is the best option for world maps
# https://hub.arcgis.com/datasets/a21fdb46d23e4ef896f31475217cbb08_1
# https://www.programmableweb.com/news/how-to-access-any-restful-api-using-r-language/how-to/2017/07/21

# download zip file of shapefiles
# had to download manually - got 403 Forbidden error when trying to download via R
url <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_sovereignty.zip"
# temp <- tempfile(fileext = ".zip")
# download.file(url, temp)
# unzip(temp, exdir = "world_shapefiles_arcgis_uia")

# read in shapefile as sf
# using epsg: 4326 by default
world_ne_sovereign_sp <- readOGR(dsn = "./world_shapefiles_naturalearth/sovereignty", layer = "ne_50m_admin_0_sovereignty")
# head(world_ne_sovereign_sp)
glimpse(world_ne_sovereign_sp)
class(world_ne_sovereign_sp)
world_ne_sovereign_sp 

# get data from shapefile
world_ne_sovereign_sp_data <- world_ne_sovereign_sp@data
glimpse(world_ne_sovereign_sp_data)
class(world_ne_sovereign_sp_data)
world_ne_sovereign_sp_data %>% distinct(NE_ID) %>% nrow()

# use tidy from broom to convert shapefile into a df (tidy is the replacement for deprecated fortify function)
# note that tidy drops other non-map variables in shapefile, and assigns "id" var starting with 0, in order of regions in shapefile
world_ne_sovereign <- tidy(world_ne_sovereign_sp)
head(world_ne_sovereign)
glimpse(world_ne_sovereign)
class(world_ne_sovereign)

# add back the non-map variables from the shapefile that were dropped when running tidy function
# add id variable to sp_data to use for join with tidy sp_data (relying on fact that tidy assigns "id" var in order of regions in shapefile)
# https://stackoverflow.com/questions/40576457/keep-region-names-when-tidying-a-map-using-broom-package
world_ne_sovereign_sp_data <- world_ne_sovereign_sp_data %>% mutate(id = as.character(seq(from = 0, to = nrow(.) - 1, by = 1)))
glimpse(world_ne_sovereign_sp_data)

# join sp_data with tidy sp data by id
world_ne_sovereign <- world_ne_sovereign %>% left_join(., world_ne_sovereign_sp_data, by = "id")
glimpse(world_ne_sovereign)


#########################################################################


# world shapefiles from arcgis - these are not very detailed shapefiles, better to use natural earth
# https://hub.arcgis.com/datasets/a21fdb46d23e4ef896f31475217cbb08_1
# https://www.programmableweb.com/news/how-to-access-any-restful-api-using-r-language/how-to/2017/07/21

# download zip file of shapefiles
url <- "https://opendata.arcgis.com/datasets/252471276c9941729543be8789e06e12_0.zip"
temp <- tempfile(fileext = ".zip")
download.file(url, temp)
unzip(temp, exdir = "world_shapefiles_arcgis")

# read in shapefile as sf
# using epsg: 4326 by default
world_sf <- st_read("world_shapefiles_arcgis/UIA_World_Countries_Boundaries.shp")
world_sf
glimpse(world_sf)
class(world_sf)


#########################################################################


# transform points for non-conus geographies

# create rotation function for use in transform_states function
rotation = function(a){
        r = a * pi/180 #degrees to radians
        matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
} 

# create transform_states function
# for shift, format is c(horizontal (more negative is west), vertical (more negative is south))
transform_states <- function(points_sf, shift = c(0, 0), scale = 1, rotate_degrees = 0) {
        
        # get original_crs to make sure the output has same crs
        original_crs <- st_crs(points_sf)
        
        # get geometry for points
        points_transformed_sfc <- st_geometry(points_sf$geometry)
        
        # get initial centroid for points
        points_centroid_sfc <- st_centroid(points_transformed_sfc)
        
        # transform points
        points_transformed_sfc <- (points_transformed_sfc - points_centroid_sfc) * scale * rotation(rotate_degrees) + 
                shift + points_centroid_sfc
        
        # replace geometry of points_sf with the transformed points
        points_sf_transformed <- st_set_geometry(points_sf, points_transformed_sfc)
        
        # ensure original_crs is retained
        points_sf_transformed <- points_sf_transformed %>% st_set_crs(original_crs) %>% st_transform(original_crs)
        
        # return points_sf_transformed
        points_sf_transformed
}

# test transform_states
# points_sf <- states_sf %>% filter(NAME == "California")
# scale <- .3
# shift <- c(100, 100)
# rotate_degrees <- 90
# points_sf %>% transform_states(points_sf = ., scale = scale, shift = shift, rotate_degrees = rotate_degrees) %>%
#         ggplot() + geom_sf()


#########################


# create transform_counties function
# for shift, format is c(horizontal (more negative is west), vertical (more negative is south))
# note that transform_counties gets state centroid from states_sf
transform_counties <- function(points_sf, state_name, shift = c(0, 0), scale = 1, rotate_degrees = 0) {
        
        # get state centroid
        state_centroid <- states_sf %>% filter(NAME == state_name) %>% st_geometry() %>% st_centroid()
        
        # get original_crs to make sure the output has same crs
        original_crs <- st_crs(points_sf)
        
        # get geometry for points
        points_transformed_sfc <- st_geometry(points_sf$geometry)
        
        # transform points
        points_transformed_sfc <- (points_transformed_sfc - state_centroid) * scale * rotation(rotate_degrees) + 
                shift + state_centroid
        
        # replace geometry of points_tbl with the transformed points
        points_sf_transformed <- st_set_geometry(points_sf, points_transformed_sfc)
        
        # ensure original_crs is retained
        points_sf_transformed <- points_sf_transformed %>% st_set_crs(original_crs) %>% st_transform(original_crs)
        
        # return points_sf_transformed
        points_sf_transformed
}

# test transform_counties function
# points_sf <- counties_sf %>% filter(STATEFP == "06")
# state_name <- "California"
# scale <- 1
# shift <- c(0, 0)
# rotate_degrees <- 90
# points_sf %>% transform_counties(points_sf = ., state_name = "California", scale = scale, shift = shift, rotate_degrees = rotate_degrees) %>%
#         ggplot() + geom_sf()


##############################################################################################


# transform alaska state
alaska_state <- states_sf %>% filter(NAME == "Alaska") %>%
        # st_crop(c(xmin = -4000000, ymin = 1466024, xmax = -1512211, ymax = 3909687)) %>%
        transform_states(points_sf = ., shift = c(820000, -4730000), scale = .35, rotate_degrees = -50) %>%
        identity()
alaska_state

# alaska_state %>% ggplot() + geom_sf()


#####################


# transform alaska counties
alaska_counties <- counties_sf %>% filter(STATEFP == "02") %>%
        # st_crop(c(xmin = -4000000, ymin = 1466024, xmax = -1512211, ymax = 3909687)) %>%
        transform_counties(points_sf = ., state_name = "Alaska", shift = c(820000, -4730000), scale = .35, rotate_degrees = -50) %>%
        identity()
alaska_counties

# alaska_counties %>% ggplot() + geom_sf()


####################


# transform hawaii state
hawaii_state <- states_sf %>% filter(NAME == "Hawaii") %>%
        # st_crop(c(xmin = -6000000, ymin = -4000000, xmax = 4000000, ymax = -400000)) %>%
        transform_states(points_sf = ., shift = c(5000000, -1150000), scale = 1, rotate_degrees = -35) %>%
        identity()
hawaii_state

# hawaii_state %>% ggplot() + geom_sf()


#####################


# transform hawaii counties
hawaii_counties <- counties_sf %>% filter(STATEFP == "15") %>%
        # st_crop(c(xmin = -6000000, ymin = -4000000, xmax = 4000000, ymax = -400000)) %>%
        transform_counties(points_sf = ., state_name = "Hawaii", shift = c(5000000, -1150000), scale = 1, rotate_degrees = -35) %>%
        identity()
hawaii_counties

# hawaii_counties %>% ggplot() + geom_sf()


##################################################################################


# combine transformed state geographies
states_sf_transformed <- states_sf %>% filter(!(NAME %in% c("Alaska", "Hawaii"))) %>%
        rbind(alaska_state) %>%
        rbind(hawaii_state) %>%
        identity()

# remove unused geographies
states_sf_transformed <- states_sf_transformed %>% filter(!(NAME %in% c("American Samoa", "Guam", 
                                                                        "Commonwealth of the Northern Mariana Islands", "United States Virgin Islands", "Puerto Rico")))

# states_sf_transformed %>% ggplot() + geom_sf()


######################


# combine transformed county geographies
counties_sf_transformed <- counties_sf %>% filter(!(STATEFP %in% c("02", "15"))) %>%
        rbind(alaska_counties) %>%
        rbind(hawaii_counties) %>%
        identity()

# remove unused geographies
counties_sf_transformed <- counties_sf_transformed %>% filter(!(STATEFP %in% c("60", "66", 
                                                                               "69", "78", "72")))

# counties_sf_transformed %>% ggplot() + geom_sf()


####################################################################################


# add state data of interest
data(USArrests)
arrests <- USArrests
arrests <- arrests %>% mutate(state = row.names(arrests)) %>% as_tibble()
arrests

# join with states_sf_tranformed
# first convert NAME var in states_sf_tranformed to character instead of factor to avoid warning when joining
states_sf_transformed <- states_sf_transformed %>% mutate(NAME = as.character(NAME))
states_sf_transformed <- states_sf_transformed %>% left_join(., arrests, by = c("NAME" = "state"))
glimpse(states_sf_transformed)


####################


# add county data of interest
counties_sf_transformed <- counties_sf_transformed %>% mutate(county_variable = runif(n = nrow(.), min = 0, max = 100))
glimpse(counties_sf_transformed)


#####################


# add cbsa variable of interest
cbsa_sf <- cbsa_sf %>% mutate(cbsa_variable = runif(n = nrow(.), min = 0, max = 100))
glimpse(cbsa_sf)


#####################


# add world variable of interest
world_sf <- world_sf %>% mutate(world_variable = runif(n = nrow(.), min = 0, max = 100))
glimpse(world_sf)
world_sf %>% select(Country) %>% arrange(Country)

# add world_uia variable of interest
world_uia_sf <- world_uia_sf %>% mutate(world_variable = runif(n = nrow(.), min = 0, max = 100))
glimpse(world_uia_sf)
world_uia_sf %>% select(Country) %>% arrange(Country)


####################################################################################


# simplify states_sf_transformed to avoid excessive boundary plotting along coasts, etc
# https://gis.stackexchange.com/questions/243569/simplify-polygons-of-sf-object
# https://github.com/r-spatial/sf/issues/381
# convert sf to shapefile so i can use rmapshaper's simplify function, then convert back to sf
# rmapshaper properly handles borders comprehensively, as opposed to simplifying based on each polygon taken independently like sf's st_simplify()
# st_simplify can thus leave jagged gaps where one polygon got simplified in a certain way, but the adjacent polygon did not, so the borders are no longer flush
# states_sf_transformed_simplified <- ms_simplify(input = as(states_sf_transformed, 'Spatial')) %>% st_as_sf()
# states_sf_transformed_simplified


#####################


# simplify counties_sf_transformed to avoid excessive boundary plotting along coasts, etc
# counties_sf_transformed_simplified <- ms_simplify(input = as(counties_sf_transformed, 'Spatial')) %>% st_as_sf()
# counties_sf_transformed_simplified


####################################################################################



# plot states_map
# to remove gridlines with geom_sf, need to set panel.grid.major color to transparent
# https://github.com/tidyverse/ggplot2/issues/2071

# choropleth examples for style/color
# https://projects.fivethirtyeight.com/mortality-rates-united-states/mental-substance/
# https://rud.is/b/2017/09/18/mapping-fall-foliage-with-sf/
# http://strimas.com/r/ebird-county/

states_map <- states_sf_transformed %>% 
        st_transform(102003) %>%
        ggplot() + geom_sf(aes(geometry = geometry, fill = Murder), color = "white", size = .125) + coord_sf(crs = st_crs(102003)) +
        scale_fill_viridis_c() + theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              plot.title = element_text(size = 12, face = "bold", hjust = .5), legend.position = "right",
              legend.key.size = unit(2, "mm"), legend.title = element_text(size = 7),
              legend.text = element_text(size = 7),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + ggtitle("U.S. murder rate by state")

states_map


# save map
ggsave(filename = "states_map.pdf", plot = states_map, width = 8.5, height = 11, units = "in", dpi = 300)


####################################################################################


# plot counties_map
# to remove gridlines with geom_sf, need to set panel.grid.major color to transparent
# https://github.com/tidyverse/ggplot2/issues/2071
counties_map <- counties_sf_transformed %>% 
        st_transform(102003) %>%
        ggplot() + geom_sf(aes(geometry = geometry, fill = county_variable), color = "#a6a6a6", size = .05) + 
        coord_sf(crs = st_crs(102003)) + scale_fill_viridis_c() + theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              plot.title = element_text(size = 12, face = "bold", hjust = .5), legend.position = "right",
              legend.key.size = unit(2, "mm"), legend.title = element_text(size = 7),
              legend.text = element_text(size = 7),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + ggtitle("U.S. county_variable by county")

counties_map


# save map
ggsave(filename = "counties_map.pdf", plot = counties_map, width = 8.5, height = 11, units = "in", dpi = 300)


####################################################################################


# plot state_counties_map
counties_sf_transformed2 <- counties_sf_transformed %>% st_transform(102003)

state_counties_map <- states_sf_transformed %>% 
        st_transform(102003) %>%
        ggplot() +  
        geom_sf(data = counties_sf_transformed2, aes(geometry = geometry, fill = county_variable), 
                color = "#a6a6a6", size = 0.05) +
        geom_sf(aes(geometry = geometry), color = "white", size = 0.125, alpha = 0) +
        coord_sf(crs = st_crs(102003)) +
        scale_fill_viridis_c() + theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              plot.title = element_text(size = 12, face = "bold", hjust = .5), legend.position = "right",
              legend.key.size = unit(2, "mm"), legend.title = element_text(size = 7),
              legend.text = element_text(size = 7),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + ggtitle("county_variable by county")

state_counties_map

# save map
ggsave(filename = "state_counties_map.pdf", plot = state_counties_map, 
       height = 11, width = 8.5, units = "in", dpi = 300)


##################################################################################


# plot state_counties_simplified_map
# counties_sf_transformed_simplified2 <- counties_sf_transformed_simplified %>% st_transform(102003)
# 
# state_counties_simplified_map <- states_sf_transformed_simplified %>% 
#         st_transform(102003) %>%
#         ggplot() +  
#         geom_sf(data = counties_sf_transformed2, aes(geometry = geometry, fill = county_variable), 
#                 color = "#a6a6a6", size = 0.05) +
#         geom_sf(aes(geometry = geometry), color = "white", size = 0.125, alpha = 0) +
#         coord_sf(crs = st_crs(102003)) +
#         scale_fill_viridis_c() + theme_bw() +
#         theme(panel.grid.major = element_line(color = "transparent"),
#               plot.background = element_blank(), 
#               panel.grid.minor = element_blank(), panel.border = element_blank(),
#               axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
#               axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
#               plot.title = element_text(size = 12, face = "bold", hjust = .5), legend.position = "right",
#               legend.key.size = unit(2, "mm"), legend.title = element_text(size = 7),
#               legend.text = element_text(size = 7),
#               panel.grid = element_blank(),
#               line = element_blank(),
#               rect = element_blank(),
#               text = element_blank()) + ggtitle("simplified")
# 
# state_counties_simplified_map
# 
# # save map
# ggsave(filename = "state_counties_map_simplified.pdf", plot = state_counties_map, 
#        height = 11, width = 8.5, units = "in", dpi = 300)


#######################################################################################


# plot cbsa_map
# https://www2.census.gov/geo/maps/metroarea/stcbsa_pg/Feb2013/cbsa2013_CA.pdf
# https://stackoverflow.com/questions/44678978/how-to-label-an-individual-state-on-the-map-while-the-others-at-sub-divisional-l
# https://ggplot2.tidyverse.org/reference/geom_text.html
# https://stackoverflow.com/questions/49555694/size-legend-of-sf-object-wont-show-correct-symbols
# https://ggplot2.tidyverse.org/reference/scale_size.html

# add cbsa centroid lat/lon to use for labeling cbsa
cbsa_sf <- cbsa_sf %>% mutate(centroid = st_centroid(geometry),
                              lon = unlist(map(.x = .$centroid, .f = ~ st_coordinates(.x)[[1]])),
                              lat = unlist(map(.x = .$centroid, .f = ~ st_coordinates(.x)[[2]])))

# plot
cbsa_map <- cbsa_sf %>% filter(state == "CA") %>% 
        st_transform(102003) %>%
        mutate(centroid = st_centroid(geometry)) %>%
        mutate(lon = unlist(map(.x = .$centroid, .f = ~ st_coordinates(.x)[[1]])),
               lat = unlist(map(.x = .$centroid, .f = ~ st_coordinates(.x)[[2]]))) %>%
        ggplot() +  
        geom_sf(data = states_sf_transformed[states_sf_transformed$STATEFP == "06", ], 
                aes(geometry = geometry), color = "black", size = 0.125, fill = NA) +
        geom_sf(aes(geometry = centroid, color = cbsa_variable, size = ALAND), show.legend = "point") + scale_size(range = c(5, 15)) +
        geom_text(aes(label = city, x = lon, y = lat), check_overlap = TRUE, fontface = "bold") + coord_sf(crs = st_crs(102003)) + 
        scale_color_viridis_c() + 
        theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              plot.title = element_text(size = 12, face = "bold", hjust = .5), legend.position = "right",
              legend.key.size = unit(2, "mm"), legend.title = element_text(size = 7),
              legend.text = element_text(size = 7),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + ggtitle("cbsa")

cbsa_map

# save map
ggsave(filename = "cbsa_map.pdf", plot = cbsa_map, 
       height = 15, width = 12, units = "in", dpi = 300)


####################################################################################


# world map
world_map <- world_sf %>% 
        filter(Country != "Antarctica") %>%
        ggplot() + geom_sf(aes(geometry = geometry, fill = world_variable), color = "white", size = .125) + 
        scale_fill_viridis_c() + theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              plot.title = element_text(size = 12, face = "bold", hjust = .5), legend.position = "right",
              legend.key.size = unit(2, "mm"), legend.title = element_text(size = 7),
              legend.text = element_text(size = 7),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + ggtitle("world map")

world_map

# save map
ggsave(filename = "world_map.pdf", plot = world_map, units = "in", dpi = 300)


#######################################################################################

# https://github.com/r-spatial/sf/issues/509
# https://bookdown.org/robinlovelace/geocompr/transform.html

# world_uia map
world_uia_map <- world_uia_sf %>% 
        ggplot() + geom_sf(aes(geometry = geometry, fill = world_variable), color = "white", size = .125) + 
        scale_fill_viridis_c() + theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              plot.title = element_text(size = 12, face = "bold", hjust = .5), legend.position = "right",
              legend.key.size = unit(2, "mm"), legend.title = element_text(size = 7),
              legend.text = element_text(size = 7),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + ggtitle("world map")

world_uia_map

# save map
ggsave(filename = "world_uia_map.pdf", plot = world_uia_map, units = "in", dpi = 300)


#####################################################################################


north_america_map <- world_sf %>% 
        filter(Country %in% c("Canada", "United States", "Mexico")) %>%
        ggplot() + geom_sf(aes(geometry = geometry, fill = world_variable), color = "white", size = .125) + 
        scale_fill_viridis_c() + theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              plot.title = element_text(size = 12, face = "bold", hjust = .5), legend.position = "right",
              legend.key.size = unit(2, "mm"), legend.title = element_text(size = 7),
              legend.text = element_text(size = 7),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + ggtitle("north america map")

north_america_map

# save map
ggsave(filename = "north_america_map.pdf", plot = north_america_map, units = "in", dpi = 300)


#######################################################################################


asia_map <- world_sf %>% 
        filter(Country %in% c("India", "China", "Nepal", "Bhutan", "Pakistan", "Afghanistan")) %>%
        ggplot() + geom_sf(aes(geometry = geometry, fill = world_variable), color = "white", size = .125) + 
        scale_fill_viridis_c() + theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              plot.title = element_text(size = 12, face = "bold", hjust = .5), legend.position = "right",
              legend.key.size = unit(2, "mm"), legend.title = element_text(size = 7),
              legend.text = element_text(size = 7),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + ggtitle("asia map")

asia_map

# save map
ggsave(filename = "asia_map.pdf", plot = asia_map, units = "in", dpi = 300)


#####################################################################################


# world map using winkel tripel projection

# geom_sf has difficult time getting winkel tripel
# here are notes on a workaround, but they didn't work for me
# https://github.com/r-spatial/sf/issues/509

# need to use shapefile that is converted to df by broom::tidy, then use bob rudis' ggalt::geom_cartogram and ggalt::coord_proj
# https://rud.is/b/2018/01/18/bitcoin-world-map-bubbles/

# this map also uses naturalearth 50m shapefile, which is better than arcgis
# ggplot2 has map_data function built-in, which in turn gets data from the maps package
# https://www.rdocumentation.org/packages/ggplot2/versions/3.0.0/topics/map_data

# the maps package in turn gets world shapefile from natural earth project (see pg. 34 of docs)
# NE is supported by North American Cartographic Information Society (see NE homepage)
# https://cran.r-project.org/web/packages/maps/maps.pdf

# rnaturalearth has function to fetch ne shapefiles, but you can go direct to website
# https://cran.r-project.org/web/packages/rnaturalearth/vignettes/rnaturalearth.html
# https://www.naturalearthdata.com/downloads/50m-cultural-vectors/

# as can be seen on rnaturalearth github code, their ne_download function is just a wrapper to hit NE zip download urls
# https://github.com/ropensci/rnaturalearthdata/blob/master/data-raw/data_download_script.r
# https://github.com/ropensci/rnaturalearth/blob/master/R/ne_download.r
# https://github.com/ropensci/rnaturalearth/blob/master/R/ne_file_name.r

# note that NE has shapefiles for sovereign states (~197), but also countries (including breakaway provinces, etc ~ 247)
# sovereign states issue passports - more details in "About" section beneath each download option on NE website

# drop antarctica and add world variable
world_ne_sovereign_transformed <- world_ne_sovereign %>% filter(NAME != "Antarctica") %>% 
        mutate(world_var = runif(n = nrow(.), min = 0, max = 1))

world_ne_sovereign_map <-  ggplot() +
        geom_cartogram(data = world_ne_sovereign_transformed, map = world_ne_sovereign_transformed, 
                       aes(x = long, y = lat, map_id = id, fill = world_var), color = "#a6a6a6", size = 0.05) +
        scale_fill_viridis_c() + coord_proj("+proj=wintri") +
        theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              plot.title = element_text(size = 12, face = "bold", hjust = .5), legend.position = "right",
              legend.key.size = unit(2, "mm"), legend.title = element_text(size = 7),
              legend.text = element_text(size = 7),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + ggtitle("naturalearth world map w winkel tripel")

world_ne_sovereign_map

# save map
ggsave(filename = "world_ne_sovereign_map.pdf", plot = world_ne_sovereign_map, units = "in", dpi = 300)


##############################################################################################



