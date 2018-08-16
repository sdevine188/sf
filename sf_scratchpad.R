library(tidyverse)
library(rgeos)
library(maptools)
library(rgdal)
library(sf)
library(viridis)
library(rmapshaper)

options(scipen = 999)

setwd("C:/Users/Stephen/Desktop/R/sf")
setwd("H:/R/sf")

# sf docs
# https://r-spatial.github.io/sf/
# https://r-spatial.github.io/sf/articles/sf3.html#affine-transformations
# https://geocompr.robinlovelace.net/transform.html

# read in census tiger line shapefile for states
# data can be downloaded manually using tiger web interface, or using tiger ftp (like urban institute), or tigris

# https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
# https://www.census.gov/cgi-bin/geo/shapefiles/index.php
# https://www2.census.gov/geo/tiger/GENZ2017/shp/

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
counties_sf_transformed %>% nrow()
counties_sf_transformed <- counties_sf_transformed %>% mutate(county_variable = runif(n = 3142, min = 0, max = 100))
glimpse(counties_sf_transformed)


####################################################################################


# simplify states_sf_transformed to avoid excessive boundary plotting along coasts, etc
# https://gis.stackexchange.com/questions/243569/simplify-polygons-of-sf-object
# https://github.com/r-spatial/sf/issues/381
# convert sf to shapefile so i can use rmapshaper's simplify function, then convert back to sf
states_sf_transformed_simplified <- ms_simplify(input = as(states_sf_transformed, 'Spatial')) %>% st_as_sf()
states_sf_transformed_simplified


#####################


# simplify counties_sf_transformed to avoid excessive boundary plotting along coasts, etc
counties_sf_transformed_simplified <- ms_simplify(input = as(counties_sf_transformed, 'Spatial')) %>% st_as_sf()
counties_sf_transformed_simplified


####################################################################################



# plot states_map
# to remove gridlines with geom_sf, need to set panel.grid.major color to transparent
# https://github.com/tidyverse/ggplot2/issues/2071

# fivethirtyeight example
# https://projects.fivethirtyeight.com/mortality-rates-united-states/mental-substance/

states_map <- states_sf_transformed_simplified %>% 
        st_transform(102003) %>%
        ggplot() + geom_sf(aes(geometry = geometry, fill = Murder), color = "white") + coord_sf(crs = st_crs(102003)) +
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
ggsave(filename = "states_map.pdf", plot = , width = 5, height = 5, units = "in", dpi = 300)


####################################################################################


# plot counties_map
# to remove gridlines with geom_sf, need to set panel.grid.major color to transparent
# https://github.com/tidyverse/ggplot2/issues/2071
counties_map <- counties_sf_transformed_simplified %>% 
        st_transform(102003) %>%
        ggplot() + geom_sf(aes(geometry = geometry, fill = county_variable)) + coord_sf(crs = st_crs(102003)) +
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
                text = element_blank()) + ggtitle("U.S. county_variable by county")

counties_map


# save map
ggsave(filename = "counties_map.pdf", plot = , width = 5, height = 5, units = "in", dpi = 300)


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
              text = element_blank()) + ggtitle("not_simplified, .05 and .5")

state_counties_map

# save map
ggsave(filename = "state_counties_map_not_simplified.pdf", plot = state_counties_map, 
       height = 11, width = 8.5, units = "in", dpi = 300)


##################################################################################


# plot state_counties_simplified_map
counties_sf_transformed_simplified2 <- counties_sf_transformed_simplified %>% st_transform(102003)

state_counties_simplified_map <- states_sf_transformed_simplified %>% 
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
              text = element_blank()) + ggtitle("simplified")

state_counties_simplified_map

# save map
ggsave(filename = "state_counties_map_simplified.pdf", plot = state_counties_map, 
       height = 11, width = 8.5, units = "in", dpi = 300)