library(sf) # note sf should be loaded before dplyr/tidyverse, or it seems to give error regarding group_map()
library(tidyverse)
library(lubridate)
library(flextable)
library(officer)
library(fs)
library(janitor)
library(scales)
library(testthat)
library(extrafont)
library(magick)
library(pdftools)
library(rgeos)
library(maptools)
library(rgdal)
library(viridis)
library(rmapshaper)
library(broom)
library(ggalt)
library(forcats)


# load validate_anumbers function
current_wd <- getwd()
setwd("H:/R/helper_scripts")
source("validate_anumbers.R")
setwd(current_wd)

# load get_invalid_anumbers function
current_wd <- getwd()
setwd("H:/R/helper_scripts")
source("get_invalid_anumbers.R")
setwd(current_wd)

# load as_percent()
current_wd <- getwd()
setwd("H:/R/helper_scripts")
source("as_percent.R")
setwd(current_wd)

# load add_totals_row()
current_wd <- getwd()
setwd("H:/R/helper_scripts")
source("add_totals_row.R")
setwd(current_wd)

# load save_flextable()
current_wd <- getwd()
setwd("H:/R/helper_scripts")
source("save_flextable.R")
setwd(current_wd)

# load ggplot_center_legend_title function
current_wd <- getwd()
setwd("H:/R/helper_scripts")
source("ggplot_center_legend_title.R")
setwd(current_wd)


options(scipen=999)

# setwd
setwd("H:/RED/CIGP")
setwd("C:/users/sjdevine/Work Folders/Desktop/cigp")


##################################################################################


# load natural earth maps
# see sf_scratchpad.R on github and in sf folder

# load natuaral earth world map
setwd("H:/R/sf")

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


##########################################################################


# join world_ne_sovereign with data of interest
fake_data <- world_ne_sovereign %>% distinct(SOVEREIGNT) 
fake_data <- fake_data %>% mutate(n = sample(x = seq(from = 0, to = 10000, by = 1), size = nrow(fake_data))) %>%
        mutate(n = case_when(row_number() %in% 1:20 ~ NA_real_, row_number() %in% 21:40 ~ 5, TRUE ~ n))
fake_data %>% arrange(n)
fake_data %>% arrange(desc(n))
fake_data %>% ggplot(data = ., aes(x = n)) + geom_histogram()

world_ne_sovereign <- world_ne_sovereign %>% left_join(., fake_data, by = "SOVEREIGNT")
world_ne_sovereign %>% glimpse()


###########


# add fill_color_bin and fill_color
world_ne_sovereign <- world_ne_sovereign %>% mutate(fill_color_bin = case_when(is.na(n) ~ "0_to_1",
                                                                               n > 0 & n < 10 ~ "1_to_10",
                                                                               n >= 10 & n < 100 ~ "10_to_100",
                                                                               n >= 100 & n < 1000 ~ "100_to_1000",
                                                                               n >= 1000 & n < 5000 ~ "1000_to_5000",
                                                                               n >= 5000 & n < 10000 ~ "5000_to_10000"),
                                                    fill_color = case_when(fill_color_bin == "0_to_1" ~ "#ffffff",
                                                                           fill_color_bin == "1_to_10" ~ viridis_pal()(5)[1],
                                                                           fill_color_bin == "10_to_100" ~ viridis_pal()(5)[2],
                                                                           fill_color_bin == "100_to_1000" ~ viridis_pal()(5)[3],
                                                                           fill_color_bin == "1000_to_5000" ~ viridis_pal()(5)[4],
                                                                           fill_color_bin == "5000_to_10000" ~ viridis_pal()(5)[5]))

# inspect
world_ne_sovereign %>% count(fill_color_bin, fill_color)
world_ne_sovereign %>% count(n, fill_color_bin, fill_color) %>% arrange(n)
world_ne_sovereign %>% count(n, fill_color_bin, fill_color) %>% arrange(desc(n))


#############


# create fill_color_list for to pass to scale_color_manual
world_fill_color_list <- world_ne_sovereign %>% count(fill_color_bin, fill_color) %>% pull(fill_color)
names(world_fill_color_list) <- world_ne_sovereign %>% count(fill_color_bin, fill_color) %>% pull(fill_color_bin)
world_fill_color_list


############################################################################################


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

# import extra fonts from extrafont package
windowsFonts()
# font_import()
# loadfonts(device = "win")
windowsFonts()

cigp_world_map <- world_ne_sovereign %>% filter(NAME != "Antarctica") %>%
        ggplot() +
        geom_cartogram(map = world_ne_sovereign, 
                       aes(x = long, y = lat, map_id = id, fill = fill_color_bin), color = "#a6a6a6", size = 0.05) +
        scale_fill_manual(values = world_fill_color_list, name = "Number\nof\nCAGP\nparticipants\n",
                          labels = c(1, 10, 100, comma(1000), comma(5000), comma(10000))) +
        coord_proj("+proj=wintri") +
        theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              plot.title = element_text(size = 6, face = "bold", hjust = 0, family = "Source Sans Pro"), legend.position = "right",
              legend.key.size = unit(2, "mm"), legend.title = element_text(size = 4, family = "Source Sans Pro"),
              legend.text = element_text(size = 4, family = "Source Sans Pro"), 
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + ggtitle("                    Figure 2: Participants by country of birth") +
        guides(fill = guide_legend(title.hjust = .5, reverse = TRUE, keyheight = 1, label.vjust = 1.1))

cigp_world_map

# call ggplot_center_legend_title
cigp_world_map_grob <- ggplot_center_legend_title(plot = cigp_world_map)
grid.newpage()
grid.draw(cigp_world_map_grob)

# save map
ggsave(filename = "scratchpad/cigp_world_map.pdf", plot = cigp_world_map_grob, units = "in", dpi = 300, device = cairo_pdf)


#########


# convert pdf to png with magick and pdftools, then crop

# read pdf
cigp_world_map_pdf <- pdf_render_page(pdf = "scratchpad/cigp_world_map.pdf", page = 1, dpi = 300)

# convert to magick image
cigp_world_map_image <- image_read(cigp_world_map_pdf)

# crop
image_info(cigp_world_map_image)
cigp_world_map_image <- image_crop(image = cigp_world_map_image, geometry = geometry_area(width = 1370, height = 750, x_off = 180, y_off = 450))


################


# add 0 tick mark to legend

# get color_list for manual legend
starwars2 <- starwars %>% mutate(fill_color = ifelse(species == "Human", "green", "blue"), 
                                 fill_color_bin = ifelse(species == "Human", "color_green", "color_blue"))
starwars_color_list <- starwars2 %>% pull(fill_color)
names(starwars_color_list) <- starwars2 %>% pull(fill_color_bin) 
starwars_color_list

# create plot with manual legend
starwars_plot <- starwars2 %>% ggplot(data = ., aes(x = mass, fill = fill_color_bin)) + geom_bar() +
        scale_fill_manual(values = starwars_color_list, labels = c(0, 100, 1000)) +
        theme(legend.text = element_text(size = 12, family = "Source Sans Pro"))
starwars_plot

# save plot as png
ggsave(filename = "scratchpad/starwars_plot.png", plot = starwars_plot, dpi = 300, width = 15)

# load plot as image
starwars_plot_image <- image_read(path = "scratchpad/starwars_plot.png")
starwars_plot_image
image_info(starwars_plot_image)

# crop image to just the zero
zero_image <- image_crop(image = starwars_plot_image, geometry = geometry_area(width = 50, height = 50, x_off = 4280, y_off = 750))
zero_image
image_info(zero_image)

# save cropped_image
image_write(image = zero_image, path = "scratchpad/zero_image.png", format = 'png', density = '300x300')

# load map as image and add zero to legend
image_info(cigp_world_map_image)
cigp_world_map_image <- image_composite(image = cigp_world_map_image, composite_image = image_resize(image = zero_image, 
                                                                                                     geometry = geometry_size_pixels(width = 17, height = 17)), offset = geometry_point(x = 1242, y = 675))
cigp_world_map_image

# save states_map_image
image_write(image = cigp_world_map_image, path = "scratchpad/cigp_world_map.png", format = "png", density = "300x300")


###########################################################################################
###########################################################################################


# create country_bar_chart


# create country_counts with all countries w < 100 participants lumped in other category
country_counts_large <- world_ne_sovereign %>% distinct(SOVEREIGNT, n) %>% rename(participant_count = n) %>% filter(!is.na(participant_count)) %>% 
        filter(participant_count >= 8000)
country_counts_large

country_counts_small <- world_ne_sovereign %>% distinct(SOVEREIGNT, n) %>% rename(participant_count = n) %>% filter(!is.na(participant_count)) %>%
        filter(participant_count < 8000) %>% mutate(SOVEREIGNT = "Other countries", participant_count = sum(participant_count)) %>% slice(1)
country_counts_small

# note that normall country_counts_small would be left as it is, but since the fake_data has lots of large values for n, 
# this makes the Other category for summing the country_counts_small to be really big like 500,000, dwarfing the bar chart
# so for this example, I'll manually set it to something more appropriate
country_counts_small <- country_counts_small %>% mutate(participant_count = 3000)
country_counts_small

# shrink names so the labels fit better
country_counts <- country_counts_large %>% bind_rows(., country_counts_small) %>%
        mutate(SOVEREIGNT = case_when(SOVEREIGNT == "Democratic Republic of the Congo" ~ "Dem. Rep. of the Congo", 
                                      SOVEREIGNT == "Dominican Republic" ~ "Dominican Rep.", 
                                      SOVEREIGNT == "Myanmar" ~ "Burma", TRUE ~ SOVEREIGNT))

# relevel SOVEREIGNT in country_counts so that Other Countries is first, then ascending order
country_counts <- country_counts %>% mutate(SOVEREIGNT = fct_reorder(.f = factor(SOVEREIGNT), .x = participant_count)) %>%
        mutate(SOVEREIGNT = fct_relevel(.f = SOVEREIGNT, "Other countries", after = 0))


###################


# add fill_color_bin and fill_color
country_counts <- country_counts %>% rename(n = participant_count) %>% mutate(fill_color_bin = case_when(is.na(n) ~ "0_to_1",
                                                                                                         n > 0 & n < 10 ~ "1_to_10",
                                                                                                         n >= 10 & n < 100 ~ "10_to_100",
                                                                                                         n >= 100 & n < 1000 ~ "100_to_1000",
                                                                                                         n >= 1000 & n < 5000 ~ "1000_to_5000",
                                                                                                         n >= 5000 & n < 10000 ~ "5000_to_10000"),
                                                                              fill_color = case_when(fill_color_bin == "0_to_1" ~ "#ffffff",
                                                                                                     fill_color_bin == "1_to_10" ~ viridis_pal()(5)[1],
                                                                                                     fill_color_bin == "10_to_100" ~ viridis_pal()(5)[2],
                                                                                                     fill_color_bin == "100_to_1000" ~ viridis_pal()(5)[3],
                                                                                                     fill_color_bin == "1000_to_5000" ~ viridis_pal()(5)[4],
                                                                                                     fill_color_bin == "5000_to_10000" ~ viridis_pal()(5)[5]))

# inspect
country_counts %>% count(fill_color_bin, fill_color)
country_counts %>% count(n, fill_color_bin, fill_color) %>% arrange(n)
country_counts %>% count(n, fill_color_bin, fill_color) %>% arrange(desc(n))


###################


# create country_bar_chart_fill_color_list for to pass to scale_color_manual
country_bar_chart_fill_color_list <- country_counts %>% count(fill_color_bin, fill_color) %>% pull(fill_color)
names(country_bar_chart_fill_color_list) <- country_counts %>% count(fill_color_bin, fill_color) %>% pull(fill_color_bin)
country_bar_chart_fill_color_list


####################


# create bar chart
# note that the vertical nudges on the count label above the bars, and the slanted name labels, have not been updated and would need to be tweaked to the data
country_bar_chart <- country_counts %>%
        ggplot(data = ., aes(x = SOVEREIGNT, y = n, fill = fill_color_bin)) + geom_col(width = .5) + 
        scale_fill_manual(values = country_bar_chart_fill_color_list) +
        scale_y_continuous(labels = comma, expand = expand_scale(mult = 0, add = c(0, 400))) +
        labs(y = "Number of CAGP participants") +
        geom_text(aes(label = comma(n), y = n), vjust = 0, size = 2.7,
                  nudge_y = case_when(country_counts$SOVEREIGNT == "Colombia" ~ 200,
                                      country_counts$SOVEREIGNT == "El Salvador" ~ 700,
                                      country_counts$SOVEREIGNT == "Vietnam" ~ 100,
                                      country_counts$SOVEREIGNT == "Burma" ~ 600,
                                      country_counts$SOVEREIGNT == "Bhutan" ~ 100,
                                      country_counts$SOVEREIGNT == "Dominican Rep." ~ 400,
                                      country_counts$SOVEREIGNT == "Iraq" ~ 500,
                                      TRUE ~ 100)) +
        theme(
                panel.grid.major = element_line(color = "transparent"),
                plot.background = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                line = element_blank(),
                rect = element_blank(),
                axis.title.y = element_text(family = "Source Sans Pro", size = 9, color = "black"),
                axis.text.y = element_blank(),
                axis.text.x = element_text(angle = -45, hjust = 0, family = "Source Sans Pro", size = 9, color = "black"), 
                axis.title.x = element_blank(),
                plot.margin = margin(0, 10, 20, 0),
                plot.title = element_blank(),
                legend.position = "none") 

country_bar_chart

# save
ggsave(filename = "scratchpad/country_bar_chart.pdf", plot = country_bar_chart, units = "in", dpi = 300, device = cairo_pdf, height = 5, width = 10)


##########


# add source footnote

# convert to png
country_bar_chart_pdf <- pdf_render_page(pdf = "scratchpad/country_bar_chart.pdf", page = 1, dpi = 300)
country_bar_chart_image <- image_read(country_bar_chart_pdf)
image_info(country_bar_chart_image)

# save country_bar_chart_source_plot as pdf
country_bar_chart_source_plot <- starwars %>% ggplot(data = ., aes(x = mass)) + geom_histogram() +
        ggtitle("Source: Fake data.") +
        theme(plot.title = element_text(size = 12, hjust = 0, family = "Source Sans Pro", color = "#808080"))
ggsave(filename = "scratchpad/country_bar_chart_source_plot.pdf", plot = country_bar_chart_source_plot, units = "in", dpi = 300, device = cairo_pdf)

# load country_bar_chart_source_plot as image
country_bar_chart_source_pdf <- pdf_render_page(pdf = "scratchpad/country_bar_chart_source_plot.pdf", page = 1, dpi = 300)

# convert pdf to image
country_bar_chart_source_image <- image_read(country_bar_chart_source_pdf)

# crop out country_bar_chart_source_image
image_info(country_bar_chart_source_image)
country_bar_chart_source_image <- image_crop(image = country_bar_chart_source_image, geometry = geometry_area(width = 1500, height = 80, x_off = 130, y_off = 0))
country_bar_chart_source_image

# overlay source_footnote_image
image_info(country_bar_chart_image)
country_bar_chart_image <- image_composite(image = country_bar_chart_image, composite_image = image_resize(image = country_bar_chart_source_image,
                                                                                                           geometry = geometry_size_pixels(width = 950, height = 300)), offset = geometry_point(x = 0, y = 1250))
country_bar_chart_image

# save as png
image_write(image = country_bar_chart_image, path = "scratchpad/country_bar_chart.png", format = 'png', density = '300x300')


###########################################################################################
###########################################################################################
###########################################################################################


# create us_state_map

setwd("H:/R/sf")

# download zip folder with shapefile for state 
# https://github.com/UrbanInstitute/urbnmapr/blob/master/data-raw/ccdf-map.R
url <- "http://www2.census.gov/geo/tiger/GENZ2017/shp/cb_2017_us_state_5m.zip"
# temp <- tempfile(fileext = ".zip")
# download.file(url, temp)
# unzip(temp, exdir = "states_tiger_shapefiles_ftp")

# read in shapefile as sf
states_sf <- st_read("states_tiger_shapefiles_ftp/cb_2017_us_state_5m.shp")
states_sf
glimpse(states_sf)

# read in shapefile manually downloaded from census tiger web interface (it's 5k resolution, so not very detailed)
# states_sf <- st_read("state_tiger_shapefiles/tl_2017_us_state.shp")
# states_sf
# glimpse(states_sf)

# set coordinate reference system to US National Atlas Equal Area (EPSG 2163)
# when we plot the map at the end, we'll use USA Contiguous Albers Equal Area Conic (EPSG: 102003)
# even though the map will ultimately use 102003, we need to capture hawaii/alaska using 2163 to avoid distortions
# because i think 102003 focuses on projecting conus only
states_sf <- states_sf %>% 
        st_transform(2163) %>%
        identity()


###########################################################################


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


###############################################################################


# transform alaska state
alaska_state <- states_sf %>% filter(NAME == "Alaska") %>%
        # st_crop(c(xmin = -4000000, ymin = 1466024, xmax = -1512211, ymax = 3909687)) %>%
        transform_states(points_sf = ., shift = c(820000, -4730000), scale = .35, rotate_degrees = -50) %>%
        identity()
alaska_state

# alaska_state %>% ggplot() + geom_sf()


#############################


# transform hawaii state
hawaii_state <- states_sf %>% filter(NAME == "Hawaii") %>%
        # st_crop(c(xmin = -6000000, ymin = -4000000, xmax = 4000000, ymax = -400000)) %>%
        transform_states(points_sf = ., shift = c(5000000, -1150000), scale = 1, rotate_degrees = -35) %>%
        identity()
hawaii_state

# hawaii_state %>% ggplot() + geom_sf()


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


##################################################################################


# create fake data
states_sf_transformed %>% glimpse()
fake_data <- states_sf_transformed %>% pull(NAME) %>% tibble(NAME = .) %>% 
        mutate(n = c(rep(x = NA, times = 5), rep(x = 100, times = 5), rep(x = 300, times = 10), rep(x = 500, times = 10),
                     rep(x = 700, times = 5), rep(x = 900, times = 5), rep(x = 1500, times = 5), rep(x = 3000, times = 6)))
fake_data
fake_data %>% arrange(n)


##################################################################################


# join fake_data to states_sf_transformed
states_sf_transformed %>% glimpse()
states_sf_transformed <- states_sf_transformed %>% left_join(., fake_data, by = "NAME")
states_sf_transformed %>% glimpse()


#################################################################################


# add fill_color_bin and fill_color
states_sf_transformed %>% ggplot(data = ., aes(x = n)) + geom_histogram()

states_sf_transformed <- states_sf_transformed %>% mutate(n_for_plotting = ifelse(!is.na(n), comma(n), n),
                                                          fill_color_bin = case_when(is.na(n) | n == 0 ~ "c1_0_to_1",
                                                                                     n > 0 & n < 200 ~ "c2_1_to_200",
                                                                                     n >= 200 & n < 400 ~ "c3_200_to_400",
                                                                                     n >= 400 & n < 600 ~ "c4_400_to_600",
                                                                                     n >= 600 & n < 800 ~ "c5_600_to_800",
                                                                                     n >= 800 & n < 1000 ~ "c6_800_to_1000",
                                                                                     n >= 1000 & n < 2000 ~ "c7_1000_to_2000",
                                                                                     n >= 2000 & n < 5000 ~ "c8_2000_to_5000"),
                                                          fill_color = case_when(fill_color_bin == "c1_0_to_1" ~ "#ffffff",
                                                                                 fill_color_bin == "c2_1_to_200" ~ viridis_pal()(7)[1],
                                                                                 fill_color_bin == "c3_200_to_400" ~ viridis_pal()(7)[2],
                                                                                 fill_color_bin == "c4_400_to_600" ~ viridis_pal()(7)[3],
                                                                                 fill_color_bin == "c5_600_to_800" ~ viridis_pal()(7)[4],
                                                                                 fill_color_bin == "c6_800_to_1000" ~ viridis_pal()(7)[5],
                                                                                 fill_color_bin == "c7_1000_to_2000" ~ viridis_pal()(7)[6],
                                                                                 fill_color_bin == "c8_2000_to_5000" ~ viridis_pal()(7)[7]))

# inspect
states_sf_transformed %>% count(n, fill_color_bin, fill_color) %>% arrange(n)
states_sf_transformed %>% count(fill_color_bin, fill_color) %>% arrange(fill_color_bin)
states_sf_transformed %>% count(fill_color_bin, fill_color) %>% arrange(desc(fill_color_bin))
states_sf_transformed %>% select(NAME, n, n_for_plotting) %>% arrange(NAME) %>% data.frame()


#############


# create fill_color_list for to pass to scale_color_manual
state_fill_color_list <- states_sf_transformed %>% count(fill_color_bin, fill_color) %>% pull(fill_color)
names(state_fill_color_list) <- states_sf_transformed %>% count(fill_color_bin, fill_color) %>% pull(fill_color_bin)
state_fill_color_list


##############


# create text_color_list
text_color_list <- c("#000000", "#ffffff", "#ffffff", "#ffffff", "#ffffff", "#000000", "#000000", "#000000")
names(text_color_list) <- states_sf_transformed %>% count(fill_color_bin, fill_color) %>% pull(fill_color_bin)
text_color_list


##############


# create geom_segment_tbl for line segment pointing out geom_text for east coast states
# note will get warning about st_centroid assumptions - no problem
geom_segment_list <- states_sf_transformed %>% 
        filter(NAME %in% c("New Jersey", "District of Columbia", "Maryland", "Rhode Island", "Connecticut", "Delaware", "Massachusetts")) %>% 
        st_transform(102003) %>% st_centroid(geometry) %>% pull(geometry) %>% unlist()
geom_segment_list

geom_segment_tbl_id <- states_sf_transformed %>% 
        filter(NAME %in% c("New Jersey", "District of Columbia", "Maryland", "Rhode Island", "Connecticut", "Delaware", "Massachusetts")) %>% 
        st_transform(102003) %>% st_centroid(geometry) %>% mutate(NAME = as.character(NAME)) %>% pull(NAME)
geom_segment_tbl_id

geom_segment_tbl <- tibble(NAME = geom_segment_tbl_id,
                           centroid_x = geom_segment_list [seq(from = 1, to = length(geom_segment_list), by = 2)],
                           centroid_y = geom_segment_list[seq(from = 2, to = length(geom_segment_list), by = 2)])
geom_segment_tbl


##############################################################################################


# create states_map
states_map <- states_sf_transformed %>% 
        mutate(text_color = case_when(NAME == "District of Columbia" ~ "c8_2000_to_5000",
                                      NAME == "New Jersey" ~ "c8_2000_to_5000",
                                      NAME == "Rhode Island" ~ "c8_2000_to_5000",
                                      NAME == "Connecticut" ~ "c8_2000_to_5000",
                                      NAME == "Delaware" ~ "c8_2000_to_5000",
                                      NAME == "Hawaii" ~ "c8_2000_to_5000",
                                      NAME == "Massachusetts" ~ "c8_2000_to_5000",
                                      TRUE ~ fill_color_bin)) %>%
        st_transform(102003) %>%
        mutate(centroid = st_centroid(geometry)) %>%
        mutate(lon = unlist(map(.x = .$centroid, .f = ~ st_coordinates(.x)[[1]])),
               lat = unlist(map(.x = .$centroid, .f = ~ st_coordinates(.x)[[2]]))) %>%
        ggplot() + geom_sf(aes(geometry = geometry, fill = fill_color_bin), color = "#a6a6a6", size = .125) + 
        coord_sf(crs = st_crs(102003)) +
        scale_fill_manual(values = state_fill_color_list, name = "Number\nof\nCAGP\nparticipants\n",
                          labels = c(1, 200, 400, 600, 800, comma(1000), comma(2000), comma(5000))) +
        geom_text(aes(label = n_for_plotting, x = lon, y = lat, color = text_color), check_overlap = TRUE, family = "Source Sans Pro", size = 1.5,
                  nudge_x = case_when(states_sf_transformed$NAME == "New Jersey" ~ 490000,
                                      states_sf_transformed$NAME == "Rhode Island" ~ 490000,
                                      states_sf_transformed$NAME == "District of Columbia" ~ 480000,
                                      states_sf_transformed$NAME == "Connecticut" ~ 490000,
                                      states_sf_transformed$NAME == "Maryland" ~ 520000,
                                      states_sf_transformed$NAME == "Louisiana" ~ -63000,
                                      states_sf_transformed$NAME == "Florida" ~ 130000,
                                      states_sf_transformed$NAME == "Michigan" ~ 80000,
                                      states_sf_transformed$NAME == "New York" ~ 30000,
                                      states_sf_transformed$NAME == "California" ~ -30000,
                                      states_sf_transformed$NAME == "Delaware" ~ 380000,
                                      states_sf_transformed$NAME == "Massachusetts" ~ 520000,
                                      states_sf_transformed$NAME == "Hawaii" ~ -40000,
                                      TRUE ~ 0),
                  nudge_y = case_when(states_sf_transformed$NAME == "District of Columbia" ~ -210000,
                                      states_sf_transformed$NAME == "Rhode Island" ~ 170000, 
                                      states_sf_transformed$NAME == "Maryland" ~ 30000,
                                      states_sf_transformed$NAME == "Michigan" ~ -150000,
                                      states_sf_transformed$NAME == "Florida" ~ -120000,
                                      states_sf_transformed$NAME == "Delaware" ~ -120000,
                                      states_sf_transformed$NAME == "Massachusetts" ~ 260000,
                                      states_sf_transformed$NAME == "Idaho" ~ -50000,
                                      states_sf_transformed$NAME == "Hawaii" ~ -50000,
                                      states_sf_transformed$NAME == "Alaska" ~ 50000,
                                      TRUE ~ 0)) +
        scale_color_manual(values = text_color_list, guide = FALSE) +
        geom_segment(data = geom_segment_tbl, size = .1, 
                     x = case_when(geom_segment_tbl$NAME == "Maryland" ~ geom_segment_tbl$centroid_x - 50000,
                                   geom_segment_tbl$NAME == "New Jersey" ~ geom_segment_tbl$centroid_x + 20000,
                                   geom_segment_tbl$NAME == "Delaware" ~ geom_segment_tbl$centroid_x + 20000,
                                   TRUE ~ geom_segment_tbl$centroid_x),
                     y = case_when(geom_segment_tbl$NAME == "Maryland" ~ geom_segment_tbl$centroid_y + 30000,
                                   geom_segment_tbl$NAME == "Delaware" ~ geom_segment_tbl$centroid_y - 20000,
                                   geom_segment_tbl$NAME == "Massachusetts" ~ geom_segment_tbl$centroid_y + 10000,
                                   TRUE ~ geom_segment_tbl$centroid_y),
                     xend = case_when(geom_segment_tbl$NAME == "Delaware" ~ geom_segment_tbl$centroid_x + 300000,
                                      TRUE ~ geom_segment_tbl$centroid_x + 400000),
                     yend = case_when(geom_segment_tbl$NAME == "District of Columbia" ~ geom_segment_tbl$centroid_y - 200000,
                                      geom_segment_tbl$NAME == "Rhode Island" ~ geom_segment_tbl$centroid_y + 170000,
                                      geom_segment_tbl$NAME == "Maryland" ~ geom_segment_tbl$centroid_y + 30000,
                                      geom_segment_tbl$NAME == "Delaware" ~ geom_segment_tbl$centroid_y - 130000,
                                      geom_segment_tbl$NAME == "Massachusetts" ~ geom_segment_tbl$centroid_y + 260000,
                                      TRUE ~ geom_segment_tbl$centroid_y)) +
        theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(),
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              plot.title = element_text(size = 6, face = "bold", hjust = 0, family = "Source Sans Pro"), legend.position = "right",
              legend.key.size = unit(2, "mm"), legend.title = element_text(size = 4, family = "Source Sans Pro"),
              legend.text = element_text(size = 4, family = "Source Sans Pro"),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + 
        guides(fill = guide_legend(title.hjust = .5, reverse = TRUE, keyheight = 1, label.vjust = 1.13)) 

states_map


################


# note that for some reason ggplot_center_legend_title fails when geom_text is added, but the legend is roughly centered anyway (could also add w magick)
# call ggplot_center_legend_title
# states_map_grob <- ggplot_center_legend_title(plot = states_map)
# grid.newpage()
# grid.draw(states_map_grob)

# save map as pdf (note that cairo_pdf is needed to render non-standard fonts, and it can't save as png, so pdf is only option)
ggsave(filename = "scratchpad/states_map.pdf", plot = states_map, units = "in", dpi = 300, device = cairo_pdf)


#########


# read pdf as image with magick and pdftools, then crop out white space, then save as a png

# read pdf
states_map_pdf <- pdf_render_page(pdf = "scratchpad/states_map.pdf", page = 1, dpi = 300)

# convert to magick image
cigp_state_map_image <- image_read(states_map_pdf)

# crop out white space
image_info(states_map_image)
cigp_state_map_image <- image_crop(image = cigp_state_map_image, geometry = geometry_area(width = 1500, height = 900, x_off = 80, y_off = 420))


##########


# add source footnote

# save world_map_source_plot as pdf
state_map_source_plot <- starwars %>% ggplot(data = ., aes(x = mass)) + geom_histogram() + 
        ggtitle("Source: Fake data.") +
        theme(plot.title = element_text(size = 12, hjust = 0, family = "Source Sans Pro", color = "#808080"))
ggsave(filename = "scratchpad/state_map_source_plot.pdf", plot = state_map_source_plot, units = "in", dpi = 300, device = cairo_pdf)

# load state_map_source_plot as image
state_map_source_pdf <- pdf_render_page(pdf = "scratchpad/state_map_source_plot.pdf", page = 1, dpi = 300)

# convert pdf to image
state_map_source_image <- image_read(state_map_source_pdf)

# crop out state_map_source_image
image_info(state_map_source_image)
state_map_source_image <- image_crop(image = state_map_source_image, geometry = geometry_area(width = 500, height = 80, x_off = 130, y_off = 0))
state_map_source_image 

# overlay source_footnote_image
image_info(cigp_state_map_image)
cigp_state_map_image <- image_composite(image = cigp_state_map_image, composite_image = image_resize(image = state_map_source_image, 
                                                                                                     geometry = geometry_size_pixels(width = 165, height = 200)), offset = geometry_point(x = 0, y = 850))


################


# add 0 tick mark to legend

# load map as image and add zero to legend (zero image is saved in creation of world map above)
zero_image <- image_read("scratchpad/zero_image.png")
image_info(cigp_state_map_image)
cigp_state_map_image <- image_composite(image = cigp_state_map_image, composite_image = image_resize(image = zero_image, 
                                                                                                     geometry = geometry_size_pixels(width = 17, height = 17)), offset = geometry_point(x = 1441, y = 740))
cigp_state_map_image

# save states_map_image
image_write(image = cigp_state_map_image, path = "scratchpad/cigp_state_map.png", format = "png", density = "300x300")
