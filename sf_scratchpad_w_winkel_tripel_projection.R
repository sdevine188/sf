# note that fonts should be loaded before loading ggplot2; 
# see https://stackoverflow.com/questions/14733732/cant-change-fonts-in-ggplot-geom-text
extrafont::loadfonts(device="win") 
library(tidyverse)
library(cowplot)   # for theme_map()
library(sf)        # for manipulation of simple features objects
library(lwgeom)    # for st_transform_proj()
library(rworldmap) # for getMap()
library(janitor)
library(RColorBrewer)
library(scales)
library(officer)
library(devEMF)
# library(ggsn)
library(ggspatial) # ggspatial has the best map scale and north arrow
library(ggmap)
library(viridis)
library(pals)
# library(colorblindcheck)

# https://wilkelab.org/practicalgg/articles/Winkel_tripel.html
# note that getMap uses natural earth for country boundaries, but fixes several issues, so better to pull from rworldmap
# (see rworldmap docs: https://cran.r-project.org/web/packages/rworldmap/rworldmap.pdf)
# ?countriesCoarse
# ?countriesCoarseLessIslands

# another option is rnaturalearth package: https://docs.ropensci.org/rnaturalearth/

# a good site on gis in r: https://www.jessesadler.com/post/gis-with-r-intro/

# tutorial on ggsn for scale and north arrow: http://oswaldosantos.github.io/ggsn/

# options for topographical maps
# https://gis.stackexchange.com/questions/224035/how-to-create-a-crisp-topographical-terrain-map-with-ggplot2

# ggmap: https://www.littlemissdata.com/blog/maps
# https://cengel.github.io/R-spatial/mapping.html#adding-basemaps-with-ggmap

# color palettes: https://nowosad.github.io/post/cbc-bp2/

# setwd
# setwd("C:/Users/Stephen/Desktop/R/sf")
setwd("C:/Users/sdevine/Desktop/R/sf")

# get world map as sf object ####
world_sf <- st_as_sf(getMap(resolution = "low"))
world_sf
world_sf %>% glimpse()
world_sf %>% class()
st_crs(world_sf)

# note when I pull SOVEREIGNT out as a character in a tibble, it's only 202 countries; 
# there is 244 rows though, and 244 distinct SOVEREIGNT - but some are duplicates, 
# the duplicates appear to be countries with non-contiguous boundaries (aka mainlands and islands)
# since world_sf is an sf object, calling distinct(SOVEREIGNT) kind of always implicitly includes the geometry variable
# and since these non-contiguous countries have multiple geometries, distinct() returns those multiple rows
world_sf %>% nrow() # 244
world_sf %>% distinct(SOVEREIGNT) %>% nrow() # 244
world_sf %>% distinct(SOVEREIGNT) %>% pull(SOVEREIGNT) %>% 
        tibble(SOVEREIGNT = .) %>% mutate(SOVEREIGNT = as.character(SOVEREIGNT)) %>% nrow() # 244
world_sf %>% distinct(SOVEREIGNT) %>% pull(SOVEREIGNT) %>% 
        tibble(SOVEREIGNT = .) %>% mutate(SOVEREIGNT = as.character(SOVEREIGNT)) %>% 
        distinct(SOVEREIGNT) %>% nrow() # 202
world_sf %>% distinct(SOVEREIGNT) %>% pull(SOVEREIGNT) %>% 
        tibble(SOVEREIGNT = .) %>% mutate(SOVEREIGNT = as.character(SOVEREIGNT)) %>%
        get_dupes(SOVEREIGNT) %>% distinct(SOVEREIGNT)
world_sf %>% distinct(SOVEREIGNT) %>% pull(SOVEREIGNT) %>% 
        tibble(SOVEREIGNT = .) %>% mutate(SOVEREIGNT = as.character(SOVEREIGNT)) %>% 
        distinct(SOVEREIGNT) %>%
        print(n = nrow(.))


#/////////////////////////////////////////////////////////////////////////////////////


# join world_sf with data of interest ####

# create fake_data
fake_data <- world_sf %>% pull(SOVEREIGNT) %>%  tibble(SOVEREIGNT = .) %>% 
        mutate(SOVEREIGNT = as.character(SOVEREIGNT)) %>% distinct(SOVEREIGNT) 
fake_data <- fake_data %>% mutate(n = sample(x = seq(from = 0, to = 10000, by = 1), size = nrow(fake_data))) %>%
        mutate(n = case_when(row_number() %in% 1:20 ~ NA_real_, row_number() %in% 21:40 ~ 5, TRUE ~ n),
               SOVEREIGNT = factor(SOVEREIGNT))
fake_data %>% arrange(n)
fake_data %>% arrange(desc(n))
fake_data %>% ggplot(data = ., aes(x = n)) + geom_histogram()

# join data of interest with world_sf
world_sf <- world_sf %>% left_join(., fake_data, by = "SOVEREIGNT")
world_sf %>% glimpse()


#////////////////////


# create fake_data_2
fake_data_2 <- world_sf %>% pull(SOVEREIGNT) %>%  tibble(SOVEREIGNT = .) %>%  
        mutate(SOVEREIGNT = as.character(SOVEREIGNT)) %>% distinct(SOVEREIGNT) 
fake_data_2 <- fake_data_2 %>% mutate(n2 = sample(x = seq(from = 0, to = 10000, by = 1), size = nrow(fake_data))) %>%
        mutate(n2 = case_when(row_number() %in% 1:20 ~ NA_real_, row_number() %in% 21:40 ~ 5, TRUE ~ n2),
               SOVEREIGNT = factor(SOVEREIGNT))
fake_data_2 %>% arrange(n2)
fake_data_2 %>% arrange(desc(n2))
fake_data_2 %>% ggplot(data = ., aes(x = n2)) + geom_histogram()

# join data of interest with world_sf
world_sf <- world_sf %>% left_join(., fake_data_2, by = "SOVEREIGNT")
world_sf %>% glimpse()


#/////////////////////////////////////////////////////////////////////////////////////


# get color_palette ####
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

color_palette <- viridis_pal()(5)
color_palette <- tibble(hex = color_palette)
show_col(color_palette %>% pull(hex))

# add fill_color_bin and fill_color
world_sf <- world_sf %>% 
        mutate(fill_color_bin = case_when(is.na(n) ~ "0_to_1",
                                          n > 0 & n < 50 ~ "1_to_50",
                                          n >= 50 & n < 150 ~ "50_to_150",
                                          n >= 150 & n < 1500 ~ "150_to_1500",
                                          n >= 1500 & n < 5000 ~ "1500_to_5000",
                                          n >= 5000 ~ "5000_to_130000"),
               fill_color = case_when(fill_color_bin == "0_to_1" ~ "#ffffff",
                                      fill_color_bin == "1_to_50" ~ color_palette %>% slice(5) %>% pull(hex),
                                      fill_color_bin == "50_to_150" ~ color_palette %>% slice(4) %>% pull(hex),
                                      fill_color_bin == "150_to_1500" ~ color_palette %>% slice(3) %>% pull(hex),
                                      fill_color_bin == "1500_to_5000" ~ color_palette %>% slice(2) %>% pull(hex),
                                      fill_color_bin == "5000_to_130000" ~ color_palette %>% slice(1) %>% pull(hex)))

# inspect
world_sf
world_sf %>% distinct(SOVEREIGNT, fill_color_bin, fill_color) %>% count(fill_color_bin, fill_color)
world_sf %>% distinct(SOVEREIGNT, n) %>% filter(is.na(n))        


#///////////


# create fill_color_list for to pass to scale_color_manual
world_fill_color_list <- world_sf %>% select(fill_color_bin, fill_color) %>% 
        as_tibble() %>% select(-geometry) %>% count(fill_color_bin, fill_color) %>% pull(fill_color)
names(world_fill_color_list) <- world_sf %>% select(fill_color_bin, fill_color) %>% 
        as_tibble() %>% select(-geometry) %>% distinct(fill_color_bin) %>% pull(fill_color_bin)
world_fill_color_list


#/////////////////////////////////////////////////////////////////////////////////////


# import extra fonts from extrafont package

# https://stackoverflow.com/questions/61204259/how-can-i-resolve-the-no-font-name-issue-when-importing-fonts-into-r-using-ext/68642855#68642855
# https://github.com/wch/extrafont/issues/32
# https://cran.r-project.org/web/packages/extrafont/README.html
library(remotes)
remotes::install_version("Rttf2pt1", version = "1.3.8")
library(extrafont)
# extrafont::font_import()
# loadfonts(device = "win")
windowsFonts()
fonttable()
fonts()


#//////////////////////////////////////////////////////////////////////////////////////


# plot country ####
country_outline <- world_sf %>% 
        mutate(fill_color_bin = "selected_color") %>%
        filter(SOVEREIGNT != "Antarctica") %>%
        filter(SOVEREIGNT == "Ukraine") %>%
        ggplot(data = ., mapping = aes(fill = factor(fill_color_bin))) + 
        geom_sf(color = "#E0ECF7", size = 0.05) +
        scale_fill_manual(values = list("selected_color" = "#E0ECF7"), guide = "none") +
        theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              # plot.title = element_text(size = 6, face = "bold", hjust = 0, family = "Trebuchet MS"), 
              plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                          color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
              legend.position = "right",
              legend.key.size = unit(2, "mm"),
              legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")), 
              legend.title = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank())
country_outline


#/////////////////////////////////////////////////////////////////////////////////////


# plot basic, w bounding box ####
# note that basic plots like this can be overlaid with bounding box to show global location as inset for zoomed in local map
# using annotate() you can add a rect geom; note that using geom_rect has issues since it inherits the default dataframe
# passed to ggplot, so you either need to create a seperate dataframe, or other workarounds
# annotate is more elegant and general approach, since you can use it also to add other geoms incl. segments, points, text, etc 
# https://ggplot2.tidyverse.org/reference/annotate.html
# note that annotate is tied to the plot scale though, whereas ggdraw can literally draw anywhere on a plot using 0 to 1 x/y pct
world_sf %>% 
        filter(SOVEREIGNT != "Antarctica") %>%
        ggplot(aes(fill = factor(fill_color_bin, 
                                 levels = c("0_to_1", "1_to_50", "50_to_150", "150_to_1500", "1500_to_5000", "5000_to_130000")))) + 
        geom_sf(color = "#a6a6a6", size = 0.05) +
        scale_fill_manual(values = world_fill_color_list, name = " \n ",
                          labels = c(1, 50, 150, comma(1500), comma(5000), comma(130000))) +
        annotate(geom = "rect", xmin = 20, xmax = 50, ymin = 30, ymax = 60, fill = "#ff0000", alpha = .5) +
        theme_bw() +
        # theme_map() # cowplot has theme_map as short_hand; (see: ?theme_map)
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              # plot.title = element_text(size = 6, face = "bold", hjust = 0, family = "Trebuchet MS"), 
              plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                          color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
              legend.position = "right",
              legend.key.size = unit(2, "mm"),
              legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")), 
              legend.title = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + 
        guides(fill = guide_legend(title.hjust = .5, title.vjust = 1.5, reverse = TRUE, keyheight = 1, label.vjust = 1.42))


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# plot using winkel tripel projection by first transforming projection ####
world_map <- world_sf %>% st_transform_proj(crs = "+proj=wintri +datum=WGS84 +no_defs +over") %>%
        filter(SOVEREIGNT != "Antarctica") %>%
        ggplot(aes(fill = factor(fill_color_bin, 
                                 levels = c("0_to_1", "1_to_50", "50_to_150", "150_to_1500", "1500_to_5000", "5000_to_130000")))) + 
        geom_sf(color = "#a6a6a6", size = 0.05) +
        scale_fill_manual(values = world_fill_color_list, name = " \n ",
                          labels = c(1, 50, 150, comma(1500), comma(5000), comma(130000))) +
        coord_sf(datum = NULL) +
        theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              # plot.title = element_text(size = 6, face = "bold", hjust = 0, family = "Trebuchet MS"), 
              plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                          color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
              legend.position = "right",
              legend.key.size = unit(2, "mm"),
              legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")), 
              legend.title = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + 
        guides(fill = guide_legend(title.hjust = .5, title.vjust = 1.5, reverse = TRUE, keyheight = 1, label.vjust = 1.42))

# inspect
world_map


#///////////////////


# add zero to legend, add legend title, add source
world_map_output <- world_map %>% 
        ggdraw(xlim = c(0, 1.1)) + 
        draw_label("0", x = .89, y = .38, size = 12, fontfamily = "Calibri") +
        draw_label(label = "Number of", x = .9, y = .63, size = 12, fontfamily = "Calibri", hjust = .5, vjust = .5) +
        draw_label(label = "widgets", x = .9, y = .599, size = 12, fontfamily = "Calibri", hjust = .5, vjust = .5) +
        draw_label(label = "Source: USAID", x = .25, y = .29, size = 11, fontfamily = "Calibri", color = "#000000")


#///////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(world_map_output)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
# note when saving from word to pdf, you'll need to print -> as pdf, 
# instead of save_as -> pdf, since save_as has issues rendering emfs
read_docx() %>% 
        body_add_img(src = filename, width = 7, height = 7) %>% 
        print(target = "world_map.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# plot with selected regions only ####
# not using winkel tripel to match google maps (which looks normal for sub-global scale) 
world_sf %>% filter(str_detect(string = SOVEREIGNT, pattern = regex("serb", ignore_case = TRUE))) %>% select(SOVEREIGNT)

region_map <- world_sf %>%  
        filter(SOVEREIGNT %in% c("Albania", "Bosnia and Herzegovina", "Kosovo", "Macedonia",
                                 "Republic of Serbia", "Ukraine", "Belarus", "Moldova", "Armenia", "Azerbaijan",
                                 "Georgia", "Bulgaria", "Romania", "Montenegro", "Croatia", "Hungary", "Slovakia", "Poland")) %>%
        ggplot(aes(fill = factor(fill_color_bin, 
                                 levels = c("0_to_1", "1_to_50", "50_to_150", "150_to_1500", "1500_to_5000", "5000_to_130000")))) + 
        geom_sf(color = "#a6a6a6", size = 0.05) +
        scale_fill_manual(values = world_fill_color_list, name = " \n ",
                          labels = c(1, 50, 150, comma(1500), comma(5000), comma(130000))) +
        theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              # plot.title = element_text(size = 6, face = "bold", hjust = 0, family = "Trebuchet MS"), 
              plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                          color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
              legend.position = "right",
              legend.key.size = unit(2, "mm"),
              legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")), 
              legend.title = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + 
        guides(fill = guide_legend(title.hjust = .5, title.vjust = 1.5, reverse = TRUE, keyheight = 1, label.vjust = 1.42))

# inspect
region_map


#///////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(region_map)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
# note when saving from word to pdf, you'll need to print -> as pdf, 
# instead of save_as -> pdf, since save_as has issues rendering emfs
read_docx() %>% 
        body_add_img(src = filename, width = 7, height = 7) %>% 
        print(target = "region_map.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# plot with selected countries only, also north arrow and scale ####
# note that with north arrow and scaling you can't use winkel tripel projection
world_sf %>% filter(str_detect(string = SOVEREIGNT, pattern = regex("hungary", ignore_case = TRUE))) %>% pull(SOVEREIGNT)

# plot 
countries_map <- world_sf %>%
        filter(SOVEREIGNT %in% c("Kosovo", "Croatia", "Bosnia and Herzegovina", "Montenegro", "Republic of Serbia", 
                                 "Albania", "Hungary")) %>%
        ggplot(aes(fill = factor(fill_color_bin, 
                                 levels = c("0_to_1", "1_to_50", "50_to_150", "150_to_1500", "1500_to_5000", "5000_to_130000")))) + 
        geom_sf(color = "#a6a6a6", size = 0.05) +
        scale_fill_manual(values = world_fill_color_list, name = " \n ",
                          labels = c(1, 50, 150, comma(1500), comma(5000), comma(130000))) +
        annotation_scale(location = "bl", width_hint = 0.3, text_cex = 0.9) +
        annotation_north_arrow(location = "bl", which_north = "true", 
                               pad_x = unit(0.8, "in"), pad_y = unit(0.3, "in"),
                               width = unit(1, "cm"), height = unit(1, "cm"),
                               style = north_arrow_fancy_orienteering) +
        theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              # plot.title = element_text(size = 6, face = "bold", hjust = 0, family = "Trebuchet MS"), 
              plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                          color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
              legend.position = "right",
              legend.key.size = unit(2, "mm"),
              legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")), 
              legend.title = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
              legend.margin = margin(0, 0, 0, 0),
              legend.box.margin = margin(0, 0, 0, 0),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + 
        guides(fill = guide_legend(title.hjust = .5, title.vjust = 1.5, reverse = TRUE, keyheight = 2, label.vjust = 1.42))

# inspect
countries_map


#///////////////////


# add compass with cowplot, because north() icon got corrupted when saving to emf
# https://www.google.com/search?as_st=y&tbm=isch&hl=en&as_q=map+compass+icon+high+quality+resolution&as_epq=&as_oq=&as_eq=&cr=&as_sitesearch=&safe=images&tbs=isz:l#imgrc=QqE7rXXPOOE1IM
# countries_map <- ggdraw() + draw_plot(countries_map) + draw_image("compass_2_cropped.png", x = .6, y = .2, width = .1)
# countries_map


#///////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(countries_map)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
# note when saving from word to pdf, you'll need to print -> as pdf, 
# instead of save_as -> pdf, since save_as has issues rendering emfs
read_docx() %>% 
        body_add_img(src = filename, width = 7, height = 7) %>% 
        print(target = "countries_map.docx")



#///////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////
#///////////////////////////////////////////////////////////////////////////////////////////////////////


# customized to e&e ####

# get ee_sf trimmed to region
# note the sf is using wgs84 coordinate reference system
world_sf %>% filter(str_detect(string = SOVEREIGNT, pattern = regex("russia", ignore_case = TRUE))) %>% select(SOVEREIGNT)
ee_presence_countries <- c("Kosovo", "Bosnia and Herzegovina", "Republic of Serbia", 
                           "Albania", "Armenia", "Azerbaijan", "Georgia", "Macedonia",
                           "Ukraine", "Moldova", "Belarus")

ee_sf <- world_sf %>%
        mutate(fill_color_bin = case_when(!(SOVEREIGNT %in% ee_presence_countries) ~ NA_character_,
                                          SOVEREIGNT == "Ukraine" ~ "150_to_1500",
                                          SOVEREIGNT == "Armenia" ~ "1_to_50",
                                          fill_color_bin == "0_to_1" ~ "50_to_150",
                                          TRUE ~ fill_color_bin),
               fill_color = case_when(!(SOVEREIGNT %in% ee_presence_countries) ~ NA_character_,
                                      SOVEREIGNT == "Ukraine" ~ "#21908CFF",
                                      SOVEREIGNT == "Armenia" ~ "#440154FF",
                                      fill_color == "#ffffff" ~ "#6BAED6",
                                      TRUE ~ fill_color))

ee_sf
ee_sf %>% glimpse()
st_crs(ee_sf)

# convert crs to 3857
ee_sf_3857 <- st_transform(ee_sf, 3857)
st_crs(ee_sf_3857)

# get region map using bbox from sf geometry from the original sf to get a sense of boundaries
# (not sure if the original sf projection really matters, since i think ggmap_bbox function forces everything to 3857) 
# note: this map deliberately cuts off ee map, proving that output will only show polygons/fill in range of ggmap
# so excess like turkey/russia will not be plotted, which is good since it would need to be trimmed manually if it plotted
unname(st_bbox(ee_sf))
partial_ee_bbox <- unname(st_bbox(c(xmin = 13.51714, xmax = 50.36595, ymax = 48.55347, ymin = 40.93446), crs = st_crs(4326)))
partial_ee_bbox

ee_bbox <- unname(st_bbox(c(xmin = 11, xmax = 55, ymax = 57, ymin = 36), crs = st_crs(4326)))
ee_bbox

test_map_ee <- get_map(location = ee_bbox, source = "stamen", maptype = "terrain", zoom = 7, language = "en-EN")
ggmap(test_map_ee)


# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
        if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
        # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
        # and set the names to what sf::st_bbox expects:
        map_bbox <- setNames(unlist(attr(map, "bb")), 
                             c("ymin", "xmin", "ymax", "xmax"))
        
        # Coonvert the bbox to an sf polygon, transform it to 3857, 
        # and convert back to a bbox (convoluted, but it works)
        bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
        
        # Overwrite the bbox of the ggmap object with the transformed coordinates 
        attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
        attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
        attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
        attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
        map
}

# update ggmap bbox
test_map_ee <- ggmap_bbox(test_map_ee)

# create map
# note viridis color palette works better than blues, since ocean blue is too similar
output_map <- ggmap(test_map_ee) + 
        coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
        geom_sf(data = ee_sf_3857, inherit.aes = FALSE, 
                aes(fill = factor(fill_color_bin, 
                                  levels = c("1_to_50", "50_to_150", "150_to_1500", "1500_to_5000", "5000_to_130000"))),
                color = "#ffffff", size = .5) +
        scale_fill_manual(values = world_fill_color_list[-1], name = " \n ",
                          labels = c("1 to 50", "51 to 150", "151 to 1,500", "1,501 to 5,000", "5,001 to 130,000"),
                          na.translate = FALSE) +
        theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              # plot.title = element_text(size = 6, face = "bold", hjust = 0, family = "Trebuchet MS"), 
              plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                          color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
              legend.position = "bottom",
              legend.key.size = unit(2, "mm"),
              legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")), 
              legend.title = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
              legend.justification = "center",
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) +
        guides(fill = guide_legend(reverse = FALSE, 
                                   # title = "Fake data\nvalues", 
                                   title.hjust = .5,
                                   keywidth = 2,
                                   nrow = 2, byrow = TRUE))
# guides(fill = guide_legend(title.hjust = .5, title.vjust = 1.5, reverse = TRUE, keyheight = 1, label.vjust = 1.42))


# inspect
output_map


#///////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(output_map)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
# note when saving from word to pdf, you'll need to print -> as pdf, 
# instead of save_as -> pdf, since save_as has issues rendering emfs
read_docx() %>% 
        body_add_img(src = filename, width = 7, height = 7) %>% 
        print(target = "ee_output_map.docx")


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


# create e&e map with biscale ####

# note can try to tailor alpha fill setting for ee_presence countries, but it's tough and may not look good with terrain map??
# https://stackoverflow.com/questions/24800626/r-ggplot-transparency-alpha-values-conditional-on-other-variable
# https://ggplot2.tidyverse.org/reference/scale_alpha.html

# create biscale_ee_sf
biscale_ee_sf <- world_sf %>%
        mutate(n = case_when(
                !(SOVEREIGNT %in% ee_presence_countries) ~ NA_real_,
                SOVEREIGNT == "Azerbaijan" ~ 50,
                SOVEREIGNT == "Armenia" ~ 5550,
                SOVEREIGNT == "Albania" ~ 1000,
                TRUE ~ n),
               n2 = case_when(
                       !(SOVEREIGNT %in% ee_presence_countries) ~ NA_real_,
                       SOVEREIGNT == "Azerbaijan" ~ 50,
                       SOVEREIGNT == "Armenia" ~ 5550,
                       SOVEREIGNT == "Albania" ~ 1000,
                       TRUE ~ n2)
               # alpha_fill = case_when(SOVEREIGNT %in% ee_presence_countries ~ 1, TRUE ~ .5)
        )

# inspect
biscale_ee_sf


#//////////////////////////


# add bi_class variable, which is just a string listing the x/y color palette pairing for each value
# note that the first number of bi_class is the class of the x variable, the second number is the class of the y variable
biscale_ee_sf <- biscale_ee_sf %>% bi_class(.data = ., x = n, y = n2, style = "jenks", dim = 3) %>%
        mutate(bi_class = case_when(bi_class == "NA-NA" ~ NA_character_, TRUE ~ bi_class))

# inspect
biscale_ee_sf
biscale_ee_sf %>% glimpse()
biscale_ee_sf %>% pull(bi_class) %>% tibble(bi_class = .) %>% distinct(bi_class)
inspect_biscale_ee_sf <- biscale_ee_sf
st_geometry(inspect_biscale_ee_sf) <- NULL
inspect_biscale_ee_sf %>% as_tibble() %>% count(n, n2, bi_class) %>% arrange(desc(n))
inspect_biscale_ee_sf %>% as_tibble() %>% count(bi_class)
?classIntervals
# https://en.wikipedia.org/wiki/Jenks_natural_breaks_optimization
# https://www.ehdp.com/methods/jenks-natural-breaks-explain.htm (note this seems to omit mention of last step from wikipedia)
classIntervals(var = inspect_biscale_ee_sf %>% as_tibble() %>% pull(n), n = 3, style = "jenks")
classIntervals(var = inspect_biscale_ee_sf %>% as_tibble() %>% pull(n2), n = 3, style = "jenks")


#/////////////////////////////////////


# create custom_palette, which must have class bi_pal_custom, and bi_class names in order to work with bi_scale_fill
# format of bi_class mapping to legend is: 
# first number = legend col counting up from left to right
# second number = legend row counting up from bottom to top
# see color blender: https://meyerweb.com/eric/tools/color-blend/#31688E:35B779:5:hex

# inspect palette
# note that stevens.greenblue is colorblind safe: https://nowosad.github.io/post/cbc-bp2/
stevens.greenblue()
bivcol <- function(pal){
        tit = substitute(pal)
        pal = pal()
        ncol = length(pal)
        image(matrix(seq_along(pal), nrow = sqrt(ncol)),
              axes = FALSE, 
              col = pal, 
              asp = 1)
        mtext(tit)
}
bivcol(pal = stevens.greenblue)
biscale_palette <- data.frame(matrix(seq_along(stevens.greenblue()), nrow = sqrt(9))) %>% as_tibble() 
biscale_palette

# create custom_palette
custom_palette <- c("1-1" = stevens.greenblue()[biscale_palette[1, 1] %>% pull()], 
                    "1-2" = stevens.greenblue()[biscale_palette[1, 2] %>% pull()],
                    "1-3" = stevens.greenblue()[biscale_palette[1, 3] %>% pull()],
                    "2-1" = stevens.greenblue()[biscale_palette[2, 1] %>% pull()],
                    "2-2" = stevens.greenblue()[biscale_palette[2, 2] %>% pull()],
                    "2-3" = stevens.greenblue()[biscale_palette[2, 3] %>% pull()],
                    "3-1" = stevens.greenblue()[biscale_palette[3, 1] %>% pull()],
                    "3-2" = stevens.greenblue()[biscale_palette[3, 2] %>% pull()],
                    "3-3" = stevens.greenblue()[biscale_palette[3, 3] %>% pull()])
custom_palette
class(custom_palette)
class(custom_palette) <- "bi_pal_custom"
class(custom_palette)


#/////////////////////////////////////


# convert crs to 3857
biscale_ee_sf_3857 <- st_transform(biscale_ee_sf, 3857)
st_crs(biscale_ee_sf_3857)

# create map
biscale_output_map <- ggmap(test_map_ee) + 
        coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
        geom_sf(data = biscale_ee_sf_3857, inherit.aes = FALSE, mapping = aes(fill = bi_class), 
                color = "#ffffff", size = .5) +
        bi_scale_fill(pal = "DkBlue", dim = 3, na.translate = FALSE) +
        # scale_alpha_discrete(range = c(.5, 1.5)) +
        theme_bw() +
        theme(
                # plot.margin = unit(c(0, 10, 0, 0), "mm"),
                panel.grid.major = element_line(color = "transparent"),
                plot.background = element_blank(), 
                panel.grid.minor = element_blank(), panel.border = element_blank(),
                axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
                axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
                # plot.title = element_text(size = 6, face = "bold", hjust = 0, family = "Trebuchet MS"), 
                plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                            color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
                legend.position = "none",
                legend.key.size = unit(2, "mm"),
                legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")), 
                legend.title = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
                legend.justification = "center",
                panel.grid = element_blank(),
                line = element_blank(),
                rect = element_blank(),
                text = element_blank()) +
        guides(fill = guide_legend(reverse = FALSE, 
                                   # title = "Fake data\nvalues", 
                                   title.hjust = .5,
                                   keywidth = 2,
                                   nrow = 2, byrow = TRUE))

# inspect
biscale_output_map

# create legend
legend <- bi_legend(pal = "DkBlue", dim = 3, xlab = "Higher n ", ylab = "Higher n2 ", size = 8)

# add legend
final_biscale_output_map <- ggdraw() +
        draw_plot(biscale_output_map, x = 0, y = 0, width = .8, height = .8) +
        draw_plot(legend, x = 0.8, y = .3, width = 0.2, height = 0.2)

# inspect
final_biscale_output_map


#///////////////////


# save map as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(final_biscale_output_map)
dev.off()

# add emf to word doc - will manually crop map in word doc 
# i can crop with magick, but then there is image quality issues from png to ggplot roundtrip
# note when saving from word to pdf, you'll need to print -> as pdf, 
# instead of save_as -> pdf, since save_as has issues rendering emfs
read_docx() %>% 
        body_add_img(src = filename, width = 7, height = 7) %>% 
        print(target = "ee_biscale_output_map.docx")


#/////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////////////


# still experimental
# convert map to png to get smaller file size, and can crop as necessary, then annotate further in ggplot

# https://cran.r-project.org/web/packages/cowplot/vignettes/introduction.html
# https://cran.r-project.org/web/packages/magick/vignettes/intro.html

# logo_file <- system.file("extdata", "logo.png", package = "cowplot")
# logo_file
# ggdraw() + draw_image(logo_file, scale = 0.5)
# 
# ggdraw(p) + 
#         draw_image(image_file, x = 1, y = 1, hjust = 1, vjust = 1, width = 0.5, height = 0.5)
# 
# ggdraw() + 
#         draw_image(image_file, x = 1, y = 1, hjust = 1, vjust = 1, width = 0.5, height = 0.5)

image <- biscale_output_map
image
ggsave(file="output/map/biscale_output_map.svg", plot = image, width = 36, height = 24, limitsize = FALSE)

map <- image_read_svg("output/map/biscale_output_map.svg")
map
map %>% image_info()

# crop map if needed
# map <- image_crop(image = map, geometry = geometry_area(width = 2300, height = 1600, x_off = 50, y_off = 50))
# map

image_write(image = map, path = "output/map/map.png", format = "png")

image_file <- "C:/Users/Stephen/Desktop/usaid/mcp/tso_portfolio_reviews/democracy_and_governance/output/map/map.png"
ggdraw() %>% draw_image(image = image_file, width = .5, height = .5)

# https://ggplot2.tidyverse.org/reference/annotation_custom.html
base <- ggplot() +
        geom_blank() +
        theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
        )
base

ggdraw(base) + draw_image(image = image_file, x = 1, y = 1, hjust = 1, vjust = 1, width = .75, height = .75)


#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////////////////////////////




# natural earth also has data on internal country administrative boundaries 
# https://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-admin-1-states-provinces/
# this seems to be aggregated from the Berkely GADM project
# https://gadm.org/data.html
# https://blog.revolutionanalytics.com/2009/10/geographic-maps-in-r.html

# read in country level-0 admin data as sf object
# note country level-0 is country external boundary
hungary_level_0 <- readRDS(file = "gadm36_HUN_0_sf.rds")
hungary_level_0
hungary_level_0 %>% class()
hungary_level_0 %>% glimpse()


#////////////////////


# plot
hungary_level_0 %>% 
        ggplot() +
        geom_sf() +
        theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              # plot.title = element_text(size = 6, face = "bold", hjust = 0, family = "Trebuchet MS"), 
              plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                          color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
              legend.position = "right",
              legend.key.size = unit(2, "mm"),
              legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")), 
              legend.title = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + 
        guides(fill = guide_legend(title.hjust = .5, title.vjust = 1.5, reverse = TRUE, keyheight = 1, label.vjust = 1.42))


#////////////////////////////////////////////////////////////////////////////////////////


# read in country level-1 admin data as sf object
hungary_level_1 <- readRDS(file = "gadm36_HUN_1_sf.rds")
hungary_level_1
hungary_level_1 %>% class()
hungary_level_1 %>% glimpse()

# note the TYPE_1 field denotes the local language administrative boundary name
# and the ENGTYPE_1 field provides the corresponding english admin boundary name
tibble(type = hungary_level_1 %>% pull(TYPE_1),
       eng_type = hungary_level_1 %>% pull(ENGTYPE_1)) %>% count(type, eng_type)


#////////////////////


# plot
hungary_level_1 %>% 
        ggplot() +
        geom_sf() +
        theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              # plot.title = element_text(size = 6, face = "bold", hjust = 0, family = "Trebuchet MS"), 
              plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                          color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
              legend.position = "right",
              legend.key.size = unit(2, "mm"),
              legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")), 
              legend.title = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + 
        guides(fill = guide_legend(title.hjust = .5, title.vjust = 1.5, reverse = TRUE, keyheight = 1, label.vjust = 1.42))


#//////////////////////////////////////////////////////////////////////////////////


# read in country level-2 admin data as sf object
hungary_level_2 <- readRDS(file = "gadm36_HUN_2_sf.rds")
hungary_level_2
hungary_level_2 %>% class()
hungary_level_2 %>% glimpse()

# note the TYPE_1 field denotes the local language administrative boundary name
# and the ENGTYPE_1 field provides the corresponding english admin boundary name
tibble(type = hungary_level_2 %>% pull(TYPE_2),
       eng_type = hungary_level_2 %>% pull(ENGTYPE_2)) %>% count(type, eng_type)


#////////////////////


# plot
hungary_level_2 %>% 
        ggplot() +
        geom_sf() +
        theme_bw() +
        theme(panel.grid.major = element_line(color = "transparent"),
              plot.background = element_blank(), 
              panel.grid.minor = element_blank(), panel.border = element_blank(),
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              # plot.title = element_text(size = 6, face = "bold", hjust = 0, family = "Trebuchet MS"), 
              plot.caption = element_text(hjust = 0, size = 11, face = "plain", family = "Calibri", 
                                          color = "#000000", margin = margin(t = 0, r = 0, b = 0, l = 0)),
              legend.position = "right",
              legend.key.size = unit(2, "mm"),
              legend.text = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")), 
              legend.title = element_text(size = 12, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
              panel.grid = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              text = element_blank()) + 
        guides(fill = guide_legend(title.hjust = .5, title.vjust = 1.5, reverse = TRUE, keyheight = 1, label.vjust = 1.42))
