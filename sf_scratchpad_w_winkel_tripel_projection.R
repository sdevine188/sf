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

# https://wilkelab.org/practicalgg/articles/Winkel_tripel.html
# note that getMap uses natural earth for country boundaries, but fixes several issues, so better to pull from rworldmap
# (see rworldmap docs: https://cran.r-project.org/web/packages/rworldmap/rworldmap.pdf)
# ?countriesCoarse
# ?countriesCoarseLessIslands

# another option is rnaturalearth package: https://docs.ropensci.org/rnaturalearth/

# a good site on gis in r: https://www.jessesadler.com/post/gis-with-r-intro/

# tutorial on ggsn for scale and north arrow: http://oswaldosantos.github.io/ggsn/


# setwd
setwd("C:/Users/Stephen/Desktop/R/sf")

# get world map as sf object
world_sf <- st_as_sf(getMap(resolution = "low"))
world_sf
world_sf %>% glimpse()
world_sf %>% class()

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


# join world_sf with data of interest

# create fake_data
fake_data <- world_sf %>% distinct(SOVEREIGNT) %>% pull(SOVEREIGNT) %>% 
        tibble(SOVEREIGNT = .) %>% mutate(SOVEREIGNT = as.character(SOVEREIGNT)) %>% distinct(SOVEREIGNT) 
fake_data <- fake_data %>% mutate(n = sample(x = seq(from = 0, to = 10000, by = 1), size = nrow(fake_data))) %>%
        mutate(n = case_when(row_number() %in% 1:20 ~ NA_real_, row_number() %in% 21:40 ~ 5, TRUE ~ n),
               SOVEREIGNT = factor(SOVEREIGNT))
fake_data %>% arrange(n)
fake_data %>% arrange(desc(n))
fake_data %>% ggplot(data = ., aes(x = n)) + geom_histogram()

# join data of interest with world_sf
world_sf <- world_sf %>% left_join(., fake_data, by = "SOVEREIGNT")
world_sf %>% glimpse()


#/////////////////////////////////////////////////////////////////////////////////////


# get color_palette
display.brewer.pal(n = 9, name = "Blues")
color_palette <- brewer.pal(n = 9, name = "Blues") %>% tibble(hex = .)
color_palette

# add fill_color_bin and fill_color
world_sf <- world_sf %>% 
        mutate(fill_color_bin = case_when(is.na(n) ~ "0_to_1",
                                          n > 0 & n < 50 ~ "1_to_50",
                                          n >= 50 & n < 150 ~ "50_to_150",
                                          n >= 150 & n < 1500 ~ "150_to_1500",
                                          n >= 1500 & n < 5000 ~ "1500_to_5000",
                                          n >= 5000 ~ "5000_to_130000"),
               fill_color = case_when(fill_color_bin == "0_to_1" ~ "#ffffff",
                                      fill_color_bin == "1_to_50" ~ color_palette %>% slice(3) %>% pull(hex),
                                      fill_color_bin == "50_to_150" ~ color_palette %>% slice(5) %>% pull(hex),
                                      fill_color_bin == "150_to_1500" ~ color_palette %>% slice(7) %>% pull(hex),
                                      fill_color_bin == "1500_to_5000" ~ color_palette %>% slice(8) %>% pull(hex),
                                      fill_color_bin == "5000_to_130000" ~ color_palette %>% slice(9) %>% pull(hex)))

# inspect
world_sf
world_sf %>% distinct(SOVEREIGNT, fill_color_bin, fill_color) %>% count(fill_color_bin, fill_color)
world_sf %>% distinct(SOVEREIGNT, n) %>% filter(is.na(n))        


#///////////


# create fill_color_list for to pass to scale_color_manual
world_fill_color_list <- world_sf %>% count(fill_color_bin, fill_color) %>% pull(fill_color)
names(world_fill_color_list) <- world_sf %>% count(fill_color_bin, fill_color) %>% pull(fill_color_bin)
world_fill_color_list


#/////////////////////////////////////////////////////////////////////////////////////


# import extra fonts from extrafont package
windowsFonts()
# font_import()
# loadfonts(device = "win")
windowsFonts()
fonttable()
fonts()


#/////////////////////////////////////////////////////////////////////////////////////

# plot
# note that basic plots like this can be overlaid with bounding box to show global location as inset for zoomed in local map
# using annotate() you can add a rect geom; note that using geom_rect has issues since it inherits the default dataframe
# passed to ggplot, so you either need to create a seperate dataframe, or other workarounds
# annotate is more elegant and general approach, since you can use it also to add other geoms incl. segments, points, text, etc 
# https://ggplot2.tidyverse.org/reference/annotate.html
# note that annotate is tied to the plot scale though, whereas ggdraw can literally draw anywhere on a plot using 0 to 1 x/y pct
world_sf %>% 
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


# plot using winkel tripel projection by first transforming projection
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


# plot with selected regions only
region_map <- world_sf %>% st_transform_proj(crs = "+proj=wintri +datum=WGS84 +no_defs +over") %>% 
        filter(REGION %in% c("Europe", "Asia")) %>%
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


# plot with selected countries only
world_sf %>% filter(str_detect(string = SOVEREIGNT, pattern = regex("hungary", ignore_case = TRUE))) %>% pull(SOVEREIGNT)

# plot 
countries_map <- world_sf %>% st_transform_proj(crs = "+proj=wintri +datum=WGS84 +no_defs +over") %>% 
        filter(SOVEREIGNT %in% c("Kosovo", "Croatia", "Bosnia and Herzegovina", "Montenegro", "Republic of Serbia", 
                                 "Albania", "Hungary")) %>%
        ggplot(aes(fill = factor(fill_color_bin, 
                                 levels = c("0_to_1", "1_to_50", "50_to_150", "150_to_1500", "1500_to_5000", "5000_to_130000")))) + 
        geom_sf(color = "#a6a6a6", size = 0.05) +
        scale_fill_manual(values = world_fill_color_list, name = " \n ",
                          labels = c(1, 50, 150, comma(1500), comma(5000), comma(130000))) +
        coord_sf(datum = NULL) +
        # note that compass looks ok in r studio, but when output to emf it gets corrupted; will use cowplot instead
        # north(symbol = 16, x.min = 1950000, x.max = 1850000, y.min = 5100000, y.max = 5300000, scale = 1.5) +
        # scalebar(data = world_sf, dist = 100, dist_unit = "km", transform = FALSE, model = "WGS84", 
        #          anchor = c(x = 1950000, y = 4600000), height = 100, st.size = 5, st.dist = 200, st.bottom = TRUE) +
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
        st_transform_proj(crs = "+proj=wintri +datum=WGS84 +no_defs +over") %>%
        ggplot() +
        geom_sf() +
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
        st_transform_proj(crs = "+proj=wintri +datum=WGS84 +no_defs +over") %>%
        ggplot() +
        geom_sf() +
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
        st_transform_proj(crs = "+proj=wintri +datum=WGS84 +no_defs +over") %>%
        ggplot() +
        geom_sf() +
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


