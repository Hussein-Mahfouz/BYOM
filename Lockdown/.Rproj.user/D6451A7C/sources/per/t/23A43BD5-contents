library(tidyverse)
library(osmdata)
library(sf)
library(tmap)

############################## Get Green Spaces ##############################

# 1. PARKS
parks <- opq ("london uk") %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf()

parks_polygon <- parks$osm_polygons
# Hampstead Heath
parks_hampstead <- parks_polygon %>% 
  filter(name %in% c("Hampstead Heath", "Hampstead Heath Extension")) %>%
  select(osm_id, name)
plot(st_geometry(parks_hampstead))

############################## Get Buildings ##############################

#c(xmin, ymin, xmax, ymax)
buildings <- opq (bbox = c(-0.194493, 51.550838, -0.137189, 51.574424)) %>%
  add_osm_feature(key = "building") %>%
  osmdata_sf()    

buildings <- st_union(buildings$osm_polygons)

############################## Get Roads ##############################

roads_barrington <- opq (bbox = c(-0.194493, 51.550838, -0.137189, 51.574424)) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()
roads_barrington <- roads_barrington$osm_lines

# remove roads in the park
sel_sgbp <- st_intersects(x=roads_barrington, y=parks_hampstead)
sel_logical <- lengths(sel_sgbp) == 0
roads_barrington <- roads_barrington[sel_logical, ]

roads_barrington <- st_union(roads_barrington)
     
############################## Get Home ##############################
# 2 barrington court
barrington <- st_sfc(st_point(c(-0.15187906, 51.55327852)), crs = 4326) # %>% st_set_crs(4326)        
     

# tm_shape(buildings) +
#   tm_polygons(border.col  = 'grey40',
#               col = 'grey70',
#               lwd = 1,
#               alpha = 0.25) +
tm_shape(roads_barrington) +
  tm_lines(col = 'grey80',
           lwd = 2,
           alpha = 0.4) +
  tm_shape(parks_hampstead) +
  tm_fill(col = '#008080',
           lwd = 1.6,
           alpha = 0.4) +
tm_shape(barrington) +
  tm_dots(size = 0.4, 
          alpha = 0.8,
          col = "brown") +
  tm_layout(title = "2 Barrington Court",        
            title.size = 1.5,
            title.color = "azure4", #grey55
            title.position = c("left", "bottom"),
            #title.bg.color = 'white',
            #title.bg.alpha = 0.5,
            bg.color = 'purple',
            fontfamily = 'Georgia',
            frame.double.line =  TRUE,
            frame = 'grey50') #-> map_home 


# save
tmap_save(map_home, filename="lockdown_roads.png", dpi=1000)           
           