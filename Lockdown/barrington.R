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

rm(sel_logical, sel_sgbp)
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
            #bg.color = 'purple',
            fontfamily = 'Georgia',
            frame.double.line =  TRUE,
            frame = 'grey50') #-> map_home 


# save
tmap_save(map_home, filename="barrington.png", dpi=1000) 



##################################### circular map ##################################### 
barrington_buffer <- barrington %>% 
  st_transform(27700) %>% # convert to uk projection (metres)
  st_buffer(1150) %>%  # buffer x metres
  st_transform(4326) # transform back
       

# --------------- get roads
roads_circular <- opq (bbox = st_bbox(barrington_buffer)) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()    

roads_circular <- roads_circular$osm_lines
# remove roads in the parks
sel_sgbp <- st_intersects(x=roads_circular, y=parks_polygon)
sel_logical <- lengths(sel_sgbp) == 0
roads_circular <- roads_circular[sel_logical, ]

# remove roads outside of circular buffer
sel_sgbp <- st_within(x=roads_circular, y=barrington_buffer)
sel_logical <- lengths(sel_sgbp) > 0
roads_circular <- roads_circular[sel_logical, ]

# convert to one geometry
roads_circular <- st_union(roads_circular)

# ---------------- get green space
# we already queried all parks in London. Convert then to one geometry
parks_circular <- st_union(parks_polygon)
# this gets all the polygons from parks_circular that are in the circular buffer
parks_circular <- st_intersection(x=barrington_buffer, y=parks_circular)

# CELIA
tm_shape(barrington_buffer) +
  tm_polygons(col = '#ffde42',  #marta    # celia     #amparo  # hussein: #FFE4B5
              alpha = 0.5, 
              border.col = 'grey50',
              border.alpha = 0.5,
              lwd = 2) +
tm_shape(roads_circular) +
  tm_lines(col = 'grey40',       #marta    # celia     #amparo
          lwd = 1.6,
          alpha = 0.7) +
tm_shape(barrington) +
  tm_dots(size = 0.4, 
          col = "brown") +
tm_shape(parks_circular) +
  tm_fill(col = '#008080') +
  tm_layout(title = "La Casa de \nLos Cocineros",
            title.size = 1.2,
            title.color = "azure4", #grey55
            title.position = c("left", "bottom"),
            fontfamily = 'Georgia',
            frame.double.line =  TRUE,
            frame = 'grey50') -> map_circular

tmap_save(map_circular, filename="barrington_celia.png", dpi=1000) 

# AMPARO
tm_shape(barrington_buffer) +
  tm_polygons(col = '#a8d5baff',  #c5e3bf 
              alpha = 0.3, 
              border.col = 'grey50',
              border.alpha = 0.5,
              lwd = 2) +
  tm_shape(roads_circular) +
  tm_lines(col = 'grey40',       #marta    # celia     #amparo
           lwd = 1.6,
           alpha = 0.7) +
  tm_shape(barrington) +
  tm_dots(size = 0.4, 
          col = "brown") +
  tm_shape(parks_circular) +
  tm_fill(col = '#008080') +
  tm_layout(title = "La Casa de \nLos Cocineros",
            title.size = 1.2,
            title.color = "azure4", #grey55
            title.position = c("left", "bottom"),
            fontfamily = 'Georgia',
            frame.double.line =  TRUE,
            frame = 'grey50') -> map_circular

tmap_save(map_circular, filename="barrington_amparo.png", dpi=1000) 

# MARTA
tm_shape(barrington_buffer) +
  tm_polygons(col = '#1034A6',  #marta    # celia     #amparo  # hussein: #FFE4B5
              alpha = 0.3, 
              border.col = 'grey50',
              border.alpha = 0.5,
              lwd = 2) +
  tm_shape(roads_circular) +
  tm_lines(col = 'grey40',     
           lwd = 1.6,
           alpha = 0.7) +
  tm_shape(barrington) +
  tm_dots(size = 0.4, 
          col = "brown") +
  tm_shape(parks_circular) +
  tm_fill(col = '#1034A6', 
          alpha = 0.9) +
  tm_layout(title = "La Casa de \nLos Cocineros",
            title.size = 1.2,
            title.color = "azure4", #grey55
            title.position = c("left", "bottom"),
            fontfamily = 'Georgia',
            frame.double.line =  TRUE,
            frame = 'grey50') -> map_circular

tmap_save(map_circular, filename="barrington_marta.png", dpi=1000) 


