library(tidyverse)
library(osmdata)
library(sf)
library(tmap)

# ----------------------------- Home ----------------------------- #

home <- st_sfc(st_point(c(-0.160912, 51.491979)), crs = 4326)

# buffer for circular map
home_buffer <- home %>% 
  st_transform(27700) %>% # convert to uk projection (metres)
  st_buffer(1500) %>%  # buffer x metres
  st_transform(4326) # transform back


# ----------------------------- Green Spaces ----------------------------- #

# 1. PARKS
parks <- opq ("london uk") %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf()

#  To explore / get park names
parks_polygon <- parks$osm_polygons
parks_multipolygon <- parks$osm_multipolygons


# we already queried all parks in London. Convert then to one geometry
parks_polygon <- st_union(parks_polygon)
# handle bad geometries
parks_circular <- st_make_valid(parks_polygon)

# this gets all the polygons from parks_circular that are in the circular buffer
parks_circular <- st_intersection(x=home_buffer, y=parks_circular)

plot(st_geometry(parks_circular))
# ----------------------------- Waterways ----------------------------- #

# get the Thames
waterways <- opq ("london uk") %>%
  add_osm_feature(key = "water", value = "river") %>%
  osmdata_sf()

# here you have to check if it is a polygon, multipolygon, or line. Just try all until you get 
# something that looks right. In this case, the Thames is a multipolygon
plot(st_geometry(waterways$osm_multipolygons))

# Convert it to one geometry, for plotting
waterways_circular <- st_union(waterways$osm_multipolygons)
# this gets all the waterways from 'waterways' that are in the circular buffer
waterways_circular <- st_intersection(x=home_buffer, y=waterways_circular)

plot(st_geometry(waterways_circular))

# ----------------------------- Roads ----------------------------- #

# get roads
roads_circular <- opq (bbox = st_bbox(home_buffer)) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()    

roads_circular <- roads_circular$osm_lines

# remove roads in the parks
sel_sgbp <- st_intersects(x=roads_circular, y=parks_circular)
sel_logical <- lengths(sel_sgbp) == 0
roads_circular <- roads_circular[sel_logical, ]

# remove roads outside of circular buffer
sel_sgbp <- st_within(x=roads_circular, y=home_buffer)
sel_logical <- lengths(sel_sgbp) > 0
roads_circular <- roads_circular[sel_logical, ]

# see the unique road types
roads_circular %>% st_drop_geometry %>% distinct(highway)

# give a thickness value to each road type, for plotting
roads_circular <- roads_circular %>% 
  mutate(thickness = case_when(highway %in% c("motorway", "motorway_link")  ~ 5,
                               highway %in% c("trunk", "trunk_link") ~ 4,
                               highway %in% c("primary", "primary_link") ~ 3.5,
                               highway %in% c("secondary", "secondary_link") ~ 3.2,
                               highway %in% c("tertiary", "tertiary_link") ~ 2.8,
                               highway == 'residential' ~ 2.2,
                               highway == "cycleway" ~ 2,
                               # Last argument is a catch all argument. if 'no 'highway doesn''t match any of the above arguments
                               TRUE ~ 1.5)) 


# ----------------------------- Buildings ----------------------------- #

buildings <- opq (bbox = (bbox = st_bbox(home_buffer))) %>%
  add_osm_feature(key = "building") %>%
  osmdata_sf()    

plot(st_geometry(buildings$osm_polygons))

# buildings are the polygon layer
buildings <- buildings$osm_polygons
plot(st_geometry(buildings))

# remove buildings in the parks
sel_sgbp <- st_intersects(x=buildings, y=parks_circular)
sel_logical <- lengths(sel_sgbp) == 0
buildings <- buildings[sel_logical, ]

# remove buildings outside of circular buffer
sel_sgbp <- st_within(x=buildings, y=home_buffer)
sel_logical <- lengths(sel_sgbp) > 0
buildings <- buildings[sel_logical, ]


# combine into one geometry for plotting
buildings <- st_union(buildings)
plot(st_geometry(buildings))

# ----------------------------- Map ----------------------------- #

tm_shape(home_buffer) +
  tm_polygons(col = '#f5f2d0', 
              alpha = 0.1, 
              border.col = 'grey50',
              border.alpha = 0.5,
              lwd = 2) +
tm_shape(waterways_circular) +
  tm_fill(col = '#4682b4',
           alpha = 0.8) +
tm_shape(roads_circular) +
  tm_lines(col = 'darkred',
           lwd = 'thickness',
           scale = 2,
           alpha = 0.6) +
tm_shape(buildings) +
  tm_fill(col = 'grey80',
          alpha = 0.8) +
tm_shape(parks_circular) +
  tm_fill(col = '#008080',
          alpha = 0.4) +
tm_shape(home) +
  tm_dots(size = 0.15, 
          alpha = 0.8,
          col = "#1034a6") +
  tm_layout(#main.title = "Sloane Square",
            #main.title.color = "grey50",
            title = "7 Culford Gardens",        
            title.size = 1,
            title.color = "azure4",
            title.position = c("left", "bottom"),
            legend.show = FALSE,
            fontfamily = 'Georgia',
            fontface = 3,
            frame.double.line =  TRUE,
            frame = 'grey50') -> map_buildings

map_buildings

# save
tmap_save(map_buildings, filename="culford_gardens.png", dpi=1000) 
