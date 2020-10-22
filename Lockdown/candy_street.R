library(tidyverse)
library(osmdata)
library(sf)
library(tmap)

# ----------------------------- Home ----------------------------- #

home <- st_sfc(st_point(c(-0.026530, 51.537060)), crs = 4326)

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

# Victoria Park
parks_vic <- parks$osm_multipolygons %>% 
  filter(name == 'Victoria Park') %>%
  select(osm_id, name)
plot(st_geometry(parks_vic))


# Hackney Marshes
parks_hackney_marshes <- parks$osm_polygons %>% 
  filter(name == 'Hackney Marshes') %>%
  select(osm_id, name)
plot(st_geometry(parks_hackney_marshes))

# Queen Elizabeth Olympic Park
parks_olympic <- parks$osm_multipolygons %>% 
  filter(name == 'Queen Elizabeth Olympic Park') %>%
  select(osm_id, name)
plot(st_geometry(parks_olympic))

# Mile-end Park
parks_mile_end <- parks$osm_multipolygons %>% 
  filter(name == 'Mile End Park') %>%
  select(osm_id, name)
plot(st_geometry(parks_mile_end))

# London Fields
parks_london_fields <- parks$osm_polygons %>% 
  filter(name == 'London Fields') %>%
  select(osm_id, name)
plot(st_geometry(parks_london_fields))

# 2. LANDUSE (For Wick Woodland)
landuse <- opq ("london uk") %>%
  add_osm_feature(key = "landuse", value = "forest") %>%
  osmdata_sf()

# Wick Woodland
parks_wickwoodland <- landuse$osm_polygons %>% 
  filter(name == "Wick Community Woodland") %>%
  select(osm_id, name)
plot(st_geometry(parks_wickwoodland))

# Combine all into one geometry
parks_all <- rbind(parks_london_fields, parks_mile_end, parks_vic, parks_hackney_marshes,
                   parks_olympic, parks_wickwoodland)

plot(st_geometry(parks_all))

# make it valid, and then crop the park to the buffer extent
parks_circular <- st_make_valid(parks_all)
parks_circular <- st_intersection(x=home_buffer, y=parks_circular)
# plot to check
plot(st_geometry(parks_circular))


# ----------------------------- Waterways ----------------------------- #

# all water bodies
waterways <- opq ("london uk") %>%
  add_osm_feature(key = "waterway") %>%
  osmdata_sf()

# Convert then to one geometry
waterways_circular <- st_union(waterways$osm_lines)
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


tm_shape(home_buffer) +
  tm_polygons(col = '#f5f2d0', 
              alpha = 0.1, 
              border.col = 'grey50',
              border.alpha = 0.5,
              lwd = 2) +
tm_shape(waterways_circular) +
  tm_lines(col = '#4682b4',
           lwd = 2.8,
           alpha = 0.5) +
tm_shape(roads_circular) +
  tm_lines(col = 'grey80',
           lwd = 'thickness',
           alpha = 0.6) +
tm_shape(parks_circular) +
  tm_fill(col = '#008080',
           lwd = 1.6,
           alpha = 0.4) +
tm_shape(home) +
  tm_dots(size = 0.25, 
          alpha = 0.8,
          col = "brown") +
  tm_layout(main.title = "93 Candy St",
            main.title.color = "grey50",
            title = "What Else Would \nYou Be At?",        
            title.size = 1,
            title.color = "azure4",
            title.position = c("left", "bottom"),
            legend.show = FALSE,
            fontfamily = 'Georgia',
            fontface = 3,
            frame.double.line =  TRUE,
            frame = 'grey50') -> map_home

map_home
# save
tmap_save(map_home, filename="candy_street.png", dpi=1000) 


#c(xmin, ymin, xmax, ymax)
buildings <- opq (bbox = (bbox = st_bbox(home_buffer))) %>%
  add_osm_feature(key = "building") %>%
  osmdata_sf()    

plot(st_geometry(buildings$osm_polygons))


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



buildings <- st_union(buildings)
plot(st_geometry(buildings))

tm_shape(home_buffer) +
  tm_polygons(col = '#f5f2d0', 
              alpha = 0.1, 
              border.col = 'grey50',
              border.alpha = 0.5,
              lwd = 2) +
tm_shape(waterways_circular) +
  tm_lines(col = '#4682b4',
           lwd = 2.8,
           alpha = 0.5) +
tm_shape(roads_circular) +
  tm_lines(col = 'darkred',
           lwd = 'thickness',
           scale = 2,
           alpha = 0.5) +
tm_shape(buildings) +
  tm_fill(col = 'grey80',
           alpha = 0.8) +
tm_shape(parks_circular) +
  tm_fill(col = '#008080',
          lwd = 1.6,
          alpha = 0.4) +
tm_shape(home) +
  tm_dots(size = 0.2, 
          alpha = 0.8,
          col = "#1034a6") +
  tm_layout(main.title = "93 Candy St",
            main.title.color = "grey50",
            title = "Where Wudja \nBe Going?",        
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
tmap_save(map_buildings, filename="candy_street_build_road.png", dpi=1000) 
