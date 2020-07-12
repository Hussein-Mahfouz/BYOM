library(tidyverse)
library(osmdata)
library(sf)
library(tmap)
library(grid)

############################## Get Green Spaces ##############################

# 1. PARKS
parks <- opq ("london uk") %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf()

parks_polygon <- parks$osm_polygons
plot(st_geometry(parks_polygon))
parks_multipolygon <- parks$osm_multipolygons
plot(st_geometry(parks_multipolygon))

# Victoria Park
parks_vic <- parks_multipolygon %>% 
  filter(name == 'Victoria Park') %>%
  select(osm_id, name)
plot(st_geometry(parks_vic))

# Mile-end Park
parks_mile_end <- parks_multipolygon %>% 
  filter(name == 'Mile End Park') %>%
  select(osm_id, name)
plot(st_geometry(parks_mile_end))

# Hampstead Heath
parks_hampstead <- parks_polygon %>% 
  filter(name %in% c("Hampstead Heath", "Hampstead Heath Extension")) %>%
  select(osm_id, name)
plot(st_geometry(parks_hampstead))

# Highbury Fields
parks_highbury_fields <- parks_polygon %>% 
  filter(name == 'Highbury Fields') %>% 
  select(osm_id, name)
plot(st_geometry(parks_highbury_fields))

# London Fields
parks_london_fields <- parks_polygon %>% 
  filter(name == 'London Fields') %>%
  select(osm_id, name)
plot(st_geometry(parks_london_fields))

# Haggerston Park
parks_haggerston <- parks_polygon %>% 
  filter(name == 'Haggerston Park') %>%
  select(osm_id, name)
plot(st_geometry(parks_haggerston))

# Hackney Marshes
parks_hackney_marshes <- parks_polygon %>% 
  filter(name == 'Hackney Marshes') %>%
  select(osm_id, name)
plot(st_geometry(parks_hackney_marshes))

# Queen Elizabeth Olympic Park
parks_olympic <- parks_multipolygon %>% 
  filter(name == 'Queen Elizabeth Olympic Park') %>%
  select(osm_id, name)
plot(st_geometry(parks_olympic))

# 2. WOODLANDS 
woodland <- opq ("london uk") %>%
  add_osm_feature(key = "natural", value = "wood") %>%
  osmdata_sf()

woodland_polygon <- woodland$osm_polygons
plot(st_geometry(woodland_polygon))
woodland_multipolygon <- woodland$osm_multipolygons
plot(st_geometry(woodland_multipolygon))

# Epping Forest
parks_epping <- woodland_multipolygon %>% 
  filter(name == 'Epping Forest') %>%
  select(osm_id, name)
plot(st_geometry(parks_epping))

# Queens Wood
parks_queenswood <- woodland_polygon %>% 
  filter(name == "Queen’s Wood") %>%
  select(osm_id, name)
plot(st_geometry(parks_queenswood))


# 3. LANDUSE (For Wick Woodland)
landuse <- opq ("london uk") %>%
  add_osm_feature(key = "landuse", value = "forest") %>%
  osmdata_sf()

landuse_polygon <- landuse$osm_polygons
plot(st_geometry(woodland_polygon))

# Wick Woodland
parks_wickwoodland <- landuse_polygon %>% 
  filter(name == "Wick Community Woodland") %>%
  select(osm_id, name)
plot(st_geometry(parks_wickwoodland))

# Combine all into one geometry
parks_all <- rbind(parks_epping, parks_haggerston, parks_hampstead, parks_highbury_fields,
                   parks_london_fields, parks_mile_end, parks_vic, parks_hackney_marshes,
                   parks_olympic, parks_wickwoodland, parks_queenswood)

plot(st_geometry(parks_all))

# I need some vertical space at the top to add epping forest incet map 
# Add Walthamstow wetlnds then crop cycleways to this geometry
leisure <- opq ("london uk") %>%
  add_osm_feature(key = "leisure", value = "nature_reserve") %>%
  osmdata_sf()
leisure_polygon <- leisure$osm_polygons

parks_walthamstow <- leisure_polygon %>% 
  filter(name == "Walthamstow Wetlands") %>%
  select(osm_id, name)
plot(st_geometry(parks_walthamstow))

# this buffer will be used to crop the cycleways (when we don't want epping forest)
parks_all_buffer <- rbind(parks_haggerston, parks_hampstead, parks_highbury_fields,
                          parks_london_fields, parks_mile_end, parks_vic, parks_hackney_marshes,
                          parks_olympic, parks_wickwoodland, parks_queenswood, parks_walthamstow)
  
  
  
  
rm(parks, parks_haggerston, parks_hampstead, parks_highbury_fields, parks_london_fields,
   parks_mile_end, parks_multipolygon, parks_polygon, parks_vic, parks_queenswood, parks_olympic, parks_wickwoodland,
   parks_hackney_marshes, parks_walthamstow, woodland, woodland_multipolygon, woodland_polygon, landuse, 
   landuse_polygon, leisure, leisure_polygon)

############################## Get Cycle Lanes ##############################

# Data on Main Routes (Cycleways, Quietways etc)
cycleways <- st_read("data-raw/CycleRoutes.json")
# All roads with cycling infrastructure (more comprehensive)
cycle_infra <- st_read("data-raw/cycle_lane_track.json")
plot(st_geometry(cycleways))
plot(st_geometry(cycle_infra), add = TRUE, col='grey')
plot(st_geometry(parks_all), add=TRUE, col='green')

# crop to the extent of the parks sf. That is all I need for the map
cycleways <- st_crop(cycleways, st_bbox(parks_all_buffer))
cycle_infra <- st_crop(cycle_infra, st_bbox(parks_all_buffer))

# Remove cycleways that are inside parks (for aesthetic purposes)
sel_sgbp <- st_intersects(x=cycleways, y=parks_all_buffer)
sel_logical <- lengths(sel_sgbp) == 0
cycleways_hollow <- cycleways[sel_logical, ]
# combine into one geometry for faster plotting
cycleways_hollow <- st_union(cycleways_hollow)

# Remove cycle infra that is inside parks (for aesthetic purposes)
sel_sgbp <- st_intersects(x=cycle_infra, y=parks_all_buffer)
sel_logical <- lengths(sel_sgbp) == 0
cycle_infra_hollow <- cycle_infra[sel_logical, ]
# combine into one geometry for faster plotting
cycle_infra_hollow <- st_union(cycle_infra_hollow)


# epping forest is pretty far. I will remove it then try and add it closer using the grid package
parks_no_epping <- parks_all %>% filter(name != 'Epping Forest')


############################ Points ############################
# R's home
home <- st_sfc(st_point(c(-0.026530, 51.537060)), crs = 4326) # %>% st_set_crs(4326)


############################ Roads ############################

roads <- opq (bbox = st_bbox(parks_all_buffer)) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()
roads <- roads$osm_lines
# remove roads in parks
sel_sgbp <- st_intersects(x=roads, y=parks_all)
sel_logical <- lengths(sel_sgbp) == 0
roads <- roads[sel_logical, ]

# combine into one geometry for faster plotting
roads <- st_union(roads)

# same but for epping forest incet map
roads_epping <- opq (bbox = st_bbox(parks_epping)) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()
roads_epping <- roads_epping$osm_lines

sel_sgbp <- st_intersects(x=roads_epping, y=parks_epping)
sel_logical <- lengths(sel_sgbp) == 0
roads_epping <- roads_epping[sel_logical, ]

roads_epping <- st_union(roads_epping)


rm(sel_logical, sel_sgbp)

############################################### MAP ##########################################

tm_shape(cycle_infra_hollow) +
  tm_lines(col = '#2B22AA',
           lwd = 1.5,
           alpha = 0.3) +
tm_shape(cycleways_hollow) +
  tm_lines(col = '#2B22AA',
           lwd = 1.5,
           alpha = 0.3) +
tm_shape(parks_no_epping) +
  tm_fill(col = 'palegreen3') +
tm_shape(home) +
  tm_dots(size = 0.8, 
          alpha = 0.8,
          col = "#2B22AA") +
tm_layout(title = "Life Under Lockdown",        
            title.size = 1.2,
            title.color = "azure4",
            title.position = c("left", "bottom"),
            title.bg.color = 'white',
            title.bg.alpha = 0.5,
            fontfamily = 'Georgia',
            frame = FALSE) -> map_base

#epping forest
tm_shape(parks_epping) +
  tm_fill(col = 'palegreen3')  +
tm_shape(roads_epping) +
  tm_lines(col = 'grey70',
           lwd = 1,
           alpha = 0.25) +
  tm_layout(frame.lwd =  4,
            frame = "grey50")-> map_epping

# to check
map_base
print(map_epping, vp = grid::viewport(x= 0.9, y = 0.8, width= 0.4, height= 0.4))

# Save
tmap_save(map_base, insets_tm = map_epping, 
          insets_vp=viewport(x= 0.9, y = 0.8, width= 0.4, height= 0.4), 
          filename="lockdown.png", dpi=600)


############################################### MAP WITH ROADS ##########################################

tm_shape(roads) +
  tm_lines(col = 'grey70',
           lwd = 1,
           alpha = 0.25) +
tm_shape(cycle_infra_hollow) +
  tm_lines(col = '#2B22AA',
           lwd = 1.6,
           alpha = 0.4) +
  tm_shape(cycleways_hollow) +
  tm_lines(col = '#2B22AA',
           lwd = 1.6,
           alpha = 0.4) +
  tm_shape(parks_no_epping) +
  tm_fill(col = 'palegreen3') +   ##5DBB63   #008080
  tm_shape(home) +
  tm_dots(size = 0.1, 
          alpha = 0.8,
          col = "brown") +
  tm_layout(title = "Cycling Through Lockdown",        
            title.size = 1,
            title.color = "azure4", #grey55
            title.position = c("left", "bottom"),
            title.bg.color = 'white',
            title.bg.alpha = 0.5,
            fontfamily = 'Georgia',
            frame = FALSE) -> map_roads

# Save
tmap_save(map_roads, insets_tm = map_epping, 
          insets_vp=viewport(x= 0.9, y = 0.8, width= 0.4, height= 0.4), 
          filename="lockdown_roads.png", dpi=1000)


############################################### MAP WITH ROADS COLORED DIFFERENTLY ##########################################
#epping forest
tm_shape(parks_epping) +
  tm_fill(col = '#008080')  +
  tm_shape(roads_epping) +
  tm_lines(col = '#2B22AA',
           lwd = 1,
           alpha = 0.2) +
  tm_layout(frame.lwd =  4,
            frame = "grey50")-> map_epping_inv


tm_shape(roads) +
  tm_lines(col = '#2B22AA',
           lwd = 1,
           alpha = 0.2) +
  tm_shape(cycle_infra_hollow) +
  tm_lines(col = 'grey40',
           lwd = 1.6,
           alpha = 0.7) +
  tm_shape(cycleways_hollow) +
  tm_lines(col = 'grey40',
           lwd = 2,
           alpha = 0.7) +
  tm_shape(parks_no_epping) +
  tm_fill(col = '#008080') +   ##5DBB63   #008080
  tm_shape(home) +
  tm_dots(size = 0.15, 
          alpha = 0.8,
          col = "brown") +
  tm_layout(title = "Cycling Through Lockdown",        
            title.size = 1,
            title.color = "azure4", #grey55
            title.position = c("left", "bottom"),
            title.bg.color = 'white',
            title.bg.alpha = 0.5,
            fontfamily = 'Georgia',
            frame = FALSE) -> map_roads_inv

# Save
tmap_save(map_roads_inv, insets_tm = map_epping_inv, 
          insets_vp=viewport(x= 0.9, y = 0.8, width= 0.4, height= 0.4), 
          filename="lockdown_roads_inv.png", dpi=1000)


