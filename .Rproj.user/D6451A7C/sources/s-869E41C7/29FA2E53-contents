library(tidyverse)
library(raster)
library(sp)
library(rayshader)

# download data from here   # https://dwtkns.com/srtm/

# load in the data
sinai <- raster("srtm_43_07.tif")
#check crs
sinai

plot(sinai)

# crop to the desired area

# st catherine (28.555, 33.976)
# mt sinai (28.539, 33.975)

e <- as(extent(33.67, 34.33, 28.18, 28.88), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
south_sinai <- crop(sinai, e)

plot(south_sinai)

#convert it to a matrix using rayshader
elmat = raster_to_matrix(south_sinai)


#Use one of rayshader's built-in textures:
elmat %>%
     sphere_shade(texture = "desert") %>%
     add_shadow(ray_shade(elmat,sunaltitude=20, zscale=20),max_darken=0.2) %>%
     #add_shadow(ambient_shade(elmat, zscale = 50), 0.5) %>%
     plot_3d(elmat, zscale = 20, fov = 0, theta = 90, zoom = 1.1, phi = 20, windowsize = c(1000, 800),
             baseshape = "circle")
#render_label(elmat, x = 390, y = 10, z = 12000, zscale = 50,
#             text = "St. Katherine", textsize = 1, linewidth = 2)
render_snapshot()


# OR GIF
# functions from this tutorial (https://wcmbishop.github.io/rayshader-demo/)
source("functions.R")

n_frames <- 90
thetas <- transition_values(from = -45, to = -135, steps = n_frames)
phis <- transition_values(from = 10, to = 30, steps = n_frames)
# generate gif
zscale <- 30
elmat %>% 
  sphere_shade(texture = "desert", zscale = zscale) %>%
  add_shadow(ambient_shade(elmat, zscale = zscale), 0.5) %>%
  add_shadow(ray_shade(elmat, zscale = zscale, lambert = TRUE), max_darken=0.3) %>%
  save_3d_gif(elmat, file = "sinai2.gif", duration = 20,
              solid = TRUE, shadow = TRUE, zscale = zscale,
              theta = thetas, phi = phis)





