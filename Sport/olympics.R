library(sf)
library(tidyr)
library(dplyr)
library(readr)
library(jsonlite)
library(ggplot2)
library(ggthemes)
library(gganimate)
library(maps)
library(transformr)

# Olympic Data
# https://docs.google.com/spreadsheets/d/1zeeZQzFoHE2j_ZrqDkVJK9eF7OH1yvg75c8S-aBcxaU/edit#gid=322436777

data = read_csv("Summer_Olympic_medallists_1896_2008.csv", skip = 4)
# read in world map as sf
world = st_read("world.geo.json")
#plot to check 
#plot(st_geometry(world))

#subset only necessary columns
world <- world[, c("sovereignt", "iso_a3")]

# GET STATS 

# function to calculate most frequent categorical value  https://exploratory.io/note/kanaugust/1701090969905358
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# For each year, get country with most medals and the matching no of medals
year_best = data %>% group_by(Edition) %>%
          summarise(best_country = calculate_mode(NOC),  #use function above to get most repeated country
                   medals = length(which(NOC==calculate_mode(NOC)))) # get count of medals for best country

# change names because the ISO standard is different to the data found
year_best$best_country[year_best$best_country=='GRE'] <- 'GRC'
year_best$best_country[year_best$best_country=='URS'] <- 'RUS'
year_best$best_country[year_best$best_country=='GER'] <- 'DEU'


# merge map with year_best
x <- merge(world, year_best)
# filter to get only the rows where the spatial data matches the winning country
map_winners <- x[x$iso_a3==x$best_country, ]

# create new column to display as ggplot title 
# cannot use different columns as title / subtitle. It will only display the column passed to transition_states()
# https://stackoverflow.com/questions/53864892/gganimate-include-additional-variable-other-than-states-level-variable-or-frame
map_winners <- mutate(map_winners, title_var2 = factor(paste(Edition, sovereignt, medals, sep=" - "), levels = paste(Edition, sovereignt, medals, sep=" - ")))

p <- ggplot(data = world) +
      geom_sf() +
      geom_sf(data = map_winners, aes(fill = medals)) +
      theme(plot.title = element_text(size = 30, face = "bold")) +
      labs(title = "{closest_state} Medals") +
      scale_fill_viridis_c(trans = "sqrt", alpha = .4) + 
      transition_states(map_winners$title_var2, state_length = 15) +
      enter_fade() +
      exit_fade()

# duration/nframes should be the time spent on each frame
animate(p, height = 600, width = 920, duration = 52, nframes = 26)
# save
anim_save("olympics3.gif")

