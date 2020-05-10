library(sf)
library(tidyr)
library(dplyr)
library(readr)
library(jsonlite)

data = read_csv("Summer_Olympic_medallists_1896_2008.csv", skip = 4)
# read in world map as sf
world = st_read("world.geo.json")
#plot to check 
plot(st_geometry(world))

#subset only necessary columns
world <- world[, c("sovereignt", "iso_a3")]
plot(st_geometry(world))

# GET STATS 

# function to calculate most frequent categorical value  https://exploratory.io/note/kanaugust/1701090969905358
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}


year_best = data %>% group_by(Edition) %>%
          summarise(best_country = calculate_mode(NOC),  #use function above to get most repeated country
                   medals = length(which(NOC==calculate_mode(NOC)))) # get count of medals for best country

year_best$best_country[year_best$best_country=='GRE'] <- 'GRC'
year_best$best_country[year_best$best_country=='URS'] <- 'RUS'

# merge map with year_best
x <- merge(world, year_best)
# filter to get only the rows where the spatial data matches the winning country
mteq <- x[x$iso_a3==x$best_country, ]

filter(x, sov_a3 == year_best)
x2 <- left_join(world, year_best, by = c("sov_a3" = "best_country"))
x2 <- left_join(year_best, world, by = c("best_country" = "sov_a3"))
