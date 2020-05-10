library(tidyr)
# pivot for plotting (to get count sheep and count wolves together)

forbox <- merged %>% pivot_longer(cols = c(`count sheep`, `count wolves`), names_to = "variable", values_to = "count")
# convert group column to string for plotting
forbox$group <- as.character(forbox$group)
# add a new column with concatenated sheep and wolf initial populations (for x axis)
forbox = forbox%>% 
  unite(label, `initial-number-wolves`, `initial-number-sheep`, sep = "/", remove = FALSE)
  
  
p1 <- forbox %>% ggplot(aes(y=count, x=group, group=group)) + 
  geom_boxplot(aes(fill = group)) +
  facet_grid(variable ~ ., scales='free')
p1


# plots of sheep and wolves mean and std at fixed grass regrowth time
grass_regrowth  <- 40

p2 <- forbox %>% filter(`grass-regrowth-time` == grass_regrowth) %>% 
                ggplot(aes(y=count, x=label, group=group)) + 
                geom_boxplot() +
                ggtitle("Effect of Different Initial Number of Wolves and Sheep (Grass Regrowth = 40)") +
                labs(y="Number Remaining after 1000 Steps", x = "Initial Number (Wolves/Sheep)") +
                theme(axis.text.x = element_text(angle=50, hjust=1)) +
                facet_grid(variable ~ ., scales='free')
p2
ggsave(path = "Plots", file="Box_Regrowth_40.png", p2, width = 10, height = 6)


# plot by grouping by ratio of wolves to sheep (fixed grass regrowth time)
forbox$initial_ratio <- as.character(forbox$initial_ratio)

p3 <- forbox %>% filter(`grass-regrowth-time` == grass_regrowth) %>% 
              ggplot(aes(y=count, x=initial_ratio, group=initial_ratio)) + 
              geom_boxplot() +
              ggtitle("Effect of Different Initial Number of Wolves and Sheep (Grass Regrowth = 40)") +
              labs(y="Number Remaining after 1000 Steps", x = "Ratio of Wolves to Sheep at Start") +
              facet_grid(variable ~ ., scales='free')
p3

ggsave(path = "Plots", file="Box_Regrowth_40_Ratio.png", p3, width = 10, height = 6)


# HEATMAP

# add a new column with concatenated sheep and wolf initial populations (for x axis)
forHeatMap = forHeatMap %>% 
  unite(`Initial Population (Wolves/Sheep)`, `initial_wolves`, `initial_sheep`, sep = "/", remove = FALSE)

# plots that show variation with steps
# pivot for plotting (to get count sheep and count wolves together)

forHeatMapFacet <- forHeatMap %>% pivot_longer(cols = c(`sheep`, `wolves`), names_to = "variable", values_to = "count")
forHeatMapFacet$ratio <- round(forHeatMapFacet$initial_wolves / forHeatMapFacet$initial_sheep, 2) %>% as.character()
# convert group column to string for plotting


# check grass regrowth time!

p4 <- forHeatMap %>% filter(grass_regrowth_time == grass_regrowth) %>% 
         ggplot(aes(y=`wolves`, x=`[step]`, group=group, colour = `Initial Population (Wolves/Sheep)`)) +
         geom_line() 

# Facet Plot
p5 <- forHeatMapFacet %>% filter(`grass_regrowth_time` == grass_regrowth) %>% 
            ggplot(aes(y=count, x=`[step]`, group=`group`, colour =`Initial Population (Wolves/Sheep)`)) + 
            geom_line() +
            ggtitle("Effect of Different Initial Number of Wolves and Sheep (Grass Regrowth = 20)") +
            labs(y="Number of Wolves/Sheep", x = "Step") +
            facet_grid(variable ~ ., scales='free')
p5

ggsave(path = "Plots", file="Facet_Regrowth_20.png", p5, width = 10, height = 6)












p6 <- forHeatMap %>% filter(grass_regrowth_time == grass_regrowth) %>% 
  ggplot(aes(y=`wolves`, x=`[step]`, group=group, colour = `Initial Population (Wolves/Sheep)`)) +
  geom_tile() 

p6
