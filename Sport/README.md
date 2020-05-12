Animation of Olympic Medal Data


The gif here shows the country with the highest tally of Olympic medals in all games from 1896 to 2008.

Data is available from "https://docs.google.com/spreadsheets/d/1zeeZQzFoHE2j_ZrqDkVJK9eF7OH1yvg75c8S-aBcxaU/edit#gid=322436777"

The animation is made using the gganimate package. 

The transition_states() argument is added to the ggplot function to specify the column that is plotted in the animation.

The title argument in labs only takes {closest_state}, as explained in this github issue: https://github.com/thomasp85/gganimate/issues/252. This only plots the column title as the title. One way around it is to concatenate the desired title in a new column, and pass that column to the transition_states() argument. This is done in my code

NOTE ABOUT ANIMATION: The data used was grouped by country and year. It turns out that all winners of team events were listed, and so team events were counted as more than one medal. Unfortunately as a result, the animation is WRONG. 
