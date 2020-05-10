library(sp)
library(MASS)
library(reshape2)
library(geojsonio)
library(rgdal)
library(downloader)
library(maptools)
library(dplyr)
library(broom) 
library(stplanr)
library(ggplot2)
library(leaflet)

#Fetch a GeoJson of some district-level boundaries from the ONS Geoportal. First add the URL to an object
EW <- geojson_read("http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson",
                   what = "sp")

# pull out london using grep and the regex wildcard for'start of the string' (^) to to look for the bit of the 
# district code that relates to London (E09) from the 'lad15cd' column in the data slot of our sp
London <- EW[grep("^E09",EW@data$lad15cd),]
#plot it
plot(London)

#CALCULATING A DISTANCE MATRIX

#boundaries we have are not in British National Grid - the bit that says proj4string tells me 
#that we are in WGS84 or using latitude and longitude coordinates. We need to change this to 
#British National Grid so our distances are in metres and not decimal degrees, then do everything 
#we need to do to generate a distance matrix.

#first transfrom to BNG - this will be important for calculating distances using spTransform
BNG = "+init=epsg:27700"
LondonBNG <- spTransform(London, BNG)
#now, order by borough code - *this step will be imporant later on*
LondonBNG <- LondonBNG[order(LondonBNG$lad15cd),]
#now use spDists to generate a big distance matrix of all distances between boroughs in London
dist <- spDists(LondonBNG)
#melt this matrix into a list of origin/destination pairs using melt. Melt in in the reshape2 package. Reshape2, dplyr and ggplot, together, are some of the best packages in R, so if you are not familiar with them, get googling and your life will be much better!
distPair <- melt(dist)

# FLOW DATA

#read in your London Commuting Data
cdata <- read.csv("https://www.dropbox.com/s/7c1fi1txbvhdqby/LondonCommuting2001.csv?raw=1")
#read in a lookup table for translating between old borough codes and new borough codes
CodeLookup <- read.csv("https://www.dropbox.com/s/h8mpvnepdkwa1ac/CodeLookup.csv?raw=1")
#read in some population and income data
popincome <- read.csv("https://www.dropbox.com/s/84z22a4wo3x2p86/popincome.csv?raw=1")

#now merge these supplimentary data into your flow data dataframe
cdata$OrigCodeNew <- CodeLookup$NewCode[match(cdata$OrigCode, CodeLookup$OldCode)]
cdata$DestCodeNew <- CodeLookup$NewCode[match(cdata$DestCode, CodeLookup$OldCode)]
cdata$vi1_origpop <- popincome$pop[match(cdata$OrigCodeNew, popincome$code)]
cdata$vi2_origsal <- popincome$med_income[match(cdata$OrigCodeNew, popincome$code)]
cdata$wj1_destpop <- popincome$pop[match(cdata$DestCodeNew, popincome$code)]
cdata$wj2_destsal <- popincome$med_income[match(cdata$DestCodeNew, popincome$code)]

#Data needs to be ordered by borough code, if it's not, we will run into problems when 
#we try to merge our distance data back in later, so to make sure, we can arrange by orign 
#and then destination using dplyr's 'arrange' function

cdata <- arrange(cdata, OrigCodeNew, DestCodeNew)

# Add Distance Data and Create New Column(s) for Total Flows

# First create a new total column which excludes intra-borough flow totals (well sets them to 
# a very very small number for reasons you will see later...)
cdata$TotalNoIntra <- ifelse(cdata$OrigCode == cdata$DestCode,0,cdata$Total)
cdata$offset <- ifelse(cdata$OrigCode == cdata$DestCode,0.0000000001,1)
# now add the distance column into the dataframe
cdata$dist <- distPair$value

#Chop out the intra-borough flows
cdata <- cdata[cdata$OrigCode!=cdata$DestCode,]
#now re-order so that OrigCodeNew, DestCodeNew and TotalNoIntra are the first three columns 
# *note that you have to be explicit about the select function in the dplyr package as MASS also has select
cdata <- dplyr::select(cdata, OrigCodeNew, DestCodeNew, Total, everything())


# re-order so that 'lad15cd' is the first column in LondonBNG
# HUSSEIN 
library(sf)
LondonBNG_sf <- st_as_sf(LondonBNG)
# re-order so that 'lad15cd' is the first column in LondonBNG - OTHERWISE od2line WON'T WORK
LondonBNG_sf <- dplyr::select(LondonBNG_sf, lad15cd, everything())
# convert back to sp
LondonBNG <- as(LondonBNG_sf, 'Spatial')
# End HUSSEIN


# PLOT
#use the od2line function from Robin Lovelace's excellent stplanr package
travel_network <- od2line(flow = cdata, zones = LondonBNG)
#and set the line widths to some sensible value according to the flow
w <- cdata$Total / max(cdata$Total) * 6
#now plot it...
plot(travel_network, lwd = w)
plot(LondonBNG, add=T)

# leaflet map

#transform to wgs84
travel_networkwgs <- spTransform(travel_network, "+init=epsg:4326")
#plot in leaflet
leaflet() %>% addTiles() %>% addPolylines(data = travel_networkwgs)


#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
cdatamat <- dcast(cdata, Orig ~ Dest, sum, value.var = "Total", margins=c("Orig", "Dest"))


#run a production constrained SIM (the "-1" indicates no intercept in the regression model).
prodSim <- glm(Total ~ OrigCodeNew+log(wj2_destsal)+log(dist)-1, na.action = na.exclude,
               family = poisson(link = "log"), data = cdata)
#let's have a look at it's summary...
summary(prodSim)

# Model Estimates

# What were the effects of the constraints?

#create some Oi and Dj columns in the dataframe and store row and column totals in them:
#to create O_i, take cdatasub ...then... group by origcodenew ...then... summarise by calculating the sum of Total
O_i <- cdata %>% group_by(OrigCodeNew) %>% summarise(O_i = sum(Total))
cdata$O_i <- O_i$O_i[match(cdata$OrigCodeNew,O_i$OrigCodeNew)]
D_j <- cdata %>% group_by(DestCodeNew) %>% summarise(D_j = sum(Total))
cdata$D_j <- D_j$D_j[match(cdata$DestCodeNew,D_j$DestCodeNew)]

# pull out the parameter values for μi and store them back in the dataframe along with Oi and Dj
coefs <- as.data.frame(prodSim$coefficients)
#then once you have done this, you can join them back into the dataframe using a regular expression to match the bits of the identifier that you need - *note, this bit of code below took me about 2 hours to figure out!*
cdata$mu_i <- coefs$`prodSim$coefficients`[match(cdata$OrigCodeNew,sub(".*OrigCodeNew","", rownames(coefs)))]
#now, where we have missing values for our reference mu_i variable, fill those with 1s
head(cdata)

#save our parameter values into some variables…

mu_i <- prodSim$coefficients[1:33]
alpha <- prodSim$coefficients[34]
beta <- prodSim$coefficients[35]


#we’re ready to generate our estimates:
cdata$prodsimest1 <- exp((cdata$mu_i)+(alpha*log(cdata$wj2_destsal))+(beta*log(cdata$dist)))

# Flow Matrix

#first round the estimates
cdata$prodsimFitted <- round(fitted(prodSim),0)
#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
cdatamat3 <- dcast(cdata, Orig ~ Dest, sum, value.var = "prodsimFitted", margins=c("Orig", "Dest"))
cdatamat3

# TESTING THE “GOODNESS-OF-FIT”.

# METHOD 1: R-Squared
CalcRSquared <- function(observed,estimated){
  r <- cor(observed,estimated)
  R2 <- r^2
  R2
}

CalcRSquared(cdata$Total,cdata$prodsimFitted)

# our model accounts for about 51% of the variation of flows in the system. Not bad, but not brilliant either.

# METHOD 2: RMSE

CalcRMSE <- function(observed,estimated){
  res <- (observed - estimated)^2
  RMSE <- round(sqrt(mean(res)),3)
  RMSE
}

CalcRMSE(cdata$Total,cdata$prodsimFitted)


# CHANGE SALARIES

cdata$wj3_destsalScenario <- cdata$wj2_destsal

# decrease salary in City of London by 30% (38300 - 0.3*38300) = 26810
cdata$wj3_destsalScenario <- ifelse(cdata$wj3_destsalScenario == 38300,26810,cdata$wj3_destsalScenario)

cdata$prodsimest2 <- exp((cdata$mu_i)+(alpha*log(cdata$wj3_destsalScenario))+(beta*log(cdata$dist)))

cdata$prodsimest2 <- round(cdata$prodsimest2,0)
#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
cdatamat4 <- dcast(cdata, Orig ~ Dest, sum, value.var = "prodsimest2", margins=c("Orig", "Dest"))
cdatamat4

#calculate some new wj^alpha and dij^beta values
wj2_alpha <- cdata$wj2_destsal^alpha
dist_beta <- cdata$dist^beta
#calculate the first stage of the Ai values
cdata$Ai1 <- wj2_alpha*dist_beta
#now do the sum over all js bit
A_i <- cdata %>% group_by(OrigCodeNew) %>% summarise(A_i = sum(Ai1))
#now divide in to 1
A_i[,2] <- 1/A_i[,2]
#and write the A_i values back into the data frame
cdata$A_i <- A_i$A_i[match(cdata$OrigCodeNew,A_i$OrigCodeNew)]

#To check everything works, recreate the original estimates
cdata$prodsimest3 <- cdata$A_i*cdata$O_i*wj2_alpha*dist_beta


# Let’s try with our new values for the destination salary in City of London

#calculate some new wj^alpha and dij^beta values
wj3_alpha <- cdata$wj3_destsalScenario^alpha
#calculate the first stage of the Ai values
cdata$Ai1 <- wj3_alpha*dist_beta
#now do the sum over all js bit
A_i <- cdata %>% group_by(OrigCodeNew) %>% summarise(A_i = sum(Ai1))
#now divide in to 1
A_i[,2] <- 1/A_i[,2]
#and write the A_i values back into the data frame
cdata$A_i <- A_i$A_i[match(cdata$OrigCodeNew,A_i$OrigCodeNew)]


# generate some new scenario flow estimates…

#To check everything works, recreate the original estimates
cdata$prodsimest4_scenario <- cdata$A_i*cdata$O_i*wj3_alpha*dist_beta

cdata$prodsimest4_scenario <- round(cdata$prodsimest4_scenario,0)
#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
cdatamat5 <- dcast(cdata, Orig ~ Dest, sum, value.var = "prodsimest4_scenario", margins=c("Orig", "Dest"))
cdatamat5


# INCREASE DISTANCE IMPEDENCE 

# Calibrated Beta is -1.82

# try doubling Beta 

beta2 <- -3.64
# get new distance numbers
dist_beta2 <- cdata$dist^beta2

# use wj2_alpha as we did not change the alpha parameter

#calculate the first stage of the Ai values
cdata$Ai2 <- wj2_alpha*dist_beta2
#now do the sum over all js bit
A_i2 <- cdata %>% group_by(OrigCodeNew) %>% summarise(A_i2 = sum(Ai2))
#now divide in to 1
A_i2[,2] <- 1/A_i2[,2]
#and write the A_i values back into the data frame
cdata$A_i2 <- A_i2$A_i2[match(cdata$OrigCodeNew,A_i2$OrigCodeNew)]

#estimate flow with beta = -3
#To check everything works, recreate the original estimates
cdata$prodsimest5_scenario <- cdata$A_i2*cdata$O_i*wj2_alpha*dist_beta2

cdata$prodsimest5_scenario <- round(cdata$prodsimest5_scenario,0)
#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
cdatamat6 <- dcast(cdata, Orig ~ Dest, sum, value.var = "prodsimest5_scenario", margins=c("Orig", "Dest"))
cdatamat6

# # try tripling Beta 

beta3 <- -5.46
# get new distance numbers
dist_beta3 <- cdata$dist^beta3

# use wj2_alpha as we did not change the alpha parameter

#calculate the first stage of the Ai values
cdata$Ai3 <- wj2_alpha*dist_beta3
#now do the sum over all js bit
A_i3 <- cdata %>% group_by(OrigCodeNew) %>% summarise(A_i3 = sum(Ai3))
#now divide in to 1
A_i3[,2] <- 1/A_i3[,2]
#and write the A_i values back into the data frame
cdata$A_i3 <- A_i3$A_i3[match(cdata$OrigCodeNew,A_i3$OrigCodeNew)]

#estimate flow with beta = -3
#To check everything works, recreate the original estimates
cdata$prodsimest6_scenario <- cdata$A_i3*cdata$O_i*wj2_alpha*dist_beta3

cdata$prodsimest6_scenario <- round(cdata$prodsimest6_scenario,0)
#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
cdatamat7 <- dcast(cdata, Orig ~ Dest, sum, value.var = "prodsimest6_scenario", margins=c("Orig", "Dest"))
cdatamat7

