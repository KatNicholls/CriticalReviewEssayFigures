# Figure for Spread of Bacteria Globally
library(dplyr)
library(rworldmap)
library(countrycode)
library(RColorBrewer)

setwd('~/II/CRIT REVIEW ESSAY/FIGURE DATA')

#Data FROM CABI
DATA <- read.csv('BACTERIA SPREAD.csv')

#Coordinates collated from multiple data sources - to make sure have all countries/provinces/states that is included in the CABI dataset
Long_Lat <- read.csv('LONGS AND LATS.csv')

#NEED TO TRANSFORM DATA INTO SOMETHING THAT I CAN MAP -> LIKELY BUBBLE MAP
plot <- data.frame(country=DATA$Continent.Country.Region, Distribution=DATA$Distribution, Bacterium=DATA$Notes, year= DATA$First.Reported)
plot <- plot %>% arrange(plot, plot$country)

#Making Size Scale
plot$Distribution <- gsub("Present, Localized","2",plot$Distribution)
plot$Distribution <- gsub("Present, Few occurrences","3",plot$Distribution)
plot$Distribution <- gsub("Present, Widespread","4",plot$Distribution)
plot$Distribution <- gsub("Present","1",plot$Distribution)
plot$Distribution <- as.numeric(plot$Distribution)

#LONG_LAT TO THE SAME NAMES
Long_Lat$Place.Name <- gsub("\\, USA","", Long_Lat$Place.Name)
Long_Lat$Place.Name <- gsub("\\, the USA","", Long_Lat$Place.Name)
Long_Lat$Place.Name <- gsub("\\, the US","", Long_Lat$Place.Name)

#Finding matches !!
#Long_Lat$Place.Name[Long_Lat$Place.Name%in%plot$country]
#plot$country[plot$country%in%Long_Lat$Place.Name]

#Getting Matches and Reordering (both lists now althabetical)
Needed_Coordinates <- Long_Lat %>% filter(Long_Lat$Place.Name%in%plot$country) 
Needed_Coordinates <- Needed_Coordinates %>% arrange(Needed_Coordinates, Place.Name)

#Adding longitude and latitude
plot <- plot %>% mutate(plot, Latitude = Needed_Coordinates$Latitude) %>% mutate(plot, Longitude = Needed_Coordinates$Longitude) 

#Colour
colourPalette <- RColorBrewer::brewer.pal(5,'YlOrRd')

#Now mapping
mapDevice("x11")
par(mar=c(1,1,1,1), xaxs="i",yaxs="i")
mapBubbles(dF = plot, nameX = "Longitude", nameY = "Latitude",
           nameZSize = "Distribution" , nameZColour = "Bacterium", 
           fill = TRUE, pch = 21, symbolSize = 0.75, 
           main = mtext("Figure 2a) Global Distribution of Ca. Liberibacter Species", side=3, line=-2,outer=TRUE), numCats = 5,
           catMethod = "categorical", colourPalette = colourPalette, mapRegion = "world", 
           borderCol = "grey", oceanCol = 'lightblue',landCol = 'wheat', 
           addLegend = TRUE, legendVals= c(1,2,3,4), legendPos = "left", legendTitle = "Distribution Extent",
           lwd = 0.5, lwdSymbols = 1)

#ADD YEAR TO PLOT
text(plot$Longitude, plot$Latitude, plot$year, pos = 3)

#STILL NEED TO EXPLAIN DISTRIBUTION EXTENT... 


