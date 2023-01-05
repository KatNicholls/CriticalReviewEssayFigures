# Figure for Spread of Bacteria Globally
library(dplyr)
library(rworldmap)
library(countrycode)
library(RColorBrewer)

setwd('~/II/CRIT REVIEW ESSAY/FIGURE DATA')

#Data FROM CABI/EPPO
DATA <- read.csv('BACTERIA SPREAD ARROWS.csv')

#Coordinates collated from multiple data sources - to make sure have all countries/provinces/states that is included in dataset
Long_Lat <- read.csv('LONGS AND LATS.csv')

#NEED TO TRANSFORM DATA INTO SOMETHING THAT I CAN MAP -> LIKELY BUBBLE MAP
plot <- data.frame(country=DATA$Continent.Country.Region, Distribution=DATA$Distribution, Bacterium=DATA$Notes, year= DATA$First.Reported)
plot <- plot %>% arrange(plot, plot$country)

#Making Size Scale
plot$Distribution <- gsub("Present, Localized","2",plot$Distribution)
plot$Distribution <- gsub("Present, Few occurrences","2",plot$Distribution)
plot$Distribution <- gsub("Present, Widespread","3",plot$Distribution)
plot$Distribution <- gsub("Present","1",plot$Distribution)
plot$Distribution <- as.numeric(plot$Distribution)

#LONG_LAT TO THE SAME NAMES
Long_Lat$Place.Name <- gsub("\\, USA","", Long_Lat$Place.Name)
Long_Lat$Place.Name <- gsub("\\, the USA","", Long_Lat$Place.Name)
Long_Lat$Place.Name <- gsub("\\, the US","", Long_Lat$Place.Name)

#Finding matches !!
Long_Lat$Place.Name[!(Long_Lat$Place.Name%in%plot$country)]
plot$country[!(plot$country%in%Long_Lat$Place.Name)]

#Getting Matches and Reordering (both lists now althabetical)
Needed_Coordinates <- Long_Lat %>% filter(Long_Lat$Place.Name%in%plot$country) 
Needed_Coordinates <- Needed_Coordinates %>% arrange(Needed_Coordinates, Place.Name)

#Adding longitude and latitude
plot <- plot %>% mutate(plot, Latitude = Needed_Coordinates$Latitude) %>% mutate(plot, Longitude = Needed_Coordinates$Longitude) 

#Colour
colourPalette <- RColorBrewer::brewer.pal(4,'PuRd')
#colourPalette <- c("#FE2712","#FC600A", "#FB9902","#FCCC1A","#FEFE33")

#Now mapping
#mapDevice("x11")
par(mar=c(1,1,1,1), xaxs="i",yaxs="i")
mapBubbles(dF = plot, nameX = "Longitude", nameY = "Latitude",
           nameZSize = "Distribution" , nameZColour = "Bacterium", 
           fill = TRUE, pch = 21, symbolSize = 0.75, 
           main = "", numCats = 5,
           catMethod = "categorical", colourPalette = colourPalette, mapRegion = "world", 
           borderCol = "grey", oceanCol = "lightblue",landCol = 'wheat', 
           addLegend = TRUE, legendVals= c(1,2,3), legendPos = "topleft", legendTitle = "Distribution Extent",
           lwd = 0.5, lwdSymbols = 1)

#ADD YEAR TO PLOT
text(plot$Longitude, plot$Latitude, plot$year, pos = 3, cex = 0.7)

#EXPLAIN DISTRIBUTION EXTENT
text(-157,80.5, "- Present")
text(-137.5,77, "- Present, Localised/Few Occurrences")
text(-148,73.5, "- Present, Widespread")

#ADD REFERENCES AT BOTTOM
mtext("[CABI/EPPO]",side=1,line=-1)

#Arrows for some points
Arrows <- read.csv("B Arrows.csv")
for (i in 1:nrow(Arrows)){
  y1 = Arrows$Latitude[i]
  x1 = Arrows$Longitude[i]
  if(Arrows$Longitude[i]<0){
    arrows((x1+10),(y1),x1,y1,length=0, angle=40)
    text((x1+12),(y1),Arrows$First.Reported[i], cex=0.7)
  }
  else if(Arrows$Latitude[i]<26){
    arrows((x1),(y1-7),x1,y1,length=0, angle=40)
    text((x1),(y1-8),Arrows$First.Reported[i], cex=0.7)
  }
  else{
    arrows((x1),(y1+8),x1,y1,length=0, angle=40)
    text((x1),(y1+9),Arrows$First.Reported[i], cex=0.7)
  }
}
