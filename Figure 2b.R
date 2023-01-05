#Figure for spread of VECTOR GLOBALLY
library(dplyr)
library(rworldmap)
library(countrycode)
library(RColorBrewer)

setwd('~/II/CRIT REVIEW ESSAY/FIGURE DATA')

#Data FROM CABI / EPPO
DATA <- read.csv('PSYLLID SPREAD ARROWS.csv')

#Coordinates collated from multiple data sources - to make sure have all countries/provinces/states that is included in Dataset
Long_Lat <- read.csv('LONGS AND LATS.csv')

#NEED TO TRANSFORM DATA INTO SOMETHING THAT I CAN MAP -> LIKELY BUBBLE MAP
plot <- data.frame(country=DATA$Continent.Country.Region, Distribution=DATA$Distribution, PSYLLID=DATA$PSYLLID, year= DATA$First.Reported)
plot$country <- gsub("- ","",plot$country)
plot$country <- gsub("TimorLeste","Timor-Leste",plot$country)
plot$country <- gsub(", Republic of the", " [Republic]", plot$country)
plot$country <- gsub(", Democratic Republic of the", " [DRC]", plot$country)
plot <- plot %>% arrange(plot, plot$country)

#Making Size Scale
plot$Distribution <- gsub("Absent, Unconfirmed presence records","0",plot$Distribution)
plot$Distribution <- gsub("Absent, Formerly present","0",plot$Distribution)
plot$Distribution <- gsub("Absent, Invalid presence records","0",plot$Distribution)
plot$Distribution <- gsub("Absent, Intercepted only","0",plot$Distribution)
plot$Distribution <- gsub("Absent, Confirmed absent by survey","0",plot$Distribution)
plot$Distribution <- gsub("Absent","0",plot$Distribution)
plot$Distribution <- gsub("Present, Few occurrences","2",plot$Distribution)
plot$Distribution <- gsub("Present, Localized","2",plot$Distribution)
plot$Distribution <- gsub("Present, Widespread","3",plot$Distribution)
plot$Distribution <- gsub("Present, Transient under eradication","1",plot$Distribution)
plot$Distribution <- gsub("Present","2",plot$Distribution)
plot$Distribution <- as.numeric(plot$Distribution)

#LONG_LAT TO THE SAME NAMES
Long_Lat$Place.Name <- gsub("\\, USA","", Long_Lat$Place.Name)
Long_Lat$Place.Name <- gsub("\\, the USA","", Long_Lat$Place.Name)
Long_Lat$Place.Name <- gsub("\\, the US","", Long_Lat$Place.Name)

#Finding matches !!
Long_Lat$Place.Name[Long_Lat$Place.Name%in%plot$country]
plot$country[!(plot$country%in%Long_Lat$Place.Name)]

#Getting Matches and Reordering (both lists now althabetical)
Needed_Coordinates <- Long_Lat %>% filter(Long_Lat$Place.Name%in%plot$country) 
Needed_Coordinates <- Needed_Coordinates %>% arrange(Needed_Coordinates, Place.Name)

#Adding longitude and latitude - can't just order and merge like before as repeated places so LOOP
Latitude <- c(1:nrow(plot))
Longitude <- c(1:nrow(plot))
plot <- plot %>% mutate(plot, Latitude = Latitude) %>% mutate(plot, Longitude = Longitude) 
for(i in 1:nrow(plot)){
  for(j in 1:nrow(Needed_Coordinates)){
    if (plot$country[i] == Needed_Coordinates$Place.Name[j]){
      plot$Latitude[i] <- Needed_Coordinates$Latitude[j]
      plot$Longitude[i] <- Needed_Coordinates$Longitude[j]
    }
  }
}

#Colour - maybe change these were random
colourPalette <- c("#1887ab","#704776", "#d70e17")

#Now mapping
#mapDevice("x11")
par(mar=c(1,1,1,1), xaxs="i",yaxs="i")
mapBubbles(dF = plot, nameX = "Longitude", nameY = "Latitude",
           nameZSize = "Distribution" , nameZColour = "PSYLLID", 
           fill = TRUE, pch = 21, symbolSize = 0.75, 
           main = "", numCats = 5,
           catMethod = "categorical", colourPalette = colourPalette, mapRegion = "world", 
           borderCol = "grey", oceanCol = 'lightblue',landCol = 'wheat', 
           addLegend = TRUE, legendVals= c(1,2,3), legendPos = "topleft", legendTitle = "Distribution Extent", plotZeroVals = FALSE,
           lwd = 0.5, lwdSymbols = 1)

#ADD YEAR TO PLOT
text(plot$Longitude, plot$Latitude, plot$year, pos = 3, cex=0.7)

#ADD REFERENCES AT BOTTOM
mtext("[CABI/EPPO]",side=1,line=-1)

#LABEL DISTRIBUTION EXTENT
text(-138,80.5, "- Present, Transient under eradication")
text(-157,77, "- Present")
text(-148,73.5, "- Present, Widespread")

#Arrows for some points
Arrows <- read.csv("Arrows.csv")
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


