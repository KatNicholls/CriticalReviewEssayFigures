#FIGURE FOR GLOBAL CITRUS PRODUCTION
library(dplyr)
setwd('~/II/CRIT REVIEW ESSAY/FIGURE DATA')

#Data retrieved from Faostat on internet
DATA <- read.csv('GLOBAL PRODUCTION FAOSTAT.csv')

#Replacing missing values with 0
for (i in 1:nrow(DATA)){
  if(is.na(DATA[i,12])){
    DATA[i,12] <- 0
  }
}
#Collating the different types of Citrus production values
Production<- aggregate(DATA$Value, by=list(Country=DATA$Area), FUN=sum)

#Change names to make sure all get mapped & Get rid of China, mainland and China, Taiwan
Production$Country <- gsub("Côte d'Ivoire", "Ivory Coast",Production$Country)
Production$Country <- gsub("Czechia", "Czech Republic",Production$Country)
Production$Country <- gsub("Eswatini", "Swaziland",Production$Country)
Production$Country <- gsub("Türkiye", "Turkey",Production$Country)
Production2 <- Production[c(1:28,31:159),]

#NOW PRODUCING THE MAP
library(rworldmap)
library(countrycode)
library(RColorBrewer)

MAP <- joinCountryData2Map(Production2, joinCode = "NAME", nameJoinColumn = "Country", verbose=TRUE)

#create own scale for data and remove scientific notation
options(scipen=999)
scale <- c(0,1)
for (i in 3:10){
  scale[i] <- scale[i-1]*10
}

#scale <- format(scale, scientific = FALSE, big.mark = ',', as.numeric=TRUE)
#scale <- as.numeric(scale)

#Colour Scheme
colourPalette <- RColorBrewer::brewer.pal(9,'YlOrRd')

#now mapping

mapDevice("x11")
par(mar=c(1,1,1,1),xaxs="i",yaxs="i")
mapParams <- mapCountryData(MAP, nameColumnToPlot = "x", catMethod=scale, mapTitle= "", colourPalette=colourPalette, oceanCol="lightblue", missingCountryCol="white", addLegend = FALSE)
do.call(addMapLegend, c(mapParams, legendLabels="all", labelFontSize=0.9, legendShrink=0.7, legendMar = 55,
                        horizontal=FALSE, legendArgs=mtext("Production in tonnes", side=2, line=0, outer=FALSE ), tcl = -0.5, digits=2, legendIntervals="page"))

mtext("[FAOSTAT]",side=1,line=-1)

#Adding latitude lines
abline(h=c(-40,40),lty=2, lwd=2,col='black')
text(160,42.5,"40°N")
text(160,-37.5,"40°S")

