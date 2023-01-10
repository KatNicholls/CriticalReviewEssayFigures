#FIGURE FOR GLOBAL CITRUS PRODUCTION
library(dplyr)
setwd('~/II/CRIT REVIEW ESSAY/FIGURE DATA')

#Data retrieved from Faostat on internet
DATA <- read.csv('FAOSTAT_data_en_1-3-2023.csv')

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
scale <- c(0,1000,10000,100000,1000000,10000000,100000000)
#for (i in 3:10){
#  scale[i] <- scale[i-1]*10
#}

#Colour Scheme
colourPalette <- RColorBrewer::brewer.pal(6,'YlOrRd')

#now mapping
#mapDevice("x11")
par(mai=c(0,0,0,0),xaxs="i",yaxs="i")
mapParams <- mapCountryData(MAP, nameColumnToPlot = "x", catMethod=scale, mapTitle= "", colourPalette=colourPalette, oceanCol="lightblue", missingCountryCol="white", addLegend = FALSE)
do.call(addMapLegend, c(mapParams, legendLabels="all", labelFontSize=0.9, legendShrink=0.65, legendMar = 3,
                        horizontal=TRUE, legendArgs=mtext("Production in tonnes", side=1, line=-1), tcl = -0.5, digits=2, legendIntervals="page"))

mtext("[FAOSTAT]",side=3,line=-6, adj=0, outer=TRUE)

#Adding latitude lines
abline(h=c(-40,40),lty=2, lwd=2,col='black')
text(160,42.5,"40°N")
text(160,-37.5,"40°S")

#Add North Arrow
library(prettymapr)
addnortharrow(
  pos = "topright",
  padin = c(0.1, 1),
  scale = 0.8,
  lwd = 1,
  border = "black",
  cols = c("white", "black"),
  text.col = "black"
)

