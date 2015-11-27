#Load Packages
library(dplyr)
library(ggplot2)
library(rgdal)

#Data Manipulation
pData <- read.delim("T172800.dat", header = FALSE, sep = " ");
pData <- pData[,!is.na(pData[1,])]
pData <- tbl_df(pData)
pData <- rename(pData, Pnumber=V6, long=V7, lat= V8, depth=V9)
pData$lat = -pData$lat
p0Data = filter(pData, Pnumber == 0)

by_Pnumber <- group_by(pData, Pnumber)
summary <- summarize(by_Pnumber, count = n())
number = nrow(na.omit(summary)) #number of particles
                     
#plotting
nz <- fortify(readOGR('/dragonfly/gis/shapes/custom/bigislands.shp', 'bigislands'))
nz$lat <- -nz$lat

for (i in 1:nrow(p0Data)){ #nrow(p0Data) is the number of time steps
  plotName = paste("plot", toString(i), ".jpg", sep="")
  timeStep <- slice(pData, ((i - 1)*number):(number*i - 1))
  g <- ggplot(data = timeStep, aes(x=long, y=lat)) + 
  geom_point(alpha = 0.5) +
  geom_polygon(data=nz, fill='grey', aes(group=group)) +
  xlab('Longitude (\u00B0E)') +
  ylab('Latitude (\u00B0S)') + 
  xlim( min(na.omit(pData$long)), max(na.omit(pData$long))) +
  scale_y_reverse(lim=c(max(na.omit(pData$lat)),min(na.omit(pData$lat)))) +
  coord_fixed(ratio=1/cos(40/(180)*pi)) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ggsave(filename = plotName, plot = g) 
}
