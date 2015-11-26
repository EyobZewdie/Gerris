library(dplyr)
pData <- read.delim("cloud1.dat", header = FALSE, sep = " ");
pData <- pData[,!is.na(pData[1,])]
pData <- tbl_df(pData)
pData <- rename(pData, Pnumber=V6, long=V7, lat=V8, depth=V9)
by_Pnumber <- group_by(pData, Pnumber)
summary <- summarize(by_Pnumber, count = n())
p0Data = filter(pData, Pnumber == 0)

for (i in 1:nrow(p0Data)){
  plotName= paste("plot", toString(i))
  jpeg(plotName)
  with(p0Data[i,], plot (long, lat, xlim = c(min(pData$long, na.rm = T), max(pData$long, na.rm = T)), ylim = c(min(pData$lat, na.rm = T), max(pData$lat, na.rm = T)), xlab ='long', ylab = 'lat'))
  for (j in 1:nrow(na.omit(summary))){
    requiredData = filter(pData, Pnumber == j - 1)
    par(new = T)
    with(requiredData[i,], plot (long, lat, xlim = c(min(pData$long, na.rm = T), max(pData$long, na.rm = T)), ylim = c(min(pData$lat, na.rm = T), max(pData$lat, na.rm = T)), xlab ='long', ylab = 'lat'))
  }
  dev.off()
}

library(ggplot2)
library(rgdal)

nz <- fortify(readOGR('/dragonfly/gis/shapes/custom/bigislands.shp', 'bigislands'))
nz$lat <- -nz$lat


g <- ggplot(nz, aes(x=long,y=lat, group=group)) + 
  geom_polygon(fill='grey') + 
  coord_fixed(ratio=1/cos(40/(180)*pi)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank()) +
  scale_y_reverse() +
  xlab('Longitude (\u00B0E)') +
  ylab('Latitude (\u00B0S)')
ggsave('nz.pdf', width=5, height=7)
ggplot(data=groupedPD, aes(x=long, y=lat, group=0) + geom_line()
