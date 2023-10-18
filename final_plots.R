#### Create final plots 

library(tidyverse)
library(tidycensus)
library(acs)
library(ggplot2)
library(sf)
library(sp)
library(viridis)
library(stars)
library(data.table)
library(raster)
library(geostatsp)


load('results.RData')

#load('washington_state.RData')

#MGBorder = spTransform(county_geometry, projection(all.states))
#MGBorderouter <- rgeos::gUnaryUnion(MGBorder)


poprastercols <- mapmisc::colourScale(
  uest,
  breaks = quantile(values(uest)[values(uest)!=0],(0:9)/9,na.rm=TRUE),
  #dec=-1,
  style =  'fixed',
  col = "Oranges"
)


mapmisc::map.new(U.rast)
plot(U.rast,
     col = poprastercols$col,
     breaks = poprastercols$breaks,
     legend=FALSE, add=TRUE)
#plot(MGBorder, add=TRUE,border=mapmisc::col2html("black", 0.5), lwd=0.5)
#plot(MGBorderouter,add = TRUE)
mapmisc::legendBreaks('topright',poprastercols, cex=1.5,bty = 'n',inset=.1)



poprastercols <- mapmisc::colourScale(
  uest,
  breaks = quantile(values(uest)[values(uest)!=0],(0:9)/9,na.rm=TRUE),
  #dec=-1,
  style =  'fixed',
  col = "Oranges"
)



poprastercols <- mapmisc::colourScale(
  EL.rast,
 # breaks = round(quantile(values(EL.rast)[values(EL.rast)!=0],(0:10)/10,na.rm=TRUE)),
  breaks=c(0,0.10,.5,1,2,3,4,5,10,12),	  
#dec=-1,
  style =  'fixed',
  col = "Oranges"
)



mapmisc::map.new(EL.rast)
plot(EL.rast,
     col = poprastercols$col,
     breaks=poprastercols$breaks,
	#breaks = c(0,0.10,.5,1,2,3,4,5,10,12),
     legend=FALSE, add=TRUE)
mapmisc::legendBreaks('topright',poprastercols, cex=1.5,bty = 'n',inset=.1)

#plot(MGBorder, add=TRUE,border=mapmisc::col2html("black", 0.5), lwd=0.5)
#plot(MGBorderouter,add = TRUE)
