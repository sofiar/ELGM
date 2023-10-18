library(aghq)
library(TMB)
library(disaggregation)
library(raster)
library(stars)
library(sf)
library(ggplot2)



#Race='Black'
#Race='White'
Race='Hispanic'
#Race='Other'

year=2012

name=paste('lastRun_',Race,year,'.RData',sep='')
load(name)


pdf(paste0('./Results_',Race,year,".pdf"),width = 12,height = 7)


ggplot(taupdf)+geom_line(mapping = aes(x = transparam,y = pdf_transparam))+theme_bw()+
  labs(x = expression(1/sqrt(tau)),y = '') +
  theme(text = element_text(size = 18))

ggplot(data = dplyr::filter(sigmapdf,transparam < 3))+geom_line(mapping = aes(x = transparam,y = pdf_transparam))+theme_bw()+
  labs(x = expression(sigma),y = '') +
  theme(text = element_text(size = 18))

ggplot(data = rhopdf)+geom_line(mapping = aes(x = transparam,y = pdf_transparam))+theme_bw()+
  labs(x = expression(rho),y = '') +
  theme(text = element_text(size = 18))

# intercept
hist(betasamps)

# PloT U fild 
#MGBorder = spTransform(county_geometry, projection(dis_data$polygon_shapefile))
#MGBorderouter <- rgeos::gUnaryUnion(MGBorder)
#U.rast <- raster::mask(uest,MGBorder)
plot(U.rast,main='E(u|Y)')
plot(MGBorder, add=TRUE,border=mapmisc::col2html("black", 0.5), lwd=0.5)
plot(MGBorderouter,add = TRUE)

# Plot E(lamda|Y) 
#EL.rast <- raster::mask(predmean,MGBorder)
plot(EL.rast,main=expression(paste('E(',lambda,'|Y)')))
plot(MGBorder, add=TRUE,border=mapmisc::col2html("black", 0.5), lwd=0.5)
plot(MGBorderouter,add = TRUE)

# Plot P(lamda>.2|Y) 
#PL.rast <- raster::mask(predexceedence,MGBorder)
#plot(PL.rast)
#plot(MGBorder, add=TRUE,border=mapmisc::col2html("black", 0.5), lwd=0.5)
#plot(MGBorderouter,add =TRUE)

## Create maps of counts 
#spplot(all_states,zcol='nCounts')

valscols <- mapmisc::colourScale(
  all_states$nCounts,
  breaks = c(-1,0,10,50,100,500,1000,1500,2000,max(all_states$nCounts,na.rm=TRUE)), # whites /hispanic
  #breaks = c(-1,0,5,10,30,100,500,max(all_states$nCounts,na.rm=TRUE)), # black
   
  dec=0,
  style =  'fixed',
  col = "Oranges"
)


mapmisc::map.new(all_states)
plot(all_states,col = valscols$plot, add=TRUE, border=mapmisc::col2html("black", 0.5),
 lwd=0.5,main='counts',breaks=valscols$breaks)
plot(MGBorderouter,add = TRUE)
mapmisc::legendBreaks('left',valscols, cex=0.8,bty = 'n',inset=.92)
# dev.off()


#### plot offsets
plotraster <- mask(population_raster,all_states)

poprastercols <- mapmisc::colourScale(
  population_raster,
  #breaks = c(-1,unique(trunc(quantile(values(plotraster),(0:30)/30,na.rm=TRUE),digits=500))),# white,
  #breaks = c(-1,unique(trunc(quantile(values(plotraster),(0:400)/400,na.rm=TRUE),digits=500))),# black,
  breaks = c(-1,unique(trunc(quantile(values(plotraster),(0:500)/500,na.rm=TRUE),digits=500))),# hispanic
  dec=1,
  style =  'fixed',
  col = "Oranges"
)



mapmisc::map.new(plotraster)
#plot(plotraster)
plot(plotraster,col = poprastercols$col,
     breaks = poprastercols$breaks, add=TRUE, legend = FALSE, lwd=0.5)
plot(MGBorder, add=TRUE, lwd=0.5)
plot(MGBorderouter,add = TRUE)
mapmisc::legendBreaks("left",poprastercols, cex=0.8,bty = 'n',inset=+.915)
dev.off()





