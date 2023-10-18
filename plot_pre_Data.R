##### Plot mortality data and population raster 

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

year=2012

### key to download data
census_api_key("f587b3ec8f8141986f57549151c63064f1ae1bb7",install=TRUE,overwrite = TRUE)
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

Names.states=c('WA','OR', 'ID','MT','WY','NV','CA','UT','CO','AZ','NM','ND',
  'SD','NE','KS','OK','TX','MN','IA','MO','AR','LA','WI','IL','MS','KY','TN','AL',
  'FL', 'IN','GA','OH','SC','NC','VA','WV','PA','NY','MD','MI','DE','NJ','CT',
  'RI','MA','VT','NH','ME')

Names.states=c('CA')

variables=c(population = "B01001_001")
# get data by tract
states_pt <- get_acs(geography = "tract", year=year,survey='acs5',
                     geometry = TRUE, state=Names.states,
                     variables=variables)
states_pc <- get_acs(geography = "county", year=year,survey='acs5',
                    state = Names.states,geometry = TRUE,
                    variables=variables)

df=data.frame(states_pt)%>% dplyr::select(GEOID,variable,estimate) 
df$GEOID=as.factor(df$GEOID)

pt1=states_pt$geometry %>% st_cast("POLYGON",group_or_split=FALSE)
tract_geometry=as(pt1, 'Spatial')
all.population=SpatialPolygonsDataFrame(Sr=tract_geometry, 
                                        data=data.frame('estimate'=states_pt$estimate),FALSE)


meters.crs= crs("+init=epsg:2163")
rated_population_trans = spTransform(all.population, meters.crs)
population_raster = geostatsp::spdfToBrick(all.population, geostatsp::squareRaster(all.population, 100)
                                           ,pattern = 'estimate',  logSumExpected = FALSE)


### Load mortality data
filecsv=paste('tidy_data/tidy_mortalityData_',as.character(year),'.csv',sep='')
dyear=data.frame(fread(filecsv, header = T))

# filter by race
# dyear=dyear %>% filter(race%in%race.mortality)

U.states=dyear %>% dplyr::filter(stateoc%in%c(Names.states)) 
U.states$countyoc=str_pad(U.states$countyoc, 3, pad = "0")

U.states=U.states %>% 
  group_by(stateoc) %>%
  mutate(CountyF = case_when(
    all( stateoc=="TX") ~ paste('48',countyoc,sep=''),
    all( stateoc=="OK") ~ paste('40',countyoc,sep=''),
    all( stateoc=="NM") ~ paste('35',countyoc,sep=''),
    all( stateoc=="AZ") ~ paste('04',countyoc,sep=''),
    all( stateoc=="UT") ~ paste('49',countyoc,sep=''),
    all( stateoc=="CO") ~ paste('08',countyoc,sep=''),
    all( stateoc=="KS") ~ paste('20',countyoc,sep=''),
    all( stateoc=="NV") ~ paste('32',countyoc,sep=''),
    all( stateoc=="LA") ~ paste('22',countyoc,sep=''),
    all( stateoc=="AR") ~ paste('05',countyoc,sep=''),
    all( stateoc=="MO") ~ paste('29',countyoc,sep=''),
    all( stateoc=="IA") ~ paste('19',countyoc,sep=''),
    all( stateoc=="IL") ~ paste('17',countyoc,sep=''),
    all( stateoc=="CA") ~ paste('06',countyoc,sep=''),
    all( stateoc=="NE") ~ paste('31',countyoc,sep=''),
    all( stateoc=="MS") ~ paste('28',countyoc,sep=''),
    all( stateoc=="TN") ~ paste('47',countyoc,sep=''),
    all( stateoc=="AL") ~ paste('01',countyoc,sep=''),
    all( stateoc=="IN") ~ paste('18',countyoc,sep=''),
    all( stateoc=="KY") ~ paste('21',countyoc,sep=''),
    all( stateoc=="OH") ~ paste('39',countyoc,sep=''),
    all( stateoc=="GA") ~ paste('13',countyoc,sep=''),
    all( stateoc=="SC") ~ paste('45',countyoc,sep=''),
    all( stateoc=="FL") ~ paste('12',countyoc,sep=''),
    all( stateoc=="NC") ~ paste('37',countyoc,sep=''),
    all( stateoc=="WV") ~ paste('54',countyoc,sep=''),
    all( stateoc=="OR") ~ paste('41',countyoc,sep=''),
    all( stateoc=="ID") ~ paste('16',countyoc,sep=''),
    all( stateoc=="WA") ~ paste('53',countyoc,sep=''),
    all( stateoc=="WY") ~ paste('56',countyoc,sep=''),
    all( stateoc=="SD") ~ paste('46',countyoc,sep=''),
    all( stateoc=="MT") ~ paste('30',countyoc,sep=''),
    all( stateoc=="ND") ~ paste('38',countyoc,sep=''),
    all( stateoc=="MN") ~ paste('27',countyoc,sep=''),
    all( stateoc=="WI") ~ paste('55',countyoc,sep=''),
    all( stateoc=="PA") ~ paste('42',countyoc,sep=''),
    all( stateoc=="VA") ~ paste('51',countyoc,sep=''),
    all( stateoc=="MA") ~ paste('25',countyoc,sep=''),
    all( stateoc=="MI") ~ paste('26',countyoc,sep=''),
    all( stateoc=="DE") ~ paste('10',countyoc,sep=''),
    all( stateoc=="NY") ~ paste('36',countyoc,sep=''),
    all( stateoc=="MD") ~ paste('24',countyoc,sep=''),
    all( stateoc=="CT") ~ paste('09',countyoc,sep=''),
    all( stateoc=="NJ") ~ paste('34',countyoc,sep=''),
    all( stateoc=="RI") ~ paste('44',countyoc,sep=''),
    all( stateoc=="VT") ~ paste('50',countyoc,sep=''),
    all( stateoc=="NH") ~ paste('33',countyoc,sep=''),
    all( stateoc=="ME") ~ paste('23',countyoc,sep='')
    
    
  ))
U.states$CountyF=as.factor(U.states$CountyF)
states_pc$CountyF=as.factor(states_pc$GEOID)


states.counts=U.states %>% group_by(stateoc,CountyF) %>% summarise(nCounts=n())
county_geometry=as(states_pc$geometry %>% st_cast("POLYGON",group_or_split=FALSE), 'Spatial')

all.states.tpm=sp::merge(states.counts,states_pc,by="CountyF",all=TRUE)
all.states.tpm=subset(all.states.tpm, select = -c(moe,variable,NAME,geometry))
all.states=SpatialPolygonsDataFrame(Sr=county_geometry, data=all.states.tpm,match.ID=FALSE)

# plot rated population raster and number of deaths
# plot(population_raster)
# plot(all.states,add=TRUE)

#### Plots 

MGBorder = spTransform(county_geometry, projection(all_states))
MGBorderouter <- rgeos::gUnaryUnion(MGBorder)

poprastercols <- mapmisc::colourScale(
  population_raster,
  breaks = round(quantile(values(population_raster)[values(population_raster)!=0],(0:9)/9,na.rm=TRUE),-2),
  dec=-1,
  style =  'fixed',
  col = "Oranges"
)

plotraster <- mask(population_raster,all_states)

pdf(paste0('./',"populationraster-Allpopulation",".pdf"),width = 12,height = 7)
#mapmisc::map.new(plotraster)
plot(plotraster)
plot(plotraster,col = poprastercols$col,
     breaks = poprastercols$breaks, add=TRUE, legend = FALSE, lwd=0.5)
plot(MGBorder, add=TRUE, lwd=0.5)
plot(MGBorderouter,add = TRUE)
#mapmisc::legendBreaks("right",poprastercols, cex=1.5,bty = 'n')
dev.off()

#mapmisc::legendBreaks("right",poprastercols, cex=1.5,bty = 'n')

pdf(paste0('./',"lengend",".pdf"),width = 12,height = 7)
mapmisc::map.new(plotraster)
mapmisc::legendBreaks("right",poprastercols, cex=1.5,bty = 'n')
dev.off()
# plot mortality count 
#spplot(all_states,zcol='nCounts')


### Plot mortality counts 

pdf(paste0('./',"polygoncounts-allstates",'.pdf'),width = 7,height = 7)


valscols <- mapmisc::colourScale(
  all.states$nCounts,
  breaks = c(1,50,100,500,2000,3000,5000,max(all.states$nCounts,na.rm=TRUE)),
  dec=-1,
  style =  'fixed',
  col = "Oranges"
)

# spplot(all.states,z='nCounts')
# dev.off()

mapmisc::map.new(all.states)
plot(all.states,col = valscols$plot, add=TRUE, border=mapmisc::col2html("black", 0.5), lwd=0.5)
plot(MGBorderouter,add = TRUE)
mapmisc::legendBreaks('topright',valscols, cex=1.5,bty = 'n',inset=.1)
#mapmisc::scaleBar(all.states, 'topleft', bty='n', cex=2)
# dev.off()


