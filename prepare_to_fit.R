library(haven)
library(tidyverse)
library(tidycensus)
library(acs)
library(ggplot2)
library(sf)
library(sp)
library(raster)
library(stars)

data_2012 = read_dta('./data/us_county2012.dta')

### select some variables 

D2012 = data_2012 %>% dplyr::select(countyoc,stateoc,monthdth,sex,weekday,year,mandeath,ucod)
rm(data_2012)

## select deaths due to (1) malignant neoplasm of bronchus and lung
## and (2) chronic obstructive pulmonary disease
wh1=which(str_detect(D2012$ucod,'C34')==1)
wh2=which(str_detect(D2012$ucod,'J44')==1)

D2012F = D2012[c(wh1,wh2),]
D2012F$stateoc=as.factor(D2012F$stateoc)

# a=D2012F %>% group_by(stateoc) %>% summarise(nCounts=n())
# View(a)

Names.states=c('NM', 'TX','OK','AZ','UT','CO','KS','NV','LA', 'AR','IA','IL','NV',
              'CA','NE','MS','TN','AL','KY',"IN",'OH','GA','SC', 'FL','MO', 
		'NC','WV','NE','OR','ID','WA','WY','SD','MT','ND','MN','WI',
		'PA','VA','MI','NJ','NY','MA','DE','CT','RI','MD','VT','NH','ME')
		
	

U.states=D2012F %>% filter(stateoc%in%Names.states)
# to get county number FIPS codes
#fips_codes %>% filter(state_name=='Florida')

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
    all( stateoc=="NV") ~ paste('17',countyoc,sep=''),
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
    all( stateoc=="NE") ~ paste('31',countyoc,sep=''),
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
    all( stateoc=="NJ") ~ paste('34',countyoc,sep=''),
    all( stateoc=="DE") ~ paste('10',countyoc,sep=''),
    all( stateoc=="MI") ~ paste('26',countyoc,sep=''),
    all( stateoc=="NY") ~ paste('36',countyoc,sep=''),
    all( stateoc=="MA") ~ paste('25',countyoc,sep=''),
    all( stateoc=="CT") ~ paste('09',countyoc,sep=''),
    all( stateoc=="RI") ~ paste('44',countyoc,sep=''),
    all( stateoc=="MD") ~ paste('24',countyoc,sep=''),
    all( stateoc=="VT") ~ paste('50',countyoc,sep=''),
    all( stateoc=="NH") ~ paste('33',countyoc,sep=''),
    all( stateoc=="ME") ~ paste('23',countyoc,sep='')

    ))


states.counts=U.states %>% group_by(CountyF) %>% summarise(nCounts=n())

# set key to dowload data
source('./set_key.R')

year = 2012
### US state ###
states_p <- get_acs(geography = "county", year=year,survey='acs5',
                    state = Names.states,geometry = TRUE,
                    variables=c(population = "B01001_001"))

U.states$CountyF=as.factor(U.states$CountyF)
states_p$CountyF=as.factor(states_p$GEOID)

pt2=states_p$geometry %>% sf::st_cast("POLYGON",group_or_split=FALSE)
county_geometry=as(states_p$geometry %>% st_cast("POLYGON",group_or_split=FALSE), 'Spatial')

all.states.tpm=sp::merge(states.counts,states_p,by="CountyF",all=TRUE)
all.states.tpm=subset(all.states.tpm, select = -c(moe,variable,NAME,geometry))
all.states=SpatialPolygonsDataFrame(Sr=county_geometry, data=all.states.tpm,match.ID=FALSE)

# # Plot mortality counts
#spplot(all.states,zcol="nCounts",axes=TRUE)
# # Plot population size
#spplot(all.states,zcol="estimate",axes=TRUE)

# # Population by tract
population_by_tracts <- get_acs(geography = "tract", year=year,survey='acs5',
                                state = Names.states,geometry = TRUE,
                                variables=c(population = "B01001_001"))

pt1=population_by_tracts$geometry %>% st_cast("POLYGON",group_or_split=FALSE)

tract_geometry=as(pt1, 'Spatial')
all.population=SpatialPolygonsDataFrame(Sr=tract_geometry, 
                                        data=data.frame('estimate'=population_by_tracts$estimate),FALSE)

# spplot(all.population,zcol="estimate",axes=TRUE)
# plot(tract_geometry)
# lines(county_geometry,col='red')
#all.states$nCounts[which(is.na(all.states$nCounts))]=0

## Rasterize population data and set
rt_pz<-st_rasterize(population_by_tracts %>% dplyr::select(estimate, geometry))#,dx=.3,dy=.3)
write_stars(rt_pz, "popuation_size.tif")
population_raster = raster("popuation_size.tif")

save.image("statesDta.RData")

#check area and plot
# area(population_raster)
# plot(population_raster)
# lines(county_geometry,col='red')
# lines(tract_geometry,col='blue')

