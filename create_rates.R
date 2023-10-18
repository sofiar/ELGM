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


############################################################################
############################# Census data ##################################
############################################################################
rm(list=ls())
year=2009

# if we want to analyze data by race

# Race="all"
# race.mortality=c('nh-withe','hispanic','nh-black','nh-other')

#Race='White'
#race.mortality=c('nh-withe')

#Race='Black'
#race.mortality=c('nh-black')

Race='Hispanic'
race.mortality=c('hispanic')

#Race='Other'
#race.mortality=c('nh-other')

source('./select_variables.R')

### key to download data
source('set_key.R')

Names.states=c('WA','OR', 'ID','MT','WY','NV','CA','UT','CO','AZ','NM','ND',
               'SD','NE','KS','OK','TX','MN','IA','MO','AR','LA','WI','IL','MS','KY','TN','AL',
               'FL', 'IN','GA','OH','SC','NC','VA','WV','PA','NY','MD','MI','DE','NJ','CT',
               'RI','MA','VT','NH','ME')

#Names.states=c('TX')


# get data by tract
states_pt <- get_acs(geography = "tract", year=year,survey='acs5',
                     geometry = TRUE, state=Names.states,
                     variables=variables)

#states_pt=st_cast(states_pt, "POLYGON")
states_pt=states_pt[!st_is_empty(states_pt$geometry),]

df=data.frame(states_pt)
df$GEOID=as.factor(df$GEOID)

## Create sex and age factors 
sex=numeric(length(df$variable))
sex[grepl('Male', df$variable)]='M'
sex[grepl('Female', df$variable)]='F'

age=numeric(length(df$variable))
for (i in 1:length(gr.a)) ## number of age class
{
  ff=paste('a',as.character(i),sep='_')
  age[grepl(ff, df$variable)]=paste('a',as.character(i),sep='')
}

df$sex=as.factor(sex)
df$age=as.factor(age)

population.counts=df %>% group_by(sex,age) %>% summarise(K.population=sum(estimate))
population.counts=population.counts %>% mutate(age.sex=paste(as.character(sex),as.character(age),sep=''))

############################################################################
############################## Mortality data ##############################
############################################################################

filecsv=paste('tidy_data/tidy_mortalityData_',as.character(year),'.csv',sep='')
dyear=data.frame(fread(filecsv, header = T))
dyear=dyear[-which(dyear$age==''),]

# modify data frame 
dyear$race=as.factor(dyear$race)
dyear$stateoc=as.factor(dyear$stateoc)
dyear$age=as.factor(dyear$age)
dyear$sex=as.factor(dyear$sex)

# filter by race
dyear=dyear %>% filter(race%in%race.mortality)

Death.counts=dyear %>% dplyr::filter(stateoc%in%c(Names.states)) %>% group_by(sex,age) %>% summarise(nCounts=n())
Death.counts=Death.counts %>% mutate(age.sex=paste(as.character(sex),as.character(age),sep=''))

data.rates=merge(population.counts, Death.counts, by = c('age.sex')) 
final.rates=data.rates %>% group_by(age.sex) %>% summarise(rate.k=nCounts/K.population)

# calculate P_il
df=df %>% mutate(age.sex=paste(as.character(sex),as.character(age),sep=''))

rate.k=numeric(length(df$age.sex))

for (i in 1:length(final.rates$age.sex))
{
  rate.k[df$age.sex==final.rates[i,1]$age.sex]=final.rates[i,2]$rate.k
}
df$rate.k=rate.k

df = df %>% mutate(pop_times_rate= estimate*rate.k)

### Now we sum over tract every \sum_k P_kj= ~P_j
d=as.data.frame(df) %>% group_by(GEOID) %>% summarise(pop.r=sum(pop_times_rate),name=unique(NAME))
d$pop.r[d$pop.r==0]=1e-07# eliminate 0's to avoid NaNs
### create raster object 
reduce.df=states_pt %>% filter(variable==names(variables[1]))
reduce.df=reduce.df[!duplicated(reduce.df$GEOID),] # commentme
geometry=as(reduce.df$geometry %>% st_cast("MULTIPOLYGON",group_or_split=FALSE), 'Spatial')

quienes=match(reduce.df$NAME,d$name)
d=d[quienes,]
rated_population=SpatialPolygonsDataFrame(Sr=geometry, data=d,match.ID=FALSE)

# spplot(rated_population,zcol='pop.r')
# population_raster = geostatsp::spdfToBrick(rated_population, geostatsp::squareRaster(rated_population, 200)
#                                            ,pattern = 'pop.r',  logSumExpected = FALSE)

########### Change crs in meters ############

#meters.crs= crs("+init=epsg:2163")
#rated_population_trans = spTransform(rated_population, meters.crs)
#population_raster = geostatsp::spdfToBrick(rated_population, geostatsp::squareRaster(rated_population_trans, 500)
#            ,pattern = 'pop.r',  logSumExpected = FALSE)

# not changing to mtrs
population_raster = geostatsp::spdfToBrick(rated_population, geostatsp::squareRaster(rated_population, 500)
            ,pattern = 'pop.r',  logSumExpected = FALSE)


# check equal values 
sum(Death.counts$nCounts)
sum(rated_population$pop.r,na.rm=TRUE)
sum(values(population_raster), na.rm=TRUE)*prod(res(population_raster))

## multiplys by resolution 
values(population_raster)=values(population_raster)*prod(res(population_raster))

# Create deaths counts by county
U.states=dyear %>% filter(stateoc%in%Names.states)
U.states$countyoc=str_pad(U.states$countyoc, 3, pad = "0")

# for 2009
if(nchar(U.states$countyoc[1])==5)
{
U.states=U.states%>%mutate(countyoc=substr(countyoc, 3, 5))
}

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

states.counts=U.states %>% group_by(CountyF) %>% summarise(nCounts=n())
states.counts=states.counts[!duplicated(states.counts$CountyF),]

### get county geometry
states_p <- get_acs(geography = "county", year=year,survey='acs5',
                    state = Names.states,geometry = TRUE,
                    variables=c(population = "B01001_001"))


d=states_p$geometry %>% st_cast("MULTIPOLYGON",group_or_split=FALSE)
county_geometry=as(d, 'Spatial')

states_p$CountyF=as.factor(states_p$GEOID)
states_p=states_p[!duplicated(states_p$CountyF),]# to avoid repeted polygons

#all.states.tpm=sp::merge(states.counts,states_p,by="CountyF",all=FALSE)
# cambio esto
all.states.tpm=states_p
ncounts=numeric(dim(all.states.tpm)[1])
quienes=match(states.counts$CountyF,states_p$CountyF)

if (sum(is.na(quienes))>0){
  indx.nones=which(is.na(quienes))
  ncounts[na.omit(quienes)]=states.counts$nCounts[-indx.nones]
  ncounts[indx.nones]=NA
}else{
ncounts[quienes]=states.counts$nCounts
}

all.states.tpm$nCounts=ncounts 

#all.states.tpm=all.states.tpm[!duplicated(all.states.tpm$GEOID),]
# get state variable
s=sub("^[^,]*, ", "", all.states.tpm$NAME)
all.states.tpm$state=as.factor(s)

#d=states_p$CountyF[!all.states.tpm$CountyF%in%states_p$CountyF] 
#all.states.tpm=all.states.tpm[!all.states.tpm$CountyF==d,]

county_geometry1=as(all.states.tpm$geometry %>% st_cast("MULTIPOLYGON",group_or_split=FALSE), 'Spatial')
all.states=SpatialPolygonsDataFrame(Sr=county_geometry1, data=data.frame(all.states.tpm),match.ID=FALSE)

# change crs to meters
#all_states <- spTransform(all.states, meters.crs)
# not changing to mtrs
all_states <- spTransform(all.states, crs(population_raster)) 

# plot rated population raster and number of deaths
# plot(population_raster)
# plot(all_states,add=TRUE)population_raster
# spplot(all_states,zcol='nCounts')





### Save raster and spatial polygon 
file_name=paste(paste(Race,'rated_population_np',year,sep='_'),'.tif',sep='')
url=paste('./tidy_data/',file_name,sep='')
writeRaster(population_raster, url,overwrite=TRUE)

file_name=paste(paste(Race,'mortality_counts_np',year,sep='_'),'.shp',sep='')
url=paste('./tidy_data/',file_name,sep='')
raster::shapefile(all_states, url,overwrite=TRUE)

#file_name=paste(paste('county_geometry',year,sep='_'),'.shp',sep='')
#url=paste('./tidy_data/',file_name,sep='')
#raster::shapefile(county_geometry, url,overwrite=TRUE)




