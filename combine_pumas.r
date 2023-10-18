### Combine data with PUMAs resolution 
rm(list=ls())
setwd("/u/ruizsuar/ELGM")
library(tidyverse)
library(tidycensus)
library(acs)
library(ggplot2)
library(sf)
library(sp)
library(viridis)
library(stars)
library(data.table)
library(tigris)
library(raster)
library(geostatsp)

complete_with_zeros <- function(x,n) {
  if (nchar(x) < n) {
    return(paste0(substr(rep("00000"), 1, n - nchar(x)), x))
  } else {
    return(x)
  }
}

complete_geoid <- function(x) {
options(scipen = 999)
x.n=x*100
num_digits <- floor(log10(abs(x.n))) + 1
out=paste0(substr(rep("000000"), 1, 6 - num_digits),x.n)   
return(out)
}

############################################################################
############################# Census data ##################################
############################################################################
year=2012
Race='White'
race.mortality=c('nh-withe')
#education='Less-E'
education='University'

source('./select_variables.R')

### key to download data
source('set_key.R')
Names.states=c('WA','OR', 'ID','MT','WY','NV','CA','UT','CO','AZ','NM','ND',
               'SD','NE','KS','OK','TX','MN','IA','MO','AR','LA','WI','IL','MS','KY','TN','AL',
               'FL', 'IN','GA','OH','SC','NC','VA','WV','PA','NY','MD','MI','DE','NJ','CT',
               'RI','MA','VT','NH','ME')

# get data by tract
states_pt <- get_acs(geography = "tract", year=year,survey='acs5',
                     geometry = TRUE, state=Names.states,
                     variables=variables)

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

############################################################################
############################## Combine with PUMAs data #####################
############################################################################

# get PUMAs data
st_pums0 <- get_pums(variables = c("PUMA", "SEX","AGEP", "SCHL","RAC1P"), state = Names.states, survey = "acs1", year =year)
st_pums0$AGEP=as.numeric(st_pums0$AGEP)
st_pums0$SCHL=as.numeric(st_pums0$SCHL)
st_pums0$EDC=as.numeric(st_pums0$SCHL)
st_pums0$AGEF <- cut(st_pums0$AGEP,
                       breaks=c(0,5, 10, 15, 20, 30,35,45,55,65,75,80,100),
                     include.lowest = TRUE)

st_pums0$RAC1P[st_pums0$RAC1P==1]='White'
st_pums0$RAC1P[st_pums0$RAC1P==2]='Black'
st_pums0$RAC1P[st_pums0$RAC1P%in%c(3:9)]='Others'

st_pums0$EDC[st_pums0$SCHL<16]='Less-E'
st_pums0$EDC[st_pums0$SCHL==16]='HS'
st_pums0$EDC[st_pums0$SCHL>16 & st_pums0$SCHL<=20]='Collage'
st_pums0$EDC[st_pums0$SCHL>=20]='University'
st_pums0$EDC=as.factor(st_pums0$EDC)

st_tot <- st_pums0 %>%
  group_by(ST, PUMA,SEX,EDC,RAC1P) %>%
  summarize(
    total_pop = sum(PWGTP))

st_tot$PUMA <- sapply(st_tot$PUMA, complete_with_zeros,6)
st_tot=st_tot%>%mutate(SPUMA=paste(ST,PUMA,sep=''))
st_tot=st_tot%>% filter(RAC1P==Race) # filter race we want

### Get proportions p_e
r1=st_tot%>%group_by(EDC,SPUMA)%>%summarize(tot_pe=sum(total_pop))
r2=st_tot%>%group_by(SPUMA)%>%summarize(tot_pop=sum(total_pop))
ratio_df <- inner_join(r1, r2, by = "SPUMA") %>%
  mutate(ratio = tot_pe / tot_pop)%>%dplyr::filter(EDC==education) # filter education level we want

# census tracts into PUMAs
pumas.ct = read.csv("geocorr2014.csv")
pumas.ct = pumas.ct%>%dplyr::filter(stab%in%Names.states) # filter only states we want
pumas.ct$tract = sapply(pumas.ct$tract, complete_geoid)
pumas.ct$puma12 = sapply(pumas.ct$puma12, complete_with_zeros,n=6)
pumas.ct$county=sapply(pumas.ct$county,complete_with_zeros,n=5)
pumas.ct=pumas.ct%>%mutate(GEOID=paste(county,tract,sep=''))
pumas.ct=pumas.ct%>%mutate(SPUMA=paste(state,puma12,sep=''))
pumas.ct=pumas.ct%>%dplyr::select(SPUMA,GEOID)

#range(pumas.ct$GEOID)
#range(as.character(df$GEOID))

###########################################
##### Missing PUMAs for census tracts #####
###########################################
M1=match(pumas.ct$GEOID,df$GEOID)
length(pumas.ct$GEOID[is.na(M1)])

MM=match(df$GEOID,pumas.ct$GEOID)
huerfanos=unique(df[which(is.na(MM)),]$GEOID)
qq=match(huerfanos,states_pt$GEOID)
tracts.huerfanos=states_pt[qq,]
states_huerfanos <- substr(tracts.huerfanos$GEOID, 1, 2)
missing_ct_pumas=matrix(NA,ncol=3,nrow=0)

# Group the elements based on the state
grouped_list <- split(tracts.huerfanos$GEOID, states_huerfanos)
cat('computing missing census tracts...')
for(j in 1:length(grouped_list)) # looping over states
{
 state_code=as.numeric(substr( grouped_list[[j]][1], 1, 2))
# get pumas  data for that state
states.p=pumas(state=state_code,year=year)
# get census tracts in that state
ct_huerfanos=grouped_list[[j]]
      for (ii in 1:length(ct_huerfanos)) # looping over census tracts
      {
      indx_all=match(ct_huerfanos[ii],tracts.huerfanos$GEOID)
      dists=st_distance(tracts.huerfanos$geometry[indx_all],states.p$geometry)
      closest_polygon_index <- which.min(dists)
      missing_ct_pumas=rbind(missing_ct_pumas,
      cbind(states.p[closest_polygon_index,]$PUMACE10,ct_huerfanos[ii],state_code))
      }
      #print(j)
}

# complete missing census tracts
missing_ct_pumas=as.data.frame(missing_ct_pumas)
names(missing_ct_pumas)=c('SPUMA','GEOID','State_code')
missing_ct_pumas$SPUMA = sapply(missing_ct_pumas$SPUMA, complete_with_zeros,n=6)
missing_ct_pumas=missing_ct_pumas%>%mutate(SPUMA=paste(State_code,SPUMA,sep=''))
missing_ct_pumas=missing_ct_pumas%>%dplyr::select(SPUMA,GEOID)
pumas.ct=rbind(pumas.ct,missing_ct_pumas) # complete with missing census tracts

# to check correctly PUMAs codes
#length(unique(pumas.ct$SPUMA))
#length(unique(ratio_df$SPUMA))
#sum(is.na(match(pumas.ct$SPUMA,ratio_df$SPUMA)))

all <- inner_join(pumas.ct, ratio_df, by = "SPUMA")%>%dplyr::filter(EDC==education)
df_e=inner_join(all,df,by='GEOID')%>%mutate(estimate=estimate*ratio) # estimate population in this education level

population.counts=df_e %>% group_by(sex,age) %>% summarise(K.population=sum(estimate))
population.counts=population.counts %>% mutate(age.sex=paste(as.character(sex),as.character(age),sep=''))

############################################################################
############################## Mortality data ##############################
############################################################################

filecsv=paste('tidy_data/tidy_mortalityData_',as.character(year),'.csv',sep='')
states_names <- read.csv("states_names.csv")

dyear=data.frame(fread(filecsv, header = T))

if (sum(dyear$age=='')>0){
dyear=dyear[-which(dyear$age==''),]
}

# modify data frame 
dyear = dyear%>% dplyr::filter(stateoc%in%c(Names.states))
dyear$race=as.factor(dyear$race)
dyear$stateoc=as.factor(dyear$stateoc)
dyear$age=as.factor(dyear$age)
dyear$sex=as.factor(dyear$sex)

# get states without educ information
a=dyear%>%group_by(stateoc)%>%  filter(all(is.na(educ)))
states_no_educ=unique(a$stateoc)

# filter by race and education level
dyear=dyear%>%dplyr::filter(educ==education)
dyear=dyear %>% filter(race%in%race.mortality)
  
#dyear %>%filter(stateoc=='KY',countyoc==19) %>% group_by(sex,age) %>% summarise(nCounts=n())
Death.counts=dyear %>% group_by(sex,age) %>% summarise(nCounts=n())
Death.counts=Death.counts %>% mutate(age.sex=paste(as.character(sex),as.character(age),sep=''))

data.rates=merge(population.counts, Death.counts, by = c('age.sex')) 
final.rates=data.rates %>% group_by(age.sex) %>% summarise(rate.k=nCounts/K.population)

# calculate P_il
df_e=df_e %>% mutate(age.sex=paste(as.character(sex),as.character(age),sep=''))
rate.k=numeric(length(df_e$age.sex))

for (i in 1:length(final.rates$age.sex))
{
  rate.k[df_e$age.sex==final.rates[i,1]$age.sex]=final.rates[i,2]$rate.k
}
df_e$rate.k=rate.k

df_e = df_e %>% mutate(pop_times_rate= estimate*rate.k)

### Now we sum over tract every \sum_k P_kj= ~P_j
d=as.data.frame(df_e) %>% group_by(GEOID) %>% summarise(pop.r=sum(pop_times_rate),name=unique(NAME))
d$pop.r[d$pop.r==0]=1e-07# eliminate 0's to avoid NaNs
### create raster object 
reduce.df=states_pt %>% filter(variable==names(variables[1]))
reduce.df=reduce.df[!duplicated(reduce.df$GEOID),] # commentme
geometry=as(reduce.df$geometry %>% st_cast("MULTIPOLYGON",group_or_split=FALSE), 'Spatial')

quienes=match(reduce.df$NAME,d$name)
d=d[quienes,]
rated_population=SpatialPolygonsDataFrame(Sr=geometry, data=d,match.ID=FALSE)

#spplot(rated_population,zcol='pop.r')
# get raster
population_raster = geostatsp::spdfToBrick(vect(rated_population), 
geostatsp::squareRaster(vect(rated_population), 500)
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


#first_two_digits <- substr(states_p$CountyF, 1, 2)
#grouped_list <- split(states_p$CountyF, first_two_digits)
#grouped_list[['05']]
#a=U.states%>%dplyr::filter(stateoc=='AR')
#range(a$countyoc)
#range(as.character(states.counts$CountyF))
#range(as.character(states_p$CountyF))

# check all county are correct, this should be zero
sum(is.na(match(states.counts$CountyF,states_p$CountyF)))

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

# get state variable
s=sub("^[^,]*, ", "", all.states.tpm$NAME)
all.states.tpm$state=as.factor(s)

# set to NA all states with no educ info
quienes_no_educ=states_names[states_names$Abbreviations%in%c(states_no_educ),]$Name
if (length(quienes_no_educ)>0)
{
all.states.tpm[all.states.tpm$state%in%c(quienes_no_educ),]$nCounts=NA
}


county_geometry1=as(all.states.tpm$geometry %>% st_cast("MULTIPOLYGON",group_or_split=FALSE), 'Spatial')
all.states=SpatialPolygonsDataFrame(Sr=county_geometry1, data=data.frame(all.states.tpm),match.ID=FALSE)

# change crs to meters
#all_states <- spTransform(all.states, meters.crs)
# not changing to mtrs
all_states <- spTransform(all.states, crs(population_raster)) 

# plot rated population raster and number of deaths
# plot(population_raster,type="classes")
# plot(all_states,add=TRUE)
# spplot(all_states,zcol='nCounts')

### Save raster and spatial polygon 
file_name=paste(paste(Race,'rated_population_np',year,education,sep='_'),'.tif',sep='')
url=paste('./tidy_data/',file_name,sep='')
writeRaster(population_raster, url,overwrite=TRUE)

file_name=paste(paste(Race,'mortality_counts_np',year,education,sep='_'),'.shp',sep='')
url=paste('./tidy_data/',file_name,sep='')
raster::shapefile(all_states, url,overwrite=TRUE)

#file_name=paste(paste('county_geometry',year,sep='_'),'.shp',sep='')
#url=paste('./tidy_data/',file_name,sep='')
#raster::shapefile(county_geometry, url,overwrite=TRUE)
