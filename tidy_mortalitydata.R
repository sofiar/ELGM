library(data.table)
library(tidyverse)
library(haven)
year=2015

#### Mortality data 
filename=paste('/u/ruizsuar/ELGM/data/US_mort_05t015/us_county',as.character(year),'.dta',sep='')
data_year = read_dta(filename)

## select deaths due to (1) malignant neoplasm of bronchus and lung
## and (2) chronic obstructive pulmonary disease

wh1=which(str_detect(data_year$ucod,'C34')==1)
wh2=which(str_detect(data_year$ucod,'J44')==1)
data_year = data_year[c(wh1,wh2),]

if (year==2005)
{
dyear = data_year %>% dplyr::select(countyoc,stateoc,monthdth,sex,year,mandeath,ucod,race,hispanic, hspanicr,
                                sex,ager27,hspanicr,educ89,educflag,educ)
}else{
dyear = data_year %>% dplyr::select(countyoc,stateoc,monthdth,sex,year,mandeath,ucod,race,hispanic, hspanicr,
                                 sex,ager27,hspanicr,educ2003,educ1989,educflag)
}
rm(data_year)

# Create race factor: hispanic, non-hispanic black,non-hispanic withe and non-hispanic other.  
race=rep(NA,length(dyear$sex))
race[dyear$hispanic>=200]='hispanic'
race[dyear$hispanic<200 & dyear$hspanicr ==6]='nh-withe'
race[dyear$hispanic<200 & dyear$hspanicr ==7]='nh-black'
race[dyear$hispanic<200 & dyear$hspanicr ==8]='nh-other'
dyear$race=race

educ=rep(NA,length(dyear$sex))
if(year==2005)
{
dyear$educ2003=dyear$educ
dyear$educ1989=dyear$educ89
dyear$educ=NULL
}

educ[dyear$educ2003%in%c(1,2)]='Less-E'
educ[dyear$educ2003==3]='HS'
educ[dyear$educ2003%in%c(4,5)]='Collage'
educ[dyear$educ2003%in%c(6,7,8)]='University'
#educ[is.na(dyear$educ2003)]=NA

dyear$educ=educ


dyear=dyear %>%
  mutate(
    educ = case_when(
      educflag==0 & educ1989 %in% c(0:11) ~ 'Less-E',
      educflag==0 & educ1989 == 12 ~ 'HS',
      educflag==0 & educ1989 %in% c(13:16) ~ 'College',
      educflag==0 & educ1989 == 17 ~ 'University',
      TRUE ~ educ  # Keep the original 'educ' value if none of the conditions are met
    )
  )

#ga=dyear%>%filter(stateoc=='GA')
#View(ga)
# create age levels 
#age.f=rep(NA,length(dyear$sex))
#dyear$age = cut(dyear$ager27,breaks = c(1,seq(6, 21),27))
#levels(dyear$age)=as.factor(paste('a',1:17,sep=''))
dyear$age = cut(dyear$ager27,breaks = c(1,seq(6, 12),14,16,18,20,22,27))
levels(dyear$age)=as.factor(paste('a',1:13,sep=''))

# A1=dyear$ager27 %in% 1:6
# A2=dyear$ager27 %in% 7
# A3=dyear$ager27 %in% 8
# A4=dyear$ager27 %in% 9
# A5=dyear$ager27 %in% 10
# A6=dyear$ager27 %in% 11
# A7=dyear$ager27 %in% 12
# A8=dyear$ager27 %in% 13
# A9=dyear$ager27 %in% 14
# A10=dyear$ager27 %in% 15
# A11=dyear$ager27 %in% 16
# A12=dyear$ager27 %in% 17
# A13=dyear$ager27 %in% 18
# A14=dyear$ager27 %in% 19
# A15=dyear$ager27 %in% 20
# A16=dyear$ager27 %in% 21
# A17=dyear$ager27 %in% 22:26

# Fs=dyear$sex=='F'
# Ms=dyear$sex=='M'
# 
# age.f[A1 & Ms] = "male.a1"
# age.f[A2 & Ms] = "male.a2"
# age.f[A3 & Ms] = "male.a3"
# age.f[A4 & Ms] = "male.a4"
# age.f[A5 & Ms] = "male.a5"
# age.f[A6 & Ms] = "male.a6"
# age.f[A7 & Ms] = "male.a7"
# age.f[A8 & Ms] = "male.a8"
# age.f[A9 & Ms] = "male.a9"
# age.f[A10 & Ms] = "male.a10"
# age.f[A11 & Ms] = "male.a11"
# age.f[A12 & Ms] = "male.a12"
# age.f[A13 & Ms] = "male.a13"
# age.f[A14 & Ms] = "male.a14"
# age.f[A15 & Ms] = "male.a15"
# age.f[A16 & Ms] = "male.a16"
# age.f[A17 & Ms] = "male.a17"
# 
# age.f[A1 & Fs] = "female.a1"
# age.f[A2 & Fs] = "female.a2"
# age.f[A3 & Fs] = "female.a3"
# age.f[A4 & Fs] = "female.a4"
# age.f[A5 & Fs] = "female.a5"
# age.f[A6 & Fs] = "female.a6"
# age.f[A7 & Fs] = "female.a7"
# age.f[A8 & Fs] = "female.a8"
# age.f[A9 & Fs] = "female.a9"
# age.f[A10 & Fs] = "female.a10"
# age.f[A11 & Fs] = "female.a11"
# age.f[A12 & Fs] = "female.a12"
# age.f[A13 & Fs] = "female.a13"
# age.f[A14 & Fs] = "female.a14"
# age.f[A15 & Fs] = "female.a15"
# age.f[A16 & Fs] = "female.a16"
# age.f[A17 & Fs] = "female.a17"

# dyear$age.f=age.f
# rm(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17)
# rm(Fs,Ms)

#dyear = dyear %>% dplyr::select(countyoc,stateoc,sex,year,race,educ, sex,age.f)
dyear = dyear %>% dplyr::select(countyoc,stateoc,sex,year,race,educ, sex,age)

filecsv=paste('/u/ruizsuar/ELGM/tidy_data/tidy_mortalityData_',as.character(year),'.csv',sep='')
# write file
fwrite(dyear,filecsv)
