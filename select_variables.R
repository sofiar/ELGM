
library(tidyverse)
library(tidycensus)
library(acs)
library(ggplot2)
library(sf)
library(sp)
library(viridis)
library(stars)
library(data.table)

sex=c('Male','Female')
 

variables=c()
names.v=c()


if (year==2005){
all_vars_acs5 <-load_variables(year = year, dataset = "acs1")
}else{
all_vars_acs5 <-load_variables(year = year, dataset = "acs5")
}

all_vars_acs5 = all_vars_acs5[-dim(all_vars_acs5)[1],]


if (Race=='all')
{
  
  gr.a=list('Under 5','5 to 9 years','10 to 14 years', c('15 to 17 years','18 and 19 years'),
            c('20 years', '21 years', '22 to 24 years'), '25 to 29 years','30 to 34 years',
            c('35 to 39 years','40 to 44 years'),c('45 to 49 years','50 to 54 years'),
            c('55 to 59 years','60 to 61 years','62 to 64 years'),
            c('65 to 66 years','66 to 69 years','70 to 74 years'),c('75 to 79 years','80 to 84 years'),'85 years and over')
  
  
  
  df=all_vars_acs5[all_vars_acs5$concept=='SEX BY AGE',]
  
  for (s in sex)
    {
      df.S=df[str_detect(df$label,s),]
      for (i in 1:length(gr.a))
      {
        for (j in 1:length(gr.a[[i]])){
          nam=paste(paste(s,'all','a',sep='.'),i,sep='_')
          new.vars=df.S[str_detect(df.S$label,gr.a[[i]][j]),1]$name
          variables=c(variables,new.vars)
          names.v=c(names.v,rep(nam,length(new.vars)))
      }
      }
  } 
  names(variables)=names.v
  rm(names.v,s,i,j,new.vars,df,df.S,sex,nam,all_vars_acs5)
  } else{

# by race

race= Race
gr.a=list('Under 5','5 to 9 years','10 to 14 years', c('15 to 17 years','18 and 19 years'),'20 to 24 years',
          '25 to 29 years','30 to 34 years', '35 to 44 years','45 to 54 years','55 to 64 years',
          '65 to 74 years','75 to 84 years','85 years and over')

all_whites=all_vars_acs5[all_vars_acs5$concept=='SEX BY AGE (WHITE ALONE, NOT HISPANIC OR LATINO)',]
all_black=all_vars_acs5[all_vars_acs5$concept=='SEX BY AGE (BLACK OR AFRICAN AMERICAN ALONE)',]
all_hispanic=all_vars_acs5[all_vars_acs5$concept=='SEX BY AGE (HISPANIC OR LATINO)',]

others_name=c('SEX BY AGE (AMERICAN INDIAN AND ALASKA NATIVE ALONE)','SEX BY AGE (ASIAN ALONE)',
              'SEX BY AGE (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)',
              'SEX BY AGE (SOME OTHER RACE ALONE)')

all_others=all_vars_acs5[all_vars_acs5$concept%in%others_name,]

if (Race=='White'){dfs_by_race=all_whites}
if (Race=='Black'){dfs_by_race=all_black}
if (Race=='Hispanic'){dfs_by_race=all_hispanic}
if (Race=='Other'){dfs_by_race=all_others}

df=dfs_by_race
for (s in sex)
{
  df.S=df[str_detect(df$label,s),]
  for (i in 1:length(gr.a))
   {
    for (j in 1:length(gr.a[[i]])){
    nam=paste(paste(s,race,'a',sep='.'),i,sep='_')
    new.vars=df.S[str_detect(df.S$label,gr.a[[i]][j]),1]$name
    variables=c(variables,new.vars)
    names.v=c(names.v,rep(nam,length(new.vars)))
    }
}
}

names(variables)=names.v

rm(names.v,s,i,j,new.vars,df,df.S,sex,race,others_name,nam,dfs_by_race,all_black,all_whites,
   all_others,all_vars_acs5)

}


