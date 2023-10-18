library(aghq)
library(TMB)
library(disaggregation)
library(raster)
library(stars)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(geodata)
library(rgdal)
library(rasterVis)

# Set race
#Race='all'
Race='White'
#Race='Black'
#Race= 'Hispanic'
#Race='Other'

year=2012
#education='Less-E'
education='University'


#### Load data #####
#load("statesDta.RData")

### Rated population raster ###
# no education level
#file_name=paste(paste(Race,'rated_population_np',year,sep='_'),'.tif',sep='')
#url=paste('./tidy_data/',file_name,sep='')
#population_raster2 = raster(url)
# by education level
file_name=paste(paste(Race,'rated_population_np',year,education,sep='_'),'.tif',sep='')
url=paste('./tidy_data/',file_name,sep='')
population_raster = raster(url)


### Mortality data ###
# no  education level
#file_name=paste(paste(Race,'mortality_counts_np',year,sep='_'),'.shp',sep='')
#url=paste('./tidy_data/',file_name,sep='')
#all_states2=shapefile(url) 
# by education level
file_name=paste(paste(Race,'mortality_counts_np',year,education,sep='_'),'.shp',sep='')
url=paste('./tidy_data/',file_name,sep='')
all_states=shapefile(url) 

## plots
plotraster <- terra::mask(population_raster,all_states)
breaks=quantile(na.omit(values(plotraster)),probs=c(0.05,0.15,0.25,0.35,0.5,0.75,0.85,0.95,1))
plot(plotraster, 
     breaks =breaks, 
     col = (brewer.pal(length(breaks),"Blues")))


spplot(all_states,zcol='nCounts')

### County geometry data ###
file_name=paste(paste('county_geometry',year,sep='_'),'.shp',sep='')
url=paste('./tidy_data/',file_name,sep='')
county_geometry=shapefile(url) 

#observed vs expected
er <- raster::extract(population_raster, all_states)
ss_values <- lapply(er, sum, na.rm = TRUE)
all_states$ssExpected <- unlist(ss_values)
spplot(all_states,zcol='ssExpected')

all_states$rate=all_states$nCounts/all_states$ssExpected 
spplot(all_states,zcol='rate')

all_states$rate[which.max(all_states$rate)]
all_states$nCounts[which.max(all_states$rate)]
all_states$ssExpected[which.max(all_states$rate)]

all_states$state[which.max(all_states$rate)]
all_states$CountyF[which.max(all_states$rate)]

# to fix inla mesh
# mesh.args = list(max.edge = c(0.5,8),
#                  cut =0.5,
#                  offset = c(1, 2)),


dis_data <- prepare_data(
  polygon_shapefile =all_states, 
  covariate_rasters = population_raster,
  aggregation_raster = population_raster,
  id_var = 'GEOID', 
  response_var = 'nCounts', 
  na.action = TRUE, 
  ncores = 8
)

#plot(dis_data$mesh)
#plot(dis_data)

## Fit their model ----
tau_u <- .1
tau_alpha <-.01
sigma_u <- 1
rho_u <-3
rho_alpha <-.01
sigma_alpha <-.01

## Prepare model object ----
# Need the TMB function object
# from make_model_object()
data = dis_data
family = 'poisson'
link = 'log'
# set priors 
priors = list(
  priormean_intercept = -5,
  priorsd_intercept = 10, 
  priormean_slope = 0.0, 
  priorsd_slope = 0.5, 
  prior_rho_min = rho_u, 
  prior_rho_prob =rho_alpha, 
  prior_sigma_max = sigma_u, 
  prior_sigma_prob =sigma_alpha,
  prior_iideffect_sd_max=tau_u,
  prior_iideffect_sd_prob =tau_alpha)

field <- iid <- TRUE
family_id <- 2 # Poisson
link_id <- 1 # Log link 

nu = 1
spde <- (INLA::inla.spde2.matern(data$mesh, alpha = nu + 1)$param.inla)[c("M0", "M1", "M2")]	
Apix <- INLA::inla.mesh.project(data$mesh, loc = data$coordsForFit)$A
n_s <- nrow(spde$M0)

# set covariates (but we do not need them)
cov_matrix <- as.matrix(data$covariate_data[, -c(1:2)])
if (ncol(cov_matrix) == 1) {  cov_matrix <- as.matrix(base::apply(cov_matrix, 1, as.numeric))
}else {cov_matrix <- t(base::apply(cov_matrix, 1, as.numeric))}

parameters <- list(intercept = -5,
                   slope =  rep(0, ncol(cov_matrix)),
                   log_tau_gaussian = 8,
                   iideffect = rep(0, nrow(data$polygon_data)),
                   iideffect_log_tau = 1,
                   log_sigma = 0,
                   log_rho = 4,
                   nodemean = rep(0, n_s))

input_data <- list(x = cov_matrix,
                   aggregation_values = data$aggregation_pixels,
                   Apixel = Apix,
                   spde = spde,
                   startendindex = data$startendindex,
                   polygon_response_data = data$polygon_data$response,
                   response_sample_size = data$polygon_data$N,
                   family = family_id,
                   link = link_id,
                   nu = nu,
                   field = as.integer(field),
                   iid = as.integer(iid))

input_data <- c(input_data, priors)
tmb_map <- list(log_tau_gaussian = as.factor(NA), slope=as.factor(NA))
## END: taken directly from disaggregate::make_model_object()

random_effects <- c('nodemean','iideffect','intercept')

obj <- TMB::MakeADFun(
  data = input_data, 
  parameters = parameters,
  map = tmb_map,
  random = random_effects,
  hessian = TRUE,
  silent = TRUE, # Always do silent = TRUE
  DLL = "disaggregation")

obj$fn()
obj$gr()

## Fit the model ---- 
# Fit the model using TMB- meaning, optimize the Laplace approximate marginal Likelihood
# and then get parameter standard errors from the Hessian at the mode.
cat("Fitting using TMB/Laplace.\n")
system.time({
  opt <- aghq::optimize_theta(obj,startingvalue = rep(0,3),control = default_control(method = 'trust',negate = TRUE,numhessian = TRUE))
  sdr <- TMB::sdreport(obj) # Note this uses the most recent objective function value inside obj, i.e. the mode.
  sdsummary <- summary(sdr,select = 'fixed')
})
# startingval

## Fit the model using AGQH ----
# theta = log(tau), log(sigma), log(rho)
cat("Fitting using AGHQ.\n")
system.time(
  aghqmodel <- aghq::marginal_laplace_tmb(obj,k=8,startingvalue = rep(0,3),control = default_control_tmb())
) # k = 7, 1588 seconds, 2021/10/25


# summary(aghqmodel)
# plot(aghqmodel)
￼
taupdf <- compute_pdf_and_cdf(aghqmodel$marginals[[1]],
                              transformation = list(
                                totheta = function(x) -2*log(x),
                                fromtheta = function(x) exp(-.5*x)))
sigmapdf <- compute_pdf_and_cdf(aghqmodel$marginals[[2]],transformation = list(totheta = log,fromtheta = exp))
rhopdf <- compute_pdf_and_cdf(aghqmodel$marginals[[3]],transformation = list(totheta = log,fromtheta = exp))

ggplot(taupdf)+geom_line(mapping = aes(x = transparam,y = pdf_transparam))+theme_bw()+
  labs(x = expression(1/sqrt(tau)),y = '') +
  theme(text = element_text(size = 18))
  
ggplot(data = dplyr::filter(sigmapdf,transparam < 3))+geom_line(mapping = aes(x = transparam,y = pdf_transparam))+theme_bw()+
  labs(x = expression(sigma),y = '') +
  theme(text = element_text(size = 18))

ggplot(data = rhopdf)+geom_line(mapping = aes(x = transparam,y = pdf_transparam))+theme_bw()+
  labs(x = expression(rho),y = '') +
  theme(text = element_text(size = 18))

# Regression coefficients
samps <- sample_marginal(aghqmodel,1e03)
#samp2 <- sample_marginal(aghqmodel,1)
#sum(colnames(samp2$samps)=='iideffect')
#betasamps <- samps_forbeta$samps[1:2, ]
betasamps <- samps$samps[1, ]
mean(betasamps)
hist(betasamps)

cat("Making additive predictor rasters, AGHQ.\n")
make_eta <- function(W) {
  coords <- dis_data$coordsForPrediction
  Amatrix <- disaggregation:::getAmatrix(dis_data$mesh,coords)
  # W is a column of the matrix of samples
  # It represents a sample from the approximation to the posterior of the parameter
  # vector ordered according to TMB
  # intercept and 3 slopes, 109 polygon-level iid effects, 531 mesh-level random effects
  eta <- 0
  # Covariate effect
  eta <- eta + W[1]# + covariate_stack[[1]] * W[2] 
  # eta is now a raster
  # spatial field
  nodemean <- W[which(names(sdr$par.random)=='nodemean')]
  field <- Amatrix %*% nodemean
  field_ras <- raster::rasterFromXYZ(cbind(coords,field))
  eta <- eta + field_ras
  eta
}


system.time(ee <- base::apply(samps$samps,2,make_eta))
eb <- brick(ee)
predmean <- mean(exp(eb))
predexceedence <- mean(eb > log(.5))


## Inference for u ----

coords <- dis_data$coordsForPrediction
Amatrix <- disaggregation:::getAmatrix(dis_data$mesh,coords)

uu <- samps$samps[which(names(sdr$par.random)=='nodemean'),]
uest <- base::apply(uu,1,mean)
field=Amatrix%*%uest
uest=raster::rasterFromXYZ(cbind(coords,field))

# Get U fild and mean incidence rate: E(lamda|Y)
MGBorder = spTransform(county_geometry, projection(dis_data$polygon_shapefile))
MGBorderouter <- rgeos::gUnaryUnion(MGBorder)
U.rast <- raster::mask(uest,MGBorder)
EL.rast <- raster::mask(predmean,MGBorder)

### agregate by county: mean 
pr <- raster::extract(U.rast, all_states)
mean_values <- lapply(pr, mean, na.rm = TRUE)
all_states$mean_value <- unlist(mean_values)
spplot(all_states,zcol='mean_value')

#################################################################
########################### plots ###############################
#################################################################


# get states geometry
us <- gadm(country = "USA", level = 1, resolution = 2,
             path = "./")
us_outline <- as(st_geometry(sf::st_as_sf(us)), Class="Spatial")

# R basic plot
#plot(U.rast)
#plot(us,add=TRUE)
#plot(MGBorder, add=TRUE,border=mapmisc::col2html("black", 0.5), lwd=0.5)
#plot(MGBorderouter,add = TRUE)

# LatticeExtra plot
colr <- colorRampPalette(brewer.pal(11, 'YlOrRd'))
levelplot(U.rast, 
          margin=FALSE,                       # suppress marginal graphics
          colorkey=list(
            space='left',                   # plot legend at bottom
            labels=list(at=c(-1,-0.5,0,0.5,1.1), font=4)      # legend ticks and labels 
          ),    
          par.settings=list(
            axis.line=list(col='transparent') # suppress axes and legend outline
          ),
          scales=list(draw=FALSE),            # suppress axis labels
          col.regions=colr,                   # colour ramp
          at=seq(-1,1.1, len=101))  +           # colour ramp breaks
  latticeExtra::layer(sp.polygons(us_outline, col = "grey40"))



# R basic plot
#plot(EL.rast)
#range(values(EL.rast),na.rm=TRUE)
#plotraster <- terra::mask(population_raster,r,all_states)
#plot(MGBorder, add=TRUE,border=mapmisc::col2html("black", 0.5), lwd=0.5)
#plot(MGBorderouter,add = TRUE)

# LatticeExtra plot
colr <- colorRampPalette(brewer.pal(11, 'YlOrRd'))
levelplot(EL.rast, 
          margin=FALSE,                       # suppress marginal graphics
          colorkey=list(
            space='left',                   # plot legend at bottom
            labels=list(at=c(0.35,1,1.5,2,2.25), font=4)      # legend ticks and labels 
          ),    
          par.settings=list(
            axis.line=list(col='transparent') # suppress axes and legend outline
          ),
          scales=list(draw=FALSE),            # suppress axis labels
          col.regions=colr,                   # colour ramp
          at=seq(0.35,2.25, len=101))  +           # colour ramp breaks
  latticeExtra::layer(sp.polygons(us_outline, col = "grey40"))



# save rdata
#name=paste('lastRun_',Race,year,education,'.RData',sep='')
#save.image(name)

