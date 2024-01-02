## Authors: Leonildo 
## Date: 2020-05-20
## Ph.D. Leonildo

library(rcompanion)
library(bestNormalize)
library(data.table)
library(gstat)
library(sf)
library(sp)
library(geoR)
library(automap)
library(rgdal)
library(raster)

# ###########################################################
# Applying the ordinary Krigign 
# ###########################################################

ordinary.kriging <- function(model_normalized, attr, TheData, map, model_grid) {
  #simple variogram with more adequated distances (package geostats)
  #variograma <- variogram(attr~1, data=TheData)
  #cutoff: variogram distance (graph tam)
  #variograma <- variogram(TheData$attr~1, data=TheData, cutoff=200)
  #plot.numbers: how many peer point is at each point
  #plot(variograma, plot.numbers=TRUE)
  
  #Creating variogram with geoR (more functions and adjustments)
  attr_geodata <- as.geodata(TheData[,"attr"])
  variogram_geor <- variog(attr_geodata)
  #plot(variogram_geor)
  
  #Make the variogram adjustment
  variofit_geor <- variofit(variogram_geor)
  #print(variofit_geor) #to see the parameters (manual exploration)
  plot(variogram_geor)
  lines(variofit_geor)
  
  #optimizing the variogram parameters
  variogram_auto <- autofitVariogram(TheData$attr~1, TheData,
                                     start_vals=c(variofit_geor$nugget, variofit_geor$cov.pars[2],
                                                  variofit_geor$cov.pars[1])) #initial parameters to the asjustments
  View(variogram_auto$var_model) #to see the model
  plot(variogram_auto)
  
  #print(variofit_geor)
  
  #making the kriging (with the normalized scale)
  #kriging_auto = autoKrige(attr~1, TheData,
  #                         new_data = model_grid, start_vals = c(variofit_geor$nugget,
  #                                                               variofit_geor$cov.pars[2], variofit_geor$cov.pars[1]))
  
  #plot(kriging_auto)#plot all kigring informations
  #plot(kriging_auto$krige_output["var1.pred"]) #plot only the predictive model
  #plot(kriging_auto$krige_output["var1.stdev"]) #plot only the predictive error
  
  #Recovering the original scale
  #kriging_auto$krige_output$original <- predict(model_normalized,
  #                                               newdata=kriging_auto$krige_output$var1.pred, inverse=TRUE)
  #plot(kriging_auto$krige_output["original"]) #plot the kriging predictive with the origanal scale
  
  #Exporting the kriging to raster format (with the map)
  #kriging_raster <- raster(kriging_auto$krige_output["original"])
  #par(mfrow=c(1,1))
  #plot(kriging_raster)
  #plot(map, add=TRUE)
  
  #Vizualizing the perspective
  #persp(kriging_raster, border=NA, col="blue", shade=1, theta = 320, phi=40)
  
  #Crossing validation
  #kriging_cv <- autoKrige.cv(attr~1, TheData, start_vals=c(variofit_geor$nugget, variofit_geor$cov.pars[2], variofit_geor$cov.pars[1]))
  #print(kriging_cv)
  #Root of the mean square error in normalized scale
  #print(sqrt(mean(kriging_cv$krige.cv_output$residual^2)))
  # % explanation of variance (dimensionless)
  #print((var(kriging_cv$krige.cv_output$residual)/var(kriging_cv$krige.cv_output$observed)))
}

# ###########################################################
# Applying the indicator Krigign 
# ###########################################################

indicator.kriging <- function(model_normalized, TheData, map, model_grid) {
  
  attr <- model_normalized$x.t
  #the data is normalized, so it's necessary to know the respective value
  attr_limit <- predict(model_normalized, newdata = 20) # to know the respective AQI=20 value
  #print(attr_20)
  
  #variogram adjustment
  fit_indicator <- autofitVariogram(formula=I(attr<attr_limit)~1, TheData)
  #plot(fit_indicator)
  #print(fit_indicator$var_model) #to see the parameters
  
  #make the indicator kriging (nmax: max neighbors number)
  kriging_indicator <- krige(formula=I(attr<attr_limit)~1, locations=TheData, 
                             model=fit_indicator$var_model, nmax=12, newdata=model_grid)
  plot(kriging_indicator["var1.pred"])
  plot(map, add=TRUE)
  
}

# ###########################################################
# Prepare the data to kriging
# ###########################################################

path = "/data/doutorado-leonildo/data/O3-2019/"  # (for example)
setwd(path)
#file <- "./final-file.csv"
file <- "./pollution_us_2019.csv"
#read the file to manipulate the data
#data = fread(file, sep = ",", dec = ".")[, c('w','DateLocalCount', 'longitude', 'latitude',
#                                             'NO2AQI','O3AQI','SO2AQI','COAQI')]

data = fread(file, sep = ",", dec = ".")[, c('longitude', 'latitude','AQI')]
#read the same file in other format to converto from table to SpatialPointsDataFrame
data2 <- st_read(file)

#The variable objective
attr <- data$AQI
#save the coordinates
x <- data$longitude
y <- data$latitude
#plotNormalHistogram(attr)
#No parametric normalization
model_normalized <- bestNormalize(attr)
#print(model_normalized) ##to see the normalizations characteristics

# remove any null data rows
TheData=na.omit(data2)

# convert simple data frame into a spatial data frame object
coordinates(TheData)=  cbind(x , y)#~ x+y

#Criating an atribute to the normalization model in the data
TheData$attr <- model_normalized$x.t
attr <- model_normalized$x.t
#plotNormalHistogram(attr)

#Read the shapefile map
#map <- readOGR(dsn = "./US/city/US.shp", verbose = FALSE, encoding = 'iso-8859-1', stringsAsFactors = FALSE)
map <- readOGR(dsn = "~/doc-leonildo/shapes/USA/USA-border/USA_adm0.shp", verbose = FALSE, encoding = 'iso-8859-1', stringsAsFactors = FALSE)
crs(map) <- CRS("+proj=utm +zone=10+datum=WGS84")

#prepar the raster map
model_grid <- readGDAL("model_raster.tif")
#apply the same proj4string code from the raster model to the data in sp format
crs(TheData) <- CRS("+proj=utm +zone=10+datum=WGS84")
crs(model_grid) <- CRS("+proj=utm +zone=10+datum=WGS84")

#call ordinary kriging
ordinary.kriging(model_normalized, attr, TheData, map, model_grid)

#call indicator kriging
#indicator.kriging(model_normalized, TheData, map, model_grid)


