## Authors: Leonildo (adapted from Sidgley)
## Date: 2020-01-29
## Ph.D. Leonildo


# define path of the current script
path = "/home/leonildo/Dropbox/ICMC-USP/Doutorado/codigos/Granularity/data/US-2019"  # (for example)
setwd(path)

# read raster data
#grid <- rgdal::readOGR("/home/leonildo/Dropbox/ICMC-USP/Doutorado/codigos/Granularity/data/100000km/hex-grid.shp")
#mudando sistema de coordenas
#grid <- spTransform(grid, CRS("+proj=longlat +datum=WGS84"))

# ###########################################################
# rescale technique [0,1] 
# ###########################################################

normalize <- function(x) {
  # between [0,1], 
  # other intervals require change alpha and beta
  alpha = 0
  beta = 1
  
  if (sum(x**2, na.rm = TRUE) > 0) {
    x.new = ((beta - alpha) * ((x - min(x, na.rm = TRUE)) / 
                                 (max(x, na.rm = TRUE) - min(x, na.rm=TRUE)))) + alpha
  }
  return(x.new)
}

# ###########################################################
# Calculate the global autocorrelation for a list of
# neighbours weights matrix (Morans'I)
# ###########################################################

#dados e mariz de pesos
global.autocorrelation <- function(data, grid, nbwl, normalized) {
  res = data.frame()
  #attrs = colnames(data[, c(2,3,4,5,6)])
  attrs = colnames(data[, c(2)])
  cat("Iniciando autocorrelacao global\n\n")
  # get statistic for different attributes/measures 
  # (i.e., odds ratio, relative and absolute frequencies)
  for (attr in attrs) {
    grid.clone = grid
    grid.clone@data = left_join(x = grid@data, 
                                y = data[, c('w', attr)], 
                                by = c("ID"="w"))
      
    grid.clone@data[is.na(grid.clone@data[[attr]]), attr] <- 0
    
    res.statistics = lapply(nbwl, function(listw) { 
      
      m = moran.mc(x = grid.clone@data[[attr]], listw = listw, alternative = "greater",
                   nsim = 999, na.action = na.omit, zero.policy = FALSE)
      df = data.frame(m$statistic, m$p.value, row.names = NULL)
      
      for (ix.col in seq(1, ncol(df))) {
        colnames(df)[ix.col] = paste0(colnames(df)[ix.col], ".", listw$style)
      }
      
      return(df)
    })
    
    res.ti = data.frame(attr)
    for (ix2 in seq(1, length(res.statistics))) {
      
      if (is.na(res.statistics[[ix2]][, 1])) {
        res.statistics[[ix2]][, 2] = NaN
      }
      res.ti = cbind(res.ti, res.statistics[[ix2]])
    }
    
    res = rbind(res, res.ti)
    
  }
  #print(res)
  return(res)
}

# ###########################################################
# Calculate the local autocorrelation for a list of
# neighbours weights matrix (Morans'I)
# ###########################################################

local.autocorrelation <- function(data, grid, nbwl, normalized) {
  res = data.frame()
  #attrs = colnames(data[, 2:ncol(data)])
  attrs = colnames(data[, c(2)])
  
  cat("Iniciando autocorrelacao local\n\n")
  
  # get statistic for different attributes/measures 
  # (i.e., odds ratio, relative and absolute frequencies)
  for (attr in attrs) {
    grid.clone = grid
    
    grid.clone@data = left_join(x = grid@data, 
                                y = data[, c('w', attr)], 
                                by = c("ID"="w"))
    
    grid.clone@data[is.na(grid.clone@data[[attr]]), attr] <- 0
    
    res.statistics = lapply(nbwl, function(listw) {
      m = localmoran(x = grid.clone@data[[attr]], listw = listw, alternative = "greater", 
                     na.action = na.omit, zero.policy = TRUE)
      m = data.frame(m[, c(1,3,4,5)])
      colnames(m) = c('m.statistic', 'm.var', 'm.z', 'm.p.value' )              
      
      df = data.frame(m, row.names = NULL)
      
      for (ix.col in seq(1, ncol(df))) {
        colnames(df)[ix.col] = paste0(colnames(df)[ix.col], ".", listw$style)
      }
      
      return(df)
    })
    
    res.ti = data.frame(rep(nrow(grid)), attr, grid@data[["ID"]])
    #colnames(res.ti) = c("attr","w")
    colnames(res.ti) = c("w","attr","w")
    
    for (ix in seq(1, length(res.statistics))) {
      res.ti = cbind(res.ti, res.statistics[[ix]])
    }
    
    res = rbind(res, res.ti)
    
  }
  
  return(res)
}

# ###########################################################
# Generate a neighbours list with different spatial weights 
# ###########################################################

neighborhood.weights <- function(nb, coords) {
  
  # use inverse distance with the great circle metric
  dists = nbdists(nb = nb, coords = coords, longlat = TRUE)
  dists.inv = lapply(dists, function(x) (1/x))
  
  ret = list()
  
  ret$nbw.S = nb2listw(neighbours = nb, style = "S", zero.policy = FALSE)
  nbw = nb2listw(neighbours = nb, glist = dists.inv, style = "S", zero.policy = FALSE)
  nbw$style = paste0(nbw$style, "_InvDist")
  ret$nbw.dist.S = nbw
  
  ret$nbw.B = nb2listw(neighbours = nb, style = "B", zero.policy = FALSE)
  nbw = nb2listw(neighbours = nb, glist = dists.inv, style = "B", zero.policy = FALSE)
  nbw$style = paste0(nbw$style, "_InvDist")
  ret$nbw.dist.B = nbw
  
  ret$nbw.W = nb2listw(neighbours = nb, style = "W", zero.policy = FALSE)
  nbw = nb2listw(neighbours = nb, glist = dists.inv, style = "W", zero.policy = FALSE)
  nbw$style = paste0(nbw$style, "_InvDist")
  ret$nbw.dist.W = nbw
  
  ret$nbw.C = nb2listw(neighbours = nb, style = "C", zero.policy = FALSE)
  nbw = nb2listw(neighbours = nb, glist = dists.inv, style = "C", zero.policy = FALSE)
  nbw$style = paste0(nbw$style, "_InvDist")
  ret$nbw.dist.C = nbw
  
  return(ret)
}

# ###########################################################
# Run autocorrelation for each time unit
# ###########################################################

run.autocorrelation <- function(data, grid, folder, output.files) {
  
  # contiguity neighbors
  nb <- poly2nb(pl = grid, row.names = grid@data[['ID']], queen = FALSE)
  
  # calculate inverse distance with the great circle metric
  centroids = gCentroid(grid, byid = TRUE, id = grid@data[['ID']])
  dists = nbdists(nb = nb, coords = centroids@coords, longlat = TRUE)
  dists.inv = lapply(dists, function(x) {1/x})
  
  # spatial weight matrix
  nbwl = neighborhood.weights(nb, centroids@coords)
  
  # calculate and store the global autocorrelation
  res = global.autocorrelation(data = data, grid = grid, nbwl = nbwl, normalized = FALSE)
  write.table(x = res, file = paste0(folder, "/", output.files[1]), 
              quote = TRUE, sep = ",", dec = ".", eol = "\n", row.names = TRUE, fileEncoding = "UTF-8")
  
  # calculate and sotore the local autocorrelation
  res = local.autocorrelation(data = data, grid = grid, nbwl = nbwl, normalized = FALSE)
  write.table(x = res, file=paste0(folder, "/", output.files[2]), 
              quote = TRUE, sep = ",", dec = ".", eol = "\n", row.names = FALSE, fileEncoding = "UTF-8")
}

# ###########################################################
# Run autocorrelation for time period
# ###########################################################

run.autocorrelation.agg <- function(data, grid, folder, output.files) {
  
  # contiguity neighbors
  nb <- poly2nb(pl = grid, row.names = grid@data[['ID']], queen=FALSE)
  #nb <- poly2nb(grid)
  
  # calcule inverse distance with the great circle metric
  centroids = gCentroid(grid, byid = TRUE, id = grid@data[['ID']])
  
  #----- begin plot neighbors
  #plot(grid, col='gray', border='blue')
  #xy <- coordinates(grid)
  #plot(nb, xy, col='red', lwd=2, add=TRUE)
  #stop("just print")
  #----- end plot neighbors
  
  dists = nbdists(nb = nb, coords = centroids@coords, longlat = TRUE)
  dists.inv = lapply(dists, function(x) {1/x})
  
  # spatial weight matrix
  nbwl = neighborhood.weights(nb, centroids@coords)

  # calculate and store the global autocorrelation
  res = global.autocorrelation(data = data, grid = grid, nbwl = nbwl, normalized = FALSE)
  write.table(x = res, file = paste0(folder, "/", output.files[1]), 
              quote = TRUE, sep = ",", dec = ".", eol = "\n", row.names = FALSE, fileEncoding = "UTF-8")
  
  # calculate and store the local autocorrelation
  res = local.autocorrelation(data = data, grid = grid, nbwl = nbwl, normalized = FALSE)
  write.table(x = res[,-1], file = paste0(folder, "/", output.files[2]), 
              quote = TRUE, sep = ",", dec = ".", eol = "\n", row.names = FALSE, fileEncoding = "UTF-8")
}

# ###########################################################
# Run for each folder with shapes and data files
# ###########################################################

run <- function(areal.unit.folder) {
  cat("Processing ", areal.unit.folder, "\n")
  
  # filter the period

  file = lapply(areal.unit.folder, paste0, paste0("/",input.file))[[1]]
  #data = fread(file, sep = ",", dec = ".")[, c('w','DateLocalCount','NO2AQI','O3AQI','SO2AQI','COAQI')]
  data = fread(file, sep = ",", dec = ".")[, c('w','AQI')]
  data = data.frame(data)
  
  file = paste0(areal.unit.folder, "/", "hex-grid.shp")
  # read raster data
  grid = readOGR(dsn = file, verbose = FALSE, encoding = 'iso-8859-1', stringsAsFactors = FALSE)
  #removing polygons without neighborhood
  #grid = grid[!is.nan(grid@data$var), ]
  #mudando sistema de coordenas
  #grid <- spTransform(grid, CRS("+proj=longlat +datum=WGS84"))
  
  run.autocorrelation.agg(data, grid, areal.unit.folder, 
                          gsub("\\?", "US_pollution", output.files))
  

  select.attribute = unique(data$AQI)
  attribute.units =  unique(select.attribute)
  # bootsrapt resampling with 1000 replications (all attributes)
  for(p in seq(1, 1000)) {
   data2 = data[data$AQI %in% attribute.units[sample(length(attribute.units), replace = TRUE)], ]
   run.autocorrelation.agg(data2, grid, areal.unit.folder, 
                           gsub("\\?", paste0("US_pollution_", p) , output.files))
  }
  
  rm(data)
  rm(grid)
  
  cat("Finished ", areal.unit.folder, "\n")
}

# ###########################################################
# get folders of the areal units
# ###########################################################

prepare.folders <- function(path) {
  
  # get folders
  areal.unit.folders = list.dirs(path = ".", full.names = TRUE, recursive = TRUE)
  # include folders
  areal.unit.folders = areal.unit.folders[lapply(areal.unit.folders, 
                                                 function(x) length(grep("km", x, value=FALSE))) != 0]
  
  for(areal.unit.folder in areal.unit.folders) {
    if(!file.exists(paste0(areal.unit.folder, "/", input.file)))
      areal.unit.folders <- setdiff(areal.unit.folders, areal.unit.folder)
  }
  
  if(!rewrite) {
    for(areal.unit.folder in areal.unit.folders) {
     if(file.exists(paste0(areal.unit.folder, "/", "local_autocorrelation_heavy_rain.csv")))
       areal.unit.folders <- setdiff(areal.unit.folders, areal.unit.folder)
    }
  }
  
  # set number of cores
  #n.cores = detectCores(all.tests = FALSE, logical = TRUE)
  #res = mclapply(areal.unit.folders, run, mc.cores = n.cores)
  mclapply(areal.unit.folders, run, mc.cores = 1)
}

# ###########################################################
# main
# ###########################################################

if (!require("parallel")) install.packages("parallel")
if (!require("sp")) install.packages("sp")
if (!require("rgdal")) install.packages("rgdal")
if (!require("maptools")) install.packages("maptools")
gpclibPermit()
gpclibPermitStatus()
if (!require("dplyr")) install.packages("dplyr")
if (!require("rgeos")) install.packages("rgeos")
if (!require("reshape2")) install.packages("reshape2")
if (!require("labeling")) install.packages("labeling")
if (!require("spdep")) install.packages("spdep")
if (!require("data.table")) install.packages("data.table")
if (!require("bit")) install.packages("bit")

rewrite = TRUE
input.file = c("final-file.csv")
output.files = c("global_autocorrelation_?.csv", "local_autocorrelation_?.csv")

prepare.folders(path)