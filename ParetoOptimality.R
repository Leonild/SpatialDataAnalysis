## Authors: Leonildo (adapted from Sidgley)
## Date: 2020-03-17
## Modified: 2020-03-20
## Ph.D. Leonildo

# define path of the current script
path = "/data/doutorado-leonildo/data/seasons/O3-2019/fall/" # (for example)
setwd(path)

# ###############################################################
# read and summarize global and local autocorrelation statistics
# ###############################################################

read.autocorr <- function(file) {
  cat("Reading file ", file, "\n")
  
  df.global = read.csv2(file = file, sep = ",", dec = ".", 
                        stringsAsFactors = FALSE)[c('attr','m.statistic.W','m.p.value.W')]
  df.local = read.csv2(file = gsub("\\global", "local", file), sep = ",", dec = ".", 
                        stringsAsFactors = FALSE)[c('attr','w','m.statistic.W','m.p.value.W')]
  
  df.res = data.frame()
  
  # for each attribute signal (i.e., abs, rel, freq, odds ratio)
  for(attr in unique(df.global$attr)) {
    
    # global statistics
    split.folder = (strsplit(file, "/"))[[1]]
    
    areal.unit = split.folder[-1][1]
    source = split.folder[-1][2]
    
    # local statistics
    mean = round(mean(df.local[df.local$attr == attr, 3], na.rm = TRUE), 4)
    median = round(median(df.local[df.local$attr == attr, 3], na.rm = TRUE), 4)
    sd = round(sd(df.local[df.local$attr == attr, 3], na.rm = TRUE), 4)
    cvar = round(sd/abs(mean), 4)
    
    df.tmp = cbind(source, areal.unit, df.global[df.global$attr == attr, ], mean, median, sd, cvar)
    df.res = rbind(df.res, df.tmp)
  }
  
  if(ncol(df.res) > 0) {
    colnames(df.res) = c("source", "areal.unit", "attr",
                       "global.moran.i","global.pvalue.moran.i", 
                       "local.moran.i.mean", "local.moran.i.median", "local.moran.i.sd","local.moran.i.cvar")
  }
  return(df.res)
}

# ###############################################################
# read bootstrap replications for each areal unit (folders)
# ###############################################################

read.data <- function(areal.unit.folder) {
  cat("Reading folder", areal.unit.folder, "\n")
  files = list.files(path = areal.unit.folder, 
                     pattern = "^(global)_autocorrelation(.*)", full.names = TRUE)
  n.cores = 2
  df = mclapply(files, read.autocorr, mc.cores = n.cores)
  df = do.call("rbind", df)
  
  return(df)
}

# ###############################################################
# run pareto optimality algorithm
# ###############################################################

pareto.optimality <- function(df) {
  cat("Processing pareto optmality", "\n")
  df.list = split(df, f = df$source)
  
  # run pareto front algorithm for each subset of data
  df.pareto = lapply(df.list, function(df) {
    
    # pareto optimality settings
    options(rPref.parallel = TRUE)
    options(rPref.parallel.threads = 1)
    
    # filter attr equal "n" that represent all values
    #df = df[df$attr == "or", ]
    #df = df[df$attr == "NO2AQI", ]
    df = df[df$attr == "AQI", ]

    # pareto front for all
    p = data.frame()
    df.filter = df
    p.moran = high(global.moran.i)
    p.cvar = low(local.moran.i.cvar)
    p.cvar.moran <- low(local.moran.i.cvar) * high(global.moran.i)
    p.list = c(p.moran, p.cvar, p.cvar.moran)
    p.list.res = lapply(p.list, function(p) {
      df.p = psel(df.filter, p, top = nrow(df.filter))
      colnames(df.p)[ncol(df.p)] = as.character(p)
      return(df.p)
    })
    
    p = rbind(p, join_all(p.list.res, by = colnames(p.list.res[[1]])[1:9], type = 'full'))
    return(list(p))
  })
  
  # export pareto optimality
  res = lapply(df.pareto, function(df) {
    if(nrow(df[[1]]) > 1) {
      df = df[[1]]
      file = gsub("\\global_autocorrelation", "pareto_front", unique(df$source))
      write.table(x = df[,-1], file = paste0(output.folder, file), quote = TRUE, sep = ",", 
                  dec = ".", eol = "\n", row.names = FALSE, fileEncoding = "UTF-8")
    }
  })
  
}

# ###########################################################
# get folders of the areal units
# ###########################################################

run <- function() {
  
  # get folders
  areal.unit.folders = list.dirs(path = ".", full.names = TRUE, recursive = TRUE)
  # include folders
  areal.unit.folders = areal.unit.folders[lapply(areal.unit.folders, 
                                                 function(x) length(grep("km", x, value=FALSE))) != 0]
  # remove folders without data files
  for(areal.unit.folder in areal.unit.folders) {
    if(!file.exists(paste0(areal.unit.folder, "/global_autocorrelation_US_pollution.csv"))) {
      cat("Removing folder ", areal.unit.folder, "\n")
      areal.unit.folders <- setdiff(areal.unit.folders, areal.unit.folder)
    }
  }
  cat("Reading folder", areal.unit.folder, "\n")
  files = list.files(path = areal.unit.folder, 
                     pattern = "^(global)_autocorrelation(.*)", full.names = TRUE)
  
  #for(file in unique(files)) {
  #  cat("Reading file ", file, "\n")
  #
  #  split.folder = (strsplit(file, "/"))[[1]]
  #  areal.unit = split.folder[-1][1]
  #  cat("Areal unit", areal.unit, "\n")
  #  source = split.folder[-1][2]
  #  cat("Source ", source, "\n")
  #  cat("Pareto ", gsub("\\global_autocorrelation", "pareto_front", unique(source)), "\n")
  #}
  
  n.cores = detectCores(all.tests = FALSE, logical = TRUE)
  df.list = mclapply(areal.unit.folders, read.data, mc.cores = n.cores)
  df = do.call("rbind", df.list)
  #print(df)
  pareto.optimality(df)
}

# ###########################################################
# main
# ###########################################################

if (!require("parallel")) install.packages("parallel")
if (!require("rPref")) install.packages("rPref")
if (!require("plyr")) install.packages("plyr")

# global variables 
output.file = c("pareto_front_?.csv")
output.folder = "/data/doutorado-leonildo/data/seasons/O3-2019/fall/pareto_optimality/"

run()
