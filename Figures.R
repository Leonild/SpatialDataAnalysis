## Authors: Leonildo (adapted from Sidgley)
## Date: 2020-03-20
## Modified: 2020-01-01
## Ph.D. Leonildo

# define path of the current script
path = "/data/doutorado-leonildo/data/US-2019/"  # (for example)
#path = "/data/doutorado-leonildo/data/seasons/O3-2019/spring/"  # (for example)
setwd(path)

attrName = 'AQI'


# ########################################################
# Common functions
# ########################################################

# function 1
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none", plot.margin = unit(c(0,0,0,0), "pt")))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  invisible(combined)
}

# function 2
lappend <- function (lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}

# ########################################################
# Figure 6  
# Trade-off between the global indicator of spatial association (Global Moran’s I ) and the overall
# degree of structural (in)stability (coefficient of variation of Local Moran’s I normalized by scaling between the
# minimum and maximum values of the Global Moran’s I coefficients. Both global and local spatial statistics
# were computed for a row-standardized spatial weights matrix based on first-order rook contiguity.
# export: width=900 height=340
# ########################################################

 if (!require("spdep")) install.packages("spdep")
 if (!require("dplyr")) install.packages("dplyr")
 if (!require("data.table")) install.packages("data.table")
 if (!require("ggplot2")) install.packages("ggplot2")

 files = list()
 files = lappend(files, c("./10-0km/global_autocorrelation_US_pollution.csv","./10-0km/local_autocorrelation_US_pollution.csv"))
 files = lappend(files, c("./10-1km/global_autocorrelation_US_pollution.csv","./10-1km/local_autocorrelation_US_pollution.csv"))
 files = lappend(files, c("./10-2km/global_autocorrelation_US_pollution.csv","./10-2km/local_autocorrelation_US_pollution.csv"))
 files = lappend(files, c("./10-3km/global_autocorrelation_US_pollution.csv","./10-3km/local_autocorrelation_US_pollution.csv"))
 #files = lappend(files, c("./400km/global_autocorrelation_US_pollution.csv","./400km/local_autocorrelation_US_pollution.csv"))
 #files = lappend(files, c("./500km/global_autocorrelation_US_pollution.csv","./500km/local_autocorrelation_US_pollution.csv"))
 #files = lappend(files, c("./100km/global_autocorrelation_US_pollution.csv","./100km/local_autocorrelation_US_pollution.csv"))
 # files = lappend(files, c("/home/leonildo/California/60km/global_autocorrelation_US_pollution.csv","/home/leonildo/California/60km/local_autocorrelation_US_pollution.csv"))
 # files = lappend(files, c("/home/leonildo/California/50km/global_autocorrelation_US_pollution.csv","/home/leonildo/California/50km/local_autocorrelation_US_pollution.csv"))
 # files = lappend(files, c("/home/leonildo/California/40km/global_autocorrelation_US_pollution.csv","/home/leonildo/California/40km/local_autocorrelation_US_pollution.csv"))
 # files = lappend(files, c("/home/leonildo/California/30km/global_autocorrelation_US_pollution.csv","/home/leonildo/California/30km/local_autocorrelation_US_pollution.csv"))
 # files = lappend(files, c("/home/leonildo/California/20km/global_autocorrelation_US_pollution.csv","/home/leonildo/California/20km/local_autocorrelation_US_pollution.csv"))
 # files = lappend(files, c("/home/leonildo/California/10km/global_autocorrelation_US_pollution.csv","/home/leonildo/California/10km/local_autocorrelation_US_pollution.csv"))
 # files = lappend(files, c("/home/leonildo/California/5km/global_autocorrelation_US_pollution.csv","/home/leonildo/California/5km/local_autocorrelation_US_pollution.csv"))

 global = data.frame()
 local = data.frame()

 for(i in 1:length(files)) {
   file = files[[i]]
   #df.global = as.data.frame(fread(file[1], sep = ",", dec = ".")[4 , c('attr', 'm.statistic.W_InvDist', 'm.p.value.W_InvDist')])
   df.global = as.data.frame(fread(file[1], sep = ",", dec = ".")[ , c('attr', 'm.statistic.W', 'm.p.value.W')])
   df.global = df.global[df.global['attr'] == attrName,]
   colnames(df.global) = c('attr', 'moran','p.value')
   #df.local = as.data.frame(fread(file[2], sep = ",", dec = ".")[ , c('attr', 'm.statistic.W_InvDist', 'm.p.value.W_InvDist')])
   df.local = as.data.frame(fread(file[2], sep = ",", dec = ".")[ , c('attr', 'm.statistic.W', 'm.p.value.W')])
   df.local = df.local[df.local['attr'] == attrName,]
   colnames(df.local) = c('attr', 'moran','p.value')
   print(df.global)

   mean = round(mean(df.local[,2], na.rm = TRUE), 4)
   median = round(median(df.local[,2], na.rm = TRUE), 4)
   sd = round(sd(df.local[,2], na.rm = TRUE), 4)
   cvar = round(sd/abs(mean), 4)
   df.local = as.data.frame(cbind(unique(df.local$attr), mean, median, sd, cvar))
   colnames(df.local) = c('attr','mean','median','sd','cvar')

   split.folder = (strsplit(file[1], "/"))[[1]]
   scale = split.folder[-1][1]
   zone = 'hexagonal'

   split.folder = (strsplit(file[1], "_"))[[1]]
   global = rbind(global, cbind(zone, scale, df.global))
   local = rbind(local, cbind(zone, scale, df.local))
 }

 colnames(global) = c('zone','scale','attr', 'moran','p.value')
 colnames(local) = c('zone','scale','attr', 'mean','median','sd','cvar')

 global$scale = as.character(global$scale)
 local$scale = as.character(local$scale)
 global$zone = as.character(global$zone)
 local$zone = as.character(local$zone)
 global$attr = as.character(global$attr)
 local$attr = as.character(local$attr)
 local$mean = as.numeric(as.character(local$mean))
 local$median = as.numeric(as.character(local$median))
 local$sd = as.numeric(as.character(local$sd))
 local$cvar = as.numeric(as.character(local$cvar))

 df = full_join(local, global, by = c("zone"="zone","scale"="scale","attr"="attr"))

 levels = c("10-0 km","10-1 km","10-2 km","10-3 km")

 df['scale2'] = paste0(strsplit(df$scale, "km"), " km")
 print(df$scale2)
 df['scale2'] = factor(df$scale2, levels = levels)
 print(df$scale2)
 

 pd = position_dodge(width = 0.9)
 et = element_text(family = "Times New Roman", size = 14, color = "black", angle=0)

 print(df$moran)
 df['cvarnorm'] = 0
 alpha = min(df$moran)
 beta =  max(df$moran)
 df$cvarnorm = ((beta - alpha) * ((df$cvar - min(df$cvar)) / (max(df$cvar) - min(df$cvar)))) + alpha

 global.morans.i.text = expression(paste("Global Moran's ", italic("I")))
 local.morans.i.text = expression(paste("Coefficient of variation of Local Moran's ", italic("I"), " (normalized)"))
 local.morans.i.text2 = expression(paste("Coefficient of variation \n of Local Moran's ", italic("I"), " (normalized)"))

 g = ggplot(data = df, mapping = aes(x = scale2, group = 1))
 g = g + geom_line(mapping = aes(y = moran,
                                 colour = "Global",
                                 linetype = "Global"), 
                   position = position_dodge(width = 0))
 g = g + geom_point(mapping = aes(y = moran,
                                  colour = "Global", 
                                  shape = "Global"), 
                    size = 2.5, 
                    position = position_dodge(width = 0))
 g = g + geom_line(mapping = aes(y = cvarnorm,
                                 colour = "Local",
                                 linetype = "Local"), 
                   position = position_dodge(width = 0))
 g = g + geom_point(mapping = aes(y = cvarnorm, 
                                  colour = "Local", 
                                  shape = "Local"), 
                    size = 2.5, 
                    position = position_dodge(width = 0))
 g = g + scale_x_discrete(labels = levels)
 g = g + scale_y_continuous(breaks = seq(-.50, 1.0, .05), 
                            sec.axis = sec_axis(~ ., breaks = seq(-.50, 1.0, .05), name = local.morans.i.text2))
 g = g + guides(linetype = guide_legend(""), colour = guide_legend(""), shape = guide_legend(""))
 g = g + scale_colour_manual(labels = c(global.morans.i.text, local.morans.i.text) , values = c("Global" = "black", "Local" = "black"))
 g = g + scale_linetype_manual(labels = c(global.morans.i.text, local.morans.i.text), values = c("Global" = 1, "Local" = 4))
 g = g + scale_shape_manual(labels = c(global.morans.i.text, local.morans.i.text), values = c("Global" = 19, "Local" = 17))
 g = g + labs(title = "", x = "", y = global.morans.i.text, y.sec = local.morans.i.text)
 g = g + theme_classic()
 g = g + theme(axis.text = element_text(family = "Times New Roman", size = 14, color = "black", angle=0),
               axis.title = element_text(family = "Times New Roman", size = 14, color = "black", angle=0),
               axis.title.y = element_text(margin = margin(0,8,0,2)),
               legend.text = element_text(family = "Times New Roman", size = 14, color = "black", angle=0),
               legend.position = "bottom",
               legend.box.margin = unit(c(-20,0,0,0), "pt"),
               legend.key.width = unit(32, "pt"),
               legend.justification="left",
               plot.margin = unit(c(-10,18,0,2), "pt")
 )

 #show(g)


# ########################################################
# Figure 7 and Table 1
# Pareto frontiers and trade-off between Global Moran’s I and the coefficient of variation of Local
# Moran’s I (overall degree of structural (in)stability). Both statistics were computed for a row-standardized
# spatial weights matrix based on first-order rook contiguity.
# export: width=930 height=440
# ########################################################

 if (!require("dplyr")) install.packages("dplyr")
 if (!require("data.table")) install.packages("data.table")
 if (!require("ggplot2")) install.packages("ggplot2")

 file = "./pareto_optimality/pareto_front_US_pollution.csv"
 df = as.data.frame(fread(file, sep = ",", dec = "."))

 df['.level'] = paste0("#",df$`low(local.moran.i.cvar) * high(global.moran.i)`)
 #df['label'] = paste0(strsplit(df$areal.unit,"km"), " km²")
 df['label'] = (df$areal.unit)

 global.morans.i.text = expression(paste("Global Moran's ", italic("I")))
 local.morans.i.text = expression(paste("Coefficient of variation of the Local Moran's ", italic("I"), " (spatial variance)"))

 g = ggplot(df, aes(x = local.moran.i.cvar, y = global.moran.i, shape = factor(.level), linetype = factor(.level))) 
 g = g + geom_point(size = 3.5)
 g = g + geom_step(direction = 'vh')
 g = g + geom_text(data = df[df$areal.unit == "100km",], aes(label = label), family = "Times New Roman", size = 5.4, hjust = -0.2, vjust = 0.7, show_guide = F)
 g = g + geom_text(data = df[df$areal.unit == "200km",], aes(label = label), family = "Times New Roman", size = 5.4, hjust = +1.2, vjust = 0.5, show_guide = F)
 g = g + geom_text(data = df[df$areal.unit == "300km",], aes(label = label), family = "Times New Roman", size = 5.4, hjust = -0.2, vjust = 0.7, show_guide = F)
 g = g + geom_text(data = df[df$areal.unit == "400km",], aes(label = label), family = "Times New Roman", size = 5.4, hjust = -0.2, vjust = 0.7, show_guide = F)
 g = g + geom_text(data = df[df$areal.unit == "500km",], aes(label = label), family = "Times New Roman", size = 5.4, hjust = -0.2, vjust = 0.5, show_guide = F)
 #g = g + geom_text(data = df[df$areal.unit == "500km",], aes(label = label), family = "Times New Roman", size = 5.4, hjust = -0.2, vjust = 0.2, show_guide = F)
 #g = g + geom_text(data = df[df$areal.unit == "100km",], aes(label = label), family = "Times New Roman", size = 5.4, hjust = -0.2, vjust = 0.2, show_guide = F)
 #g = g + scale_y_continuous(breaks = seq(0, .48, .06))
 #g = g + scale_x_continuous(breaks = seq(0, 50, 5))
 g = g + guides(linetype = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))
 g = g + scale_shape_manual(values = rev(c(1,2,3,4,5,8,9,0,11,13,15,6)))
 g = g + theme_classic()
 g = g + labs(title = "", y = global.morans.i.text, x = local.morans.i.text, linetype = "Pareto frontiers", shape = "Pareto frontiers")
 g = g + theme(text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
               axis.text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
               axis.title = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
               axis.title.y = element_text(margin = margin(0,8,0,2)),
               axis.title.y.right = element_text(margin = margin(2,0,0,8)),
               legend.text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
               legend.position = "bottom",
               legend.key.width = unit(30, "pt"),
               legend.justification="left",
               legend.title = element_text(family = "Times New Roman", size = 16, face = 'bold', color = "black", angle=0),
               plot.margin = unit(c(-8,6,0,2), "pt")
 )
 show(g)

#ggsave(filename="ParetoSO2.png", plot=g)

# # Table 1
 tab1 = df[, c('.level', 'label', 'global.moran.i','global.pvalue.moran.i','local.moran.i.cvar')]
 tab1$global.moran.i = round(tab1$global.moran.i, 2)
 tab1$local.moran.i.cvar = round(tab1$local.moran.i.cvar, 2)
 colnames(tab1) = c('Frontier','Scale',"Moran's I",'p.value','LISA (CV)')
 print(tab1)
# write.csv(tab1, 'table-pareto.csv')


# # ########################################################
# # Figure 8 and Table 2
# # Robustness of the Pareto-optimal areal units using the bootstrap method with 1,000 replications
# # export: width=1100 height=520
# # # ########################################################

if (!require("dplyr")) install.packages("dplyr")
if (!require("data.table")) install.packages("data.table")
if (!require("ggplot2")) install.packages("ggplot2")

df.bootstrap.samples = data.frame()

# pareto-solution of the bootstrap samples
for(i in seq(1:200)) {
  file = paste0("/data/doutorado-leonildo/data/seasons/O3-2019/fall/pareto_optimality/pareto_front_US_pollution_", i, ".csv")
  df = as.data.frame(fread(file, sep = ",", dec = "."))
  df['.level'] = paste0("#",df$`low(local.moran.i.cvar) * high(global.moran.i)`)
  df['label'] = paste0(strsplit(df$areal.unit,"km"), " km")
  df['file'] = file
  df.bootstrap.samples = rbind(df.bootstrap.samples, df[df$.level == '#1',])
}

df.bootstrap.samples$global.moran.i = round(df.bootstrap.samples$global.moran.i, 2)
df.bootstrap.samples$local.moran.i.cvar = round(df.bootstrap.samples$local.moran.i.cvar, 2)

# Table 2
df.freq = as.data.frame(dplyr::count(df.bootstrap.samples, vars = c(label), name = 'freq'))
df.freq = df.freq[order(-df.freq$freq),] 
df.freq$per = round(df.freq$freq/sum(df.freq$freq), 4) * 100
tab2 = df.freq

OutVals = boxplot(df.bootstrap.samples$global.moran.i, plot = FALSE)$out
df.bootstrap.samples = df.bootstrap.samples[!df.bootstrap.samples$global.moran.i %in% OutVals, ]
OutVals = boxplot(df.bootstrap.samples$local.moran.i.cvar, plot = FALSE)$out
df.bootstrap.samples = df.bootstrap.samples[!df.bootstrap.samples$local.moran.i.cvar %in% OutVals, ]

# Table 2
df.freq = as.data.frame(dplyr::count(df.bootstrap.samples, vars = c(label), name = 'freq'))
df.freq = df.freq[order(-df.freq$freq),] 
df.freq$per = round(df.freq$freq/sum(df.freq$freq), 4) * 100
tab2.out.removed = df.freq

global.morans.i.text = expression(paste("Global Moran's ", italic("I")))
local.morans.i.text = expression(paste("Coefficient of variation of the Local Moran's ", italic("I")))


#g = ggplot(df.bootstrap.samples, aes(x = local.moran.i.cvar, y = global.moran.i))
g = ggplot(df.bootstrap.samples, aes(x = local.moran.i.cvar, y = global.moran.i, 
                                     shape = factor(label, 
                                                    levels = rev(c("100 km","200 km","300 km", 
                                                                   "400 km", "500 km"))))) 
g = g + geom_point(size = 2.4, alpha = .8)
#g = g + scale_y_continuous(breaks = seq(0, .5, .02))
#g = g + scale_x_continuous(breaks = seq(0, 5, .2))
g = g + guides(linetype = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))
g = g + scale_shape_manual(values = rev(c(1,2,3,4,5,8,9,10,11,13)))
g = g + theme_classic()
g = g + labs(title = "", y = global.morans.i.text, x = local.morans.i.text, shape = "Areal units")
g = g + theme(text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
              axis.text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
              axis.title = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
              axis.title.y = element_text(margin = margin(0,8,0,2)),
              axis.title.y.right = element_text(margin = margin(2,0,0,8)),
              legend.text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
              legend.position = "bottom",
              legend.key.width = unit(0, "pt"),
              legend.justification="left",
              legend.title = element_text(family = "Times New Roman", size = 16, face = 'bold', color = "black", angle=0),
              plot.margin = unit(c(-8,6,0,2), "pt")
)
show(g)
# ggsave(filename="robust-paretto.png", plot=g)

# Table 2
tab2
#tab2.out.removed
#write.csv(tab2, "robust-paretto.csv")

############################### EAF #####################################


#if (!require("eaf")) install.packages("eaf")
#if (!require("dplyr")) install.packages("dplyr")


# assuming df.bootstrap.samples is your data.frame
#y = dplyr::select(df.bootstrap.samples, local.moran.i.cvar,global.moran.i)
#x = as.matrix(sapply(y, as.numeric))
#x = matrix(as.numeric(unlist(y)),nrow=length(df.bootstrap.samples$local.moran.i.cvar))
#label = dplyr::select(df.bootstrap.samples, label)

#print(label)
#print(x)

#eafplot(y, aes(x = local.moran.i.cvar, y = global.moran.i))


# ########################################################
# Figure 9
# Comparison of spatial patterns of Pareto-optimal areal units (middle) and four arbitrary areal
# units (extremes).
# export: width=960 height=280
# ########################################################

 if (!require("rgdal")) install.packages("rgdal")
 if (!require("rgeos")) install.packages("rgeos")
 if (!require("data.table")) install.packages("data.table")
 if (!require("plyr")) install.packages("plyr")
 if (!require("spdep")) install.packages("spdep")
 if (!require("ggplot2")) install.packages("ggplot2")
 if (!require("gridExtra")) install.packages("gridExtra")
 if (!require("grid")) install.packages("grid")
 if (!require("classInt")) install.packages("classInt")
 if (!require("ggsn")) install.packages("ggsn")

 files = list()
 files = lappend(files, c("./10-0km/hex-grid.shp","./10-0km/final-file.csv"))
 files = lappend(files, c("./10-1km/hex-grid.shp","./10-1km/final-file.csv"))
 files = lappend(files, c("./10-2km/hex-grid.shp","./10-2km/final-file.csv"))
 files = lappend(files, c("./10-3km/hex-grid.shp","./10-3km/final-file.csv"))
 #files = lappend(files, c("./400km/hex-grid.shp","./400km/final-file.csv"))
 #files = lappend(files, c("./90km/hex-grid.shp","./90km/final-file.csv"))
 #files = lappend(files, c("./500km/hex-grid.shp","./500km/final-file.csv"))

 #date.start = as.POSIXct("2016-11-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
 #date.end = as.POSIXct("2017-11-30 23:59:59", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

 grids = lapply(files, function(file) {
   grid = readOGR(dsn = file[1] , verbose = FALSE, encoding = 'iso-8859-1', stringsAsFactors = FALSE)
   #df = as.data.frame(fread(file[2], sep = ",", dec = ".")[, c('w', 'DateLocalCount', 'NO2AQI' ,'O3AQI', 
   #	'SO2AQI','COAQI')])
   df = as.data.frame(fread(file[2], sep = ",", dec = ".")[, c('w', 'AQI')])
   #colnames(df) = c('ID', 'DateLocalCount', 'NO2AQI' ,'O3AQI', 'SO2AQI','COAQI')
   colnames(df) = c('ID', 'AQI')

   df['scale'] = paste(strsplit(strsplit(file[2], "/")[[1]][2], "km")[[1]][1], "km")
   df['zone'] = 'hexagonal'
   #df.agg.n = aggregate(n ~ id + scale + zone, data = df, sum)
   #df.agg.abs = aggregate(abs ~ id + scale + zone, data = df, sum)
   #df.agg.n_pop = aggregate(n_pop ~ id + scale + zone, data = df, sum)
   #df.agg.abs_pop = aggregate(abs_pop ~ id + scale + zone, data = df, sum)
   #df = join(df.agg.n, df.agg.abs, by = c("id", "scale", "zone"))
   #df = join(df, df.agg.n_pop, by = c("id", "scale", "zone"))
   #df = join(df, df.agg.abs_pop, by = c("id", "scale", "zone"))

   # odds ratio measure of the frequency of geotagged tweets
   # df['or'] = round(((df['abs'] * df['n_pop']) / (df['abs_pop'] * df['n'])), 4)

   grid@data = join(grid@data, df, by = c("ID"))
   grid@data$zone = as.character(unique(df['zone']))
   grid@data$scale = as.character(unique(df['scale']))
   return(grid)
 })

 l.global.morans.i = list()
 l.mean.local.morans.i = list()
 l.sd.local.morans.i = list()
 l.cvar.local.morans.i = list()
 l.scale = list()
 l.zone = list()

 df.local = data.frame()

 # to calculate the global and coef.var. of the local moran's i for each grid (a to e)
 for(i in 1:length(grids) ) {
   grid = grids[[i]]
   nb <- poly2nb(pl = grid, row.names = grid@data[['ID']], queen = FALSE)
   centroids = gCentroid(grid, byid = TRUE, id = grid@data[['ID']])
   dists = nbdists(nb = nb, coords = centroids@coords, longlat = TRUE)
   dists.inv = lapply(dists, function(x) (1/x))
   nbl = nb2listw(nb, glist = dists.inv, style = "W", zero.policy = FALSE)
   grid@data[is.na(grid@data[attrName]), attrName] = 0

   global.morans.i = round(moran.test(grid@data[, attrName], listw = nbl, na.action = na.exclude)$estimate[[1]], 4)
   l.global.morans.i = c(l.global.morans.i, global.morans.i)
   local.morans.i = cbind(grid@data$id, as.data.frame(localmoran(grid@data[, attrName], listw = nbl, na.action = na.exclude))[,1])

   mean.local.morans.i = round(sd(local.morans.i, na.rm = TRUE), 4)
   l.mean.local.morans.i = c(l.mean.local.morans.i, mean.local.morans.i)
   sd.local.morans.i = round(sd(local.morans.i, na.rm = TRUE), 4)
   l.sd.local.morans.i = c(l.sd.local.morans.i, sd.local.morans.i)
   cvar.local.morans.i = round(sd.local.morans.i/abs(mean.local.morans.i), 4)
   l.cvar.local.morans.i = c(l.cvar.local.morans.i, sd.local.morans.i)

   x = unique(grid@data$scale)
   x = x[!is.na(x)]
   l.scale = c(l.scale, as.character(x))
   x = unique(grid@data$zone)
   x = x[!is.na(x)]
   l.zone = c(l.zone, as.character(x))
   df.local = rbind(df.local, as.data.frame(cbind(unlist(l.zone[i]), unlist(l.scale[i]), local.morans.i)))
 }
 print(colnames(df.local))
 #colnames(df.local) = c('zone', 'scale', 'ID', 'local.morans.i')
 colnames(df.local) = c('zone', 'scale', 'local.morans.i')
 l.global.morans.i = unlist(l.global.morans.i)
 l.mean.local.morans.i = unlist(l.mean.local.morans.i)
 l.sd.local.morans.i = unlist(l.sd.local.morans.i)
 l.cvar.local.morans.i = unlist(l.cvar.local.morans.i)
 l.zone = unlist(l.zone)
 l.scale = unlist(l.scale)

 df = as.data.frame(cbind(l.global.morans.i, l.cvar.local.morans.i, l.sd.local.morans.i, l.mean.local.morans.i), stringsAsFactors = TRUE)
 df = cbind(l.zone, l.scale, df)
 df[is.infinite(df$l.cvar.local.morans.i), 'l.cvar.local.morans.i'] = 999
 df = arrange(df, desc(l.global.morans.i), l.cvar.local.morans.i)

 plots=list()
 plots = lapply(grids, function(grid) {
   city = readOGR(dsn = "./city/grid.shp", verbose = FALSE)
   #city = readOGR(dsn = "./city/USA.shp", verbose = FALSE)
   coords = fortify(model = grid, region = 'ID')

   grid@data[grid@data==0] <- NA # change 0 to Na
   print(grid@data[,attrName])
   breaks_qt = classIntervals(grid@data[,attrName], n = 4, style = "quantile")
   breaks_qt = breaks_qt$brks
   breaks_qt = unique(breaks_qt)

   values = c("#deebf7","#9ecae1","#4292c6","#08306b", "#808080")
   labels = c(expression(paste("1"^st)), expression(paste("2"^nd)), 
              expression(paste("3"^rd)), expression(paste("4"^th)), "No data")
   grid@data[,'class'] = cut(grid@data[, attrName], breaks = breaks_qt, include.lowest = TRUE)
   #print(grid@data)
   colnames(coords) = c('long','lat','order','hole','piece', 'ID', 'group')

   df = join(coords, grid@data, by = c("ID", "ID"))

   #colnames(df) = c('long','lat','order','hole','piece','id','group','DateLocalCount','NO2AQI','O3AQI','SO2AQI',
   #                 'COAQI','scale','zone','class')
   colnames(df) = c('long','lat','order','hole','piece','id','group','AQI','scale','zone','class')
   g = ggplot(data = df, aes(x = long, y = lat, group = group, fill = df[, 'class']))
   g = g + geom_map(map = df, aes(map_id = id),  colour = "grey40", size = .5)
   g = g + scale_fill_manual(values = values, 
                             # name = "Quantiles of the ‘odds ratio measure’ of the frequency of tweets", 
                             name = attrName, 
                             na.value = "#808080",#"#000000",
                             labels = labels)
   #g = g + expand_limits()
   g = g + coord_equal()
   g = g + geom_path(color = "white", size = .15)
   g = g + geom_polygon(data = city, aes(x = long, y = lat, group = group), fill = NA, color = "black", size=.25)
   g = g + facet_grid(facets = . ~ scale, margins = FALSE)
   #g = g + theme_bw()
   #g = g + theme(aspect.ratio=4/3)
   g = g + labs(fill='')
   #g = g + annotate(geom = "text", x = -46.47, y = -23.74, size = 4.1,
   #                 label = paste("\u2113 = ",round(sqrt((as.numeric(strsplit(unique(grid@data$scale)," ")[[1]][1]) * 2) / (3 * sqrt(3))), 1), "km"))
   a = paste("\u2113 = ", strsplit(unique(grid@data$scale),"km")[[1]], 'km')
   g = g + annotate(geom = "text", x = -46.47, y = -23.74, size = .41, label = a)
   #g = g + xlim(-125, -112)
   #g = g + ylim(32,42)
 #  g = g + theme(axis.text = element_blank(),
 #                axis.ticks = element_blank(),
 #                axis.title = element_blank(),
 #                strip.background = element_blank(),
 #                strip.text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
 #                legend.position = "bottom",
 #                panel.spacing = unit(0, "lines"),
 #                panel.grid.major.x = element_blank(),
 #                panel.grid=element_blank(),
 #                panel.border = element_rect(colour = "white"),
 #                legend.key.width = unit(16, "pt"),
 #                legend.key.height = unit(16, "pt"),
 #                legend.text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
 #                legend.title = element_text(family = "Times New Roman", size = 16, face = 'bold', color = "black", angle=0),
 #                plot.margin = unit(c(-25,0,0,0), "pt")
#
#   )
   #g = g + xlim(-125, -112)
   #g = g + ylim(32,42)
   g = g + xlim(-119, -117.5)
   g = g + ylim(33.6,34.8)
  # show(g)
   return(g)
 })
 print(length(grids))
 print(length(plots))

# g = grid_arrange_shared_legend(plots[[4]],
## 								plots[[5]], 
##                                plots[[4]], 
##                                plots[[3]], 
#                                plots[[2]], 
#                                plots[[1]],
#                                ncol = 4, nrow = 1, position = "bottom")
 
 ##g = grid_arrange_shared_legend(plots[1], plots[2], ncol = 2, nrow = 1, position = "bottom")
 #show(g)
 #ggsave(filename="maps.png", plot=g)
 #north2(g,.98, scale = 0.15, symbol = 3.2)
