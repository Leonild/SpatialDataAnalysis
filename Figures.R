## Authors: Leonildo (adapted from Sidgley)
## Date: 2020-03-20
## Modified: 2020-03-20
## Ph.D. Leonildo

# define path of the current script
path = "/data/doutorado-leonildo/data/California/"  # (for example)
setwd(path)

attrName = 'NO2AQI'


# ########################################################
# Common functions
# ########################################################

# function 1
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[3]] + theme(legend.position = position))$grobs
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
# Figure 1
# export: width=800 height=450
# ########################################################

# if (!require("rgdal")) install.packages("rgdal")
# if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("gridExtra")) install.packages("gridExtra")
# if (!require("dplyr")) install.packages("dplyr")
# if (!require("grid")) install.packages("grid")
# if (!require("maptools")) install.packages("maptools")

# files = list()
# files = lappend(files, "/home/leonildo/California/grid/grid.shp")
# files = lappend(files, "/home/leonildo/California/grid/grid6.shp")
# files = lappend(files, "/home/leonildo/California/grid/grid5.shp")
# files = lappend(files, "/home/leonildo/California/grid/grid2.shp")
# files = lappend(files, "/home/leonildo/California/grid/grid3.shp")
# files = lappend(files, "/home/leonildo/California/grid/grid4.shp")

# grid = readOGR(dsn = files[[2]] , verbose = FALSE, encoding = 'iso-8859-1', stringsAsFactors = FALSE)

# # generate a random spatial data for each grid
# aux = grid[grid$id == 1, ]
# points1 = spsample(x = aux, n = 120, "random")
# aux = grid[grid$id == 2, ]
# points2 = spsample(x = aux, n = 210, "random")
# aux = grid[grid$id == 3, ]
# points3 = spsample(x = aux, n = 10, "random")
# aux = grid[grid$id == 4, ]
# points4 = spsample(x = aux, n = 250, "random")
# aux = grid[grid$id == 5, ]
# points5 = spsample(x = aux, n = 30, "random")
# aux = grid[grid$id == 6, ]
# points6 = spsample(x = aux, n = 20, "random")
# aux = grid[grid$id == 7, ]
# points7 = spsample(x = aux, n = 120, "random")
# aux = grid[grid$id == 8, ]
# points8 = spsample(x = aux, n = 30, "random")
# aux = grid[grid$id == 9, ]
# points9 = spsample(x = aux, n = 210, "random")

# points = rbind(points1, points2, points3, points4, points5, points6, points7, points8, points9)
# proj4string(points) <- proj4string(grid)

# plots = lapply(files, function(file) {
#   grid = readOGR(dsn = file , verbose = FALSE, encoding = 'iso-8859-1', stringsAsFactors = FALSE)
#   freq = as.data.frame(table(over(points, grid), dnn = c('id','freq')))
#   freq$id = as.character(freq$id)
#   grid@data = left_join(grid@data, freq, by = c('id'='id'))
#   grid@data['Per'] = round(grid@data$Freq/10,0)
  
#   if(file != "/home/leonildo/California/grid/grid6.shp" & file != "/home/leonildo/California/grid/grid.shp") {
#     breaks_qt = c(0, 30, 50, 100)
#     values = c("#d4d4d4","#9a9a9a","#707070","#454545")
#     labels = c('Low','Medium','High')
#     grid@data[,'class'] = cut(grid@data$Per, breaks = breaks_qt, include.lowest = TRUE)
#   } else {
#     grid@data[,'class'] = 1
#   }
  
#   coords = fortify(model = grid, region = 'id')
#   df = full_join(coords, grid@data, by = c("id", "id"))
  
#   centroids = aggregate(cbind(long,lat) ~ id, data=df, FUN=mean)
#   colnames(centroids) = c('id','long.c', 'lat.c')
#   df = full_join(df, centroids, by = c("id", "id"))
#   centroids = left_join(centroids, grid@data, by = c('id'= 'id'))
  
#   if(file != "/home/leonildo/California/grid/grid6.shp" & 
#      file != "/home/leonildo/California/grid/grid.shp") {
#     g = ggplot(data = df, aes(x = long, y = lat, group = group, fill = class))
#     g = g + geom_map(map = df, aes(map_id = id),  colour = "black", size = .6)
#     g = g + scale_x_continuous(expand = c(0, 0))
#     g = g + scale_y_continuous(expand = c(0, 0))
#     g = g + scale_fill_manual(values = values, 
#                               name = "Density of social media data", 
#                               na.value = "#000000",
#                               labels = labels)
#     g = g + geom_text(aes(x = long.c, y = lat.c, label = paste0(Per,"%")))
#     g = g + expand_limits()
#     g = g + coord_equal()
#     g = g + theme_bw()
#     g = g + labs(fill = '')
#     g = g + theme(axis.text = element_blank(),
#                   axis.ticks = element_blank(),
#                   axis.title = element_blank(),
#                   strip.background = element_blank(),
#                   strip.text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
#                   legend.position = "bottom",
#                   panel.spacing = unit(10, "lines"),
#                   panel.grid.major.x = element_blank(),
#                   panel.grid=element_blank(),
#                   legend.key.width = unit(16, "pt"),
#                   legend.key.height = unit(16, "pt"),
#                   legend.text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
#                   legend.title = element_text(family = "Times New Roman", size = 16, face = 'bold', color = "black", angle=0),
#                   plot.margin = unit(c(5,10,0,0), "pt"))
#     g
    
#   } else if(file == "/home/leonildo/California/grid/grid6.shp") {
#     g = ggplot(data = df, aes(x = long, y = lat, group = group, fill = class))
#     g = g + geom_map(map = df, aes(map_id = id),  colour = "black", fill = 'white', size = .6)
#     g = g + scale_x_continuous(expand = c(0, 0))
#     g = g + scale_y_continuous(expand = c(0, 0))
#     g = g + geom_text(aes(x = long.c, y = lat.c, label = Freq))
#     g = g + expand_limits()
#     g = g + coord_equal()
#     g = g + theme_bw()
#     g = g + labs(fill = '')
#     g = g + theme(axis.text = element_blank(),
#                   axis.ticks = element_blank(),
#                   axis.title = element_blank(),
#                   strip.background = element_blank(),
#                   strip.text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
#                   legend.position = "none",
#                   panel.spacing = unit(10, "lines"),
#                   panel.grid.major.x = element_blank(),
#                   panel.grid=element_blank(),
#                   legend.key.width = unit(16, "pt"),
#                   legend.key.height = unit(16, "pt"),
#                   legend.text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
#                   legend.title = element_text(family = "Times New Roman", size = 16, face = 'bold', color = "black", angle=0),
#                   plot.margin = unit(c(5,10,0,0), "pt"))
#   } else {
#     pts = as.data.frame(points)
#     g = ggplot(data = df, aes(x = long, y = lat, fill = class))
#     g = g + geom_map(map = df, aes(map_id = id),  colour = "black", fill = "white", size = .6)
#     g = g + geom_point(data = pts, mapping = aes(x = pts$x, y = pts$y, group = 1), 
#                        shape = 20, size = .5, color = "black", fill = NA)
#     g = g + scale_x_continuous(expand = c(0, 0))
#     g = g + scale_y_continuous(expand = c(0, 0))
#     g = g + expand_limits()
#     g = g + coord_equal()
#     g = g + theme_bw()
#     g = g + labs(fill = '')
#     g = g + theme(axis.text = element_blank(),
#                   axis.ticks = element_blank(),
#                   axis.title = element_blank(),
#                   strip.background = element_blank(),
#                   strip.text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
#                   legend.position = "none",
#                   panel.spacing = unit(10, "lines"),
#                   panel.grid.major.x = element_blank(),
#                   panel.grid=element_blank(),
#                   legend.key.width = unit(16, "pt"),
#                   legend.key.height = unit(16, "pt"),
#                   legend.text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
#                   legend.title = element_text(family = "Times New Roman", size = 16, face = 'bold', color = "black", angle=0),
#                   plot.margin = unit(c(5,10,0,0), "pt"))
#   }
#   return(g)
# })

# g = grid_arrange_shared_legend(plots[[3]], plots[[4]], plots[[5]], plots[[6]], 
#                                plots[[1]], plots[[2]], ncol = 4, nrow = 2, position = "bottom")
# show(g)


# ########################################################
# Figure 2
# export: width=780 height=340
# ########################################################

# if (!require("spdep")) install.packages("spdep")
# if (!require("reshape2")) install.packages("reshape2")
# if (!require("dplyr")) install.packages("dplyr")
# if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("cowplot")) install.packages("cowplot")

# # to create a regular grid 5x5
# grid <- expand.grid(x = 1:5, y = 1:5) 
# grid['a-1'] = c(1,1,1,1,1,  1,1,1,1,1,  1,1,0,0,0,  1,1,1,0,0,  1,1,1,1,0)
# grid['a-2'] = c(1,1,0,1,1,  1,1,1,1,0,  1,1,1,0,0,  1,1,1,0,0,  1,1,0,0,0)
# grid['a-3'] = c(1,1,1,0,0,  1,1,1,0,0,  0,1,0,0,0,  0,1,0,0,0,  1,1,1,0,0)
# grid['a-4'] = c(0,0,0,0,0,  1,0,0,0,0,  1,0,1,1,1,  1,1,1,1,1,  1,1,1,0,1)
# grid['a-5'] = c(1,1,1,1,1,  1,1,1,1,1,  1,1,1,0,1,  0,0,1,1,1,  0,0,0,1,1)

# grid.melt = melt(grid, id.vars = c('x','y'))

# w = as.matrix(dist(cbind(grid$x, grid$y)))
# w = 1/w
# diag(w) = 0
# w = trunc(w)
# w = mat2listw(w, row.names = NULL, style = "M")
# w = nb2listw(w$neighbours, style = "W")

# l.global.morans.i = list()
# l.mean.local.morans.i = list()
# l.sd.local.morans.i = list()
# l.name = list()

# df = data.frame(id = seq(1:(max(grid$x) * max(grid$y))))

# # to calculate the global and std of the local moran's i for each grid (a-1 to a-5)
# for(i in seq(3, ncol(grid))) {
#   global.morans.i = round(moran.test(grid[,i], w)$estimate[[1]], 4)
#   l.global.morans.i = c(l.global.morans.i, global.morans.i)
#   local.morans.i = as.data.frame(localmoran(grid[,i], w))[,1]
#   mean.local.morans.i = round(sd(local.morans.i, na.rm = TRUE), 4)
#   l.mean.local.morans.i = c(l.mean.local.morans.i, mean.local.morans.i)
#   sd.local.morans.i = round(sd(local.morans.i, na.rm = TRUE), 4)
#   l.sd.local.morans.i = c(l.sd.local.morans.i, sd.local.morans.i)
#   l.name = c(l.name, colnames(grid)[i])
# }
# l.global.morans.i = unlist(l.global.morans.i)
# l.mean.local.morans.i = unlist(l.mean.local.morans.i)
# l.sd.local.morans.i = unlist(l.sd.local.morans.i)
# l.name = unlist(l.name)

# df = as.data.frame(cbind(l.global.morans.i, l.mean.local.morans.i, 
#                          l.sd.local.morans.i), stringsAsFactors = FALSE)
# df = cbind(l.name, df)
# colnames(df) = c('grid', 'global.morans.i', 'mean.local.morans.i', 'sd.local.morans.i')

# # normalize sd.local.morans.i based on global.morans.i
# alpha = min(df$global.morans.i)
# beta = max(df$global.morans.i)
# df['sd.local.morans.i.normalized'] = ((beta - alpha) * ((df$sd.local.morans.i - min(df$sd.local.morans.i)) / 
#                                                         (max(df$sd.local.morans.i) - min(df$sd.local.morans.i)))) + alpha
# df1 = df
# df2 = df
# df1['group'] = "Global Moran's I"
# df2['group'] =  "\u0394 Std. Dev. of the LISA"
# df = rbind(df1, df2)
# df = arrange(df, desc(global.morans.i), mean.local.morans.i)
  
# grid.melt$variable = as.character(grid.melt$variable)
# grid.melt$variable = factor(grid.melt$variable, l.name)

# g = ggplot(grid.melt, aes(x = x, y = y, group = variable))
# g = g + geom_tile(aes(fill = value), colour = "black", size = .5)
# g = g + scale_fill_gradient(low = "white", high = "#454545")
# g = g + scale_x_continuous(expand = c(0, 0))
# g = g + scale_y_continuous(expand = c(0, 0))
# g = g + facet_wrap(~ variable, ncol = 5)
# g = g + coord_equal()
# g = g + theme(legend.position = "none",
#               axis.text.x = element_blank(),
#               strip.background = element_blank(),
#               strip.text = element_text(family = "Times New Roman", size = 14, color = "black", angle=0),
#               panel.border = element_rect(colour = "black", fill=NA, size=1),
#               panel.spacing = unit(.6, "lines"),
#               axis.title = element_blank(),
#               axis.text = element_blank(),
#               axis.ticks = element_blank(),
#               plot.margin = unit(c(0,10,-10,20), "pt")
              
# )
# g.A = g

# global.morans.i.text = expression(paste("Global Moran's ", italic("I")))
# local.morans.i.text = expression(paste("Standard deviation of the Local Moran's ", italic("I"), " (normalized)"))

# g = ggplot(data = df, mapping = aes(x = factor(df$grid, l.name), group = 1))
# g = g + geom_line(mapping = aes(y = global.morans.i,
#                                 colour = "Global",
#                                 linetype = "Global"), 
#                   position = position_dodge(width = 0))
# g = g + geom_point(mapping = aes(y = global.morans.i,
#                                  colour = "Global", 
#                                  shape = "Global"), 
#                    size = 2.5, 
#                    position = position_dodge(width = 0))
# g = g + geom_line(mapping = aes(y = sd.local.morans.i.normalized,
#                                 colour = "Local",
#                                 linetype = "Local"), 
#                   position = position_dodge(width = 0))
# g = g + geom_point(mapping = aes(y = sd.local.morans.i.normalized, 
#                                  colour = "Local", 
#                                  shape = "Local"), 
#                    size = 2.5, 
#                    position = position_dodge(width = 0))
# g = g + scale_x_discrete(labels = levels)
# g = g + scale_y_continuous(breaks = seq(.45, .50, .01),limits = c(.45,.50))
# g = g + guides(linetype = guide_legend(""), 
#                colour = guide_legend(""),
#                shape = guide_legend(""))
# g = g + scale_colour_manual(labels = c(global.morans.i.text, local.morans.i.text) , values = c("Global" = "black", "Local" = "black"))
# g = g + scale_linetype_manual(labels = c(global.morans.i.text, local.morans.i.text), values = c("Global" = 1, "Local" = 4))
# g = g + scale_shape_manual(labels = c(global.morans.i.text, local.morans.i.text), values = c("Global" = 19, "Local" = 17))
# g = g + labs(title = "", x = "", y = "", y.sec = "")
# g = g + theme_classic()
# g = g + theme(axis.text = element_text(family = "Times New Roman", size = 14, color = "black", angle=0),
#               axis.title = element_text(family = "Times New Roman", size = 14, color = "black", angle=0),
#               axis.title.y = element_text(margin = margin(0,8,0,2)),
#               legend.text = element_text(family = "Times New Roman", size = 14, color = "black", angle=0),
#               legend.position = "bottom",
#               legend.box.margin = unit(c(-20,0,0,0), "pt"),
#               legend.key.width = unit(32, "pt"),
#               legend.justification="left",
#               plot.margin = unit(c(0,6,0,2), "pt")
# )
# g.B = g

# plot_grid(g.A, g.B, labels = c("(a)", "(b)"), nrow = 2, align = "none")


# ########################################################
# Figure 5
# export: width=820 height=493
# ########################################################

# if (!require("data.table")) install.packages("data.table")
# if (!require("plyr")) install.packages("plyr")
# if (!require("ggplot2")) install.packages("ggplot2")
# if (!require("gridExtra")) install.packages("gridExtra")

# date.start = as.POSIXct("2016-11-07 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# date.end = as.POSIXct("2017-04-30 23:59:59", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# tweets = as.data.frame(fread("/home/leonildo/California/city/tweet_signals.csv", sep = ",", dec = ".")[, c('w', 'datetime', 'abs')])
# tweets$datetime = as.POSIXct(tweets$datetime, tz = "UTC")
# tweets = tweets[(tweets$datetime >= date.start) & (tweets$datetime <= date.end), ]

# radar = as.data.frame(fread("/home/leonildo/California/weather_radar_measurements.csv", sep = ",", dec = ".")[, c('w', 'datetime', 'mean')])
# radar$datetime = as.POSIXct(radar$datetime, tz = "UTC")
# radar = radar[(radar$datetime >= date.start) & (radar$datetime <= date.end), ]

# df = join(tweets, radar, by = c("w"="w","datetime"="datetime"))
# df.melt = melt(df, id.vars = c('w','datetime'))

# g = ggplot(df, aes(x = datetime, y = mean, ymin = 0, ymax = mean))
# g = g + geom_linerange(size = 0.7, color = "#707070", alpha = 1.0) #0570b0
# g = g + geom_hline(yintercept = 10, linetype = 'dashed', size = .4)
# g = g + scale_y_continuous(limits = c(40, 0), breaks = seq(0, 40, 15), expand = c(0.01, 0), trans = "reverse")
# g = g + scale_x_discrete(expand = c(0,0))
# g = g + theme_classic()
# g = g + labs(y = "     Rainfall\n     (mm)")
# g = g + theme(plot.margin = unit(c(5,10,-32,1.0), units = "points"), 
#               panel.border = element_rect(colour = "black", fill=NA, size=.8),
#               axis.title.y = element_text(family = "Times New Roman", size = 14, color = "black", vjust = 0.3), 
#               axis.text.y = element_text(family = "Times New Roman", size = 14, color = "black", angle=0))
# g.top = g
# g.top

# g = ggplot(df, aes(x = datetime, y = abs))
# g = g + geom_line(size = 0.4, color = "black")
# g = g + scale_y_continuous(breaks = seq(0, 180, 20))
# g = g + scale_x_datetime(date_labels = "%d %b %y", date_breaks = "5 days", expand = c(0,0))
# g = g + theme_classic()
# g = g + labs(x = "",  
#              y = "Frequency of rain-related tweets") 
# g = g + theme(plot.margin = unit(c(0,10,-5,5.1), units="points"),
#               axis.title.x = element_text(family = "Times New Roman", size = 16, color = "black", margin=margin(0,0,0,0)), 
#               panel.border = element_rect(colour = "black", fill=NA, size=.8),
#               axis.title.y= element_text(family = "Times New Roman", size = 16, color = "black"), 
#               strip.text = element_text(family = "Times New Roman", size = 16, color = "black"), 
#               axis.text.x=element_text(family = "Times New Roman", size = 12, color = "black", angle = 90, vjust = 1, hjust = 1), 
#               axis.text.y=element_text(family = "Times New Roman", size = 16, color = "black"))
# g.bottom = g
# g.bottom

# grid.arrange(g.top, g.bottom, heights = c(1/5, 4/5))


# ########################################################
# Figure 6  
# Trade-off between the global indicator of spatial association (Global Moran’s I ) and the overall
# degree of structural (in)stability (coefficient of variation of Local Moran’s I normalized by scaling between the
# minimum and maximum values of the Global Moran’s I coefficients. Both global and local spatial statistics
# were computed for a row-standardized spatial weights matrix based on first-order rook contiguity.
# export: width=900 height=340
# ########################################################

# if (!require("spdep")) install.packages("spdep")
# if (!require("dplyr")) install.packages("dplyr")
# if (!require("data.table")) install.packages("data.table")
# if (!require("ggplot2")) install.packages("ggplot2")

# files = list()
# files = lappend(files, c("./California/50km/global_autocorrelation_US_pollution.csv","./California/50km/local_autocorrelation_US_pollution.csv"))
# files = lappend(files, c("./California/60km/global_autocorrelation_US_pollution.csv","./California/60km/local_autocorrelation_US_pollution.csv"))
# files = lappend(files, c("./California/70km/global_autocorrelation_US_pollution.csv","./California/70km/local_autocorrelation_US_pollution.csv"))
# files = lappend(files, c("./California/80km/global_autocorrelation_US_pollution.csv","./California/80km/local_autocorrelation_US_pollution.csv"))
# files = lappend(files, c("./California/90km/global_autocorrelation_US_pollution.csv","./California/90km/local_autocorrelation_US_pollution.csv"))
# files = lappend(files, c("./California/100km/global_autocorrelation_US_pollution.csv","./California/100km/local_autocorrelation_US_pollution.csv"))
# # files = lappend(files, c("/home/leonildo/California/60km/global_autocorrelation_US_pollution.csv","/home/leonildo/California/60km/local_autocorrelation_US_pollution.csv"))
# # files = lappend(files, c("/home/leonildo/California/50km/global_autocorrelation_US_pollution.csv","/home/leonildo/California/50km/local_autocorrelation_US_pollution.csv"))
# # files = lappend(files, c("/home/leonildo/California/40km/global_autocorrelation_US_pollution.csv","/home/leonildo/California/40km/local_autocorrelation_US_pollution.csv"))
# # files = lappend(files, c("/home/leonildo/California/30km/global_autocorrelation_US_pollution.csv","/home/leonildo/California/30km/local_autocorrelation_US_pollution.csv"))
# # files = lappend(files, c("/home/leonildo/California/20km/global_autocorrelation_US_pollution.csv","/home/leonildo/California/20km/local_autocorrelation_US_pollution.csv"))
# # files = lappend(files, c("/home/leonildo/California/10km/global_autocorrelation_US_pollution.csv","/home/leonildo/California/10km/local_autocorrelation_US_pollution.csv"))
# # files = lappend(files, c("/home/leonildo/California/5km/global_autocorrelation_US_pollution.csv","/home/leonildo/California/5km/local_autocorrelation_US_pollution.csv"))

# global = data.frame()
# local = data.frame()

# for(i in 1:length(files)) {
#   file = files[[i]]
#   df.global = as.data.frame(fread(file[1], sep = ",", dec = ".")[4 , c('attr', 'm.statistic.W_InvDist', 'm.p.value.W_InvDist')])
#   colnames(df.global) = c('attr', 'moran','p.value')
#   df.local = as.data.frame(fread(file[2], sep = ",", dec = ".")[ , c('attr', 'm.statistic.W_InvDist', 'm.p.value.W_InvDist')])
#   df.local = df.local[df.local['attr'] == attrName,]
#   colnames(df.local) = c('attr', 'moran','p.value')
  
#   mean = round(mean(df.local[,2], na.rm = TRUE), 4)
#   median = round(median(df.local[,2], na.rm = TRUE), 4)
#   sd = round(sd(df.local[,2], na.rm = TRUE), 4)
#   cvar = round(sd/abs(mean), 4)
#   df.local = as.data.frame(cbind(unique(df.local$attr), mean, median, sd, cvar))
#   colnames(df.local) = c('attr','mean','median','sd','cvar')
  
#   split.folder = (strsplit(file[1], "/"))[[1]]
#   print(split.folder)
#   scale = split.folder[-1][2]
#   zone = 'hexagonal'
  
#   split.folder = (strsplit(file[1], "_"))[[1]]
  
#   global = rbind(global, cbind(zone, scale, df.global))
#   local = rbind(local, cbind(zone, scale, df.local))
# }

# colnames(global) = c('zone','scale','attr', 'moran','p.value')
# colnames(local) = c('zone','scale','attr', 'mean','median','sd','cvar')

# global$scale = as.character(global$scale)
# local$scale = as.character(local$scale)
# global$zone = as.character(global$zone)
# local$zone = as.character(local$zone)
# global$attr = as.character(global$attr)
# local$attr = as.character(local$attr)
# local$mean = as.numeric(as.character(local$mean))
# local$median = as.numeric(as.character(local$median))
# local$sd = as.numeric(as.character(local$sd))
# local$cvar = as.numeric(as.character(local$cvar))

# df = full_join(local, global, by = c("zone"="zone","scale"="scale","attr"="attr"))

# levels = c("50 km","60 km","70 km", "80 km", "90 km", "100 km")

# df['scale2'] = paste0(strsplit(df$scale, "km"), " km")
# df['scale2'] = factor(df$scale2, levels = levels)

# pd = position_dodge(width = 0.9)
# et = element_text(family = "Times New Roman", size = 14, color = "black", angle=0)

# df['cvarnorm'] = 0
# alpha = min(df$moran)
# beta =  max(df$moran)
# df$cvarnorm = ((beta - alpha) * ((df$cvar - min(df$cvar)) / (max(df$cvar) - min(df$cvar)))) + alpha

# global.morans.i.text = expression(paste("Global Moran's ", italic("I")))
# local.morans.i.text = expression(paste("Coefficient of variation of Local Moran's ", italic("I"), " (normalized)"))
# local.morans.i.text2 = expression(paste("Coefficient of variation \n of Local Moran's ", italic("I"), " (normalized)"))

# g = ggplot(data = df, mapping = aes(x = scale2, group = 1))
# g = g + geom_line(mapping = aes(y = moran,
#                                 colour = "Global",
#                                 linetype = "Global"), 
#                   position = position_dodge(width = 0))
# g = g + geom_point(mapping = aes(y = moran,
#                                  colour = "Global", 
#                                  shape = "Global"), 
#                    size = 2.5, 
#                    position = position_dodge(width = 0))
# g = g + geom_line(mapping = aes(y = cvarnorm,
#                                 colour = "Local",
#                                 linetype = "Local"), 
#                   position = position_dodge(width = 0))
# g = g + geom_point(mapping = aes(y = cvarnorm, 
#                                  colour = "Local", 
#                                  shape = "Local"), 
#                    size = 2.5, 
#                    position = position_dodge(width = 0))
# g = g + scale_x_discrete(labels = levels)
# g = g + scale_y_continuous(breaks = seq(-.40, .40, .04), 
#                            sec.axis = sec_axis(~ ., breaks = seq(-.40, .40, .04), name = local.morans.i.text2))
# g = g + guides(linetype = guide_legend(""), colour = guide_legend(""), shape = guide_legend(""))
# g = g + scale_colour_manual(labels = c(global.morans.i.text, local.morans.i.text) , values = c("Global" = "black", "Local" = "black"))
# g = g + scale_linetype_manual(labels = c(global.morans.i.text, local.morans.i.text), values = c("Global" = 1, "Local" = 4))
# g = g + scale_shape_manual(labels = c(global.morans.i.text, local.morans.i.text), values = c("Global" = 19, "Local" = 17))
# g = g + labs(title = "", x = "", y = global.morans.i.text, y.sec = local.morans.i.text)
# g = g + theme_classic()
# g = g + theme(axis.text = element_text(family = "Times New Roman", size = 14, color = "black", angle=0),
#               axis.title = element_text(family = "Times New Roman", size = 14, color = "black", angle=0),
#               axis.title.y = element_text(margin = margin(0,8,0,2)),
#               legend.text = element_text(family = "Times New Roman", size = 14, color = "black", angle=0),
#               legend.position = "bottom",
#               legend.box.margin = unit(c(-20,0,0,0), "pt"),
#               legend.key.width = unit(32, "pt"),
#               legend.justification="left",
#               plot.margin = unit(c(-10,18,0,2), "pt")
# )

# show(g)


# ########################################################
# Figure 7 and Table 1
# Pareto frontiers and trade-off between Global Moran’s I and the coefficient of variation of Local
# Moran’s I (overall degree of structural (in)stability). Both statistics were computed for a row-standardized
# spatial weights matrix based on first-order rook contiguity.
# export: width=930 height=440
# ########################################################

# if (!require("dplyr")) install.packages("dplyr")
# if (!require("data.table")) install.packages("data.table")
# if (!require("ggplot2")) install.packages("ggplot2")

# file = "/home/leonildo/California/pareto_optimality/pareto_front_US_pollution.csv"
# df = as.data.frame(fread(file, sep = ",", dec = "."))

# df['.level'] = paste0("#",df$`low(local.moran.i.cvar) * high(global.moran.i)`)
# #df['label'] = paste0(strsplit(df$areal.unit,"km"), " km²")
# df['label'] = (df$areal.unit)

# global.morans.i.text = expression(paste("Global Moran's ", italic("I")))
# local.morans.i.text = expression(paste("Coefficient of variation of the Local Moran's ", italic("I"), " (spatial variance)"))

# g = ggplot(df, aes(x = local.moran.i.cvar, y = global.moran.i, shape = factor(.level), linetype = factor(.level))) 
# g = g + geom_point(size = 3.5)
# g = g + geom_step(direction = 'vh')
# g = g + geom_text(data = df[df$areal.unit == "50km",], aes(label = label), family = "Times New Roman", size = 5.4, hjust = -0.2, vjust = 0.7, show_guide = F)
# g = g + geom_text(data = df[df$areal.unit == "60km",], aes(label = label), family = "Times New Roman", size = 5.4, hjust = +1.2, vjust = 0.5, show_guide = F)
# g = g + geom_text(data = df[df$areal.unit == "70km",], aes(label = label), family = "Times New Roman", size = 5.4, hjust = -0.2, vjust = 0.7, show_guide = F)
# g = g + geom_text(data = df[df$areal.unit == "80km",], aes(label = label), family = "Times New Roman", size = 5.4, hjust = -0.2, vjust = 0.5, show_guide = F)
# g = g + geom_text(data = df[df$areal.unit == "90km",], aes(label = label), family = "Times New Roman", size = 5.4, hjust = -0.2, vjust = 0.2, show_guide = F)
# g = g + geom_text(data = df[df$areal.unit == "100km",], aes(label = label), family = "Times New Roman", size = 5.4, hjust = -0.2, vjust = 0.2, show_guide = F)
# #g = g + scale_y_continuous(breaks = seq(0, .48, .06))
# #g = g + scale_x_continuous(breaks = seq(0, 50, 5))
# g = g + guides(linetype = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))
# g = g + scale_shape_manual(values = rev(c(1,2,3,4,5,8,9,0,11,13,15,6)))
# g = g + theme_classic()
# g = g + labs(title = "", y = global.morans.i.text, x = local.morans.i.text, linetype = "Pareto frontiers", shape = "Pareto frontiers")
# g = g + theme(text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
#               axis.text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
#               axis.title = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
#               axis.title.y = element_text(margin = margin(0,8,0,2)),
#               axis.title.y.right = element_text(margin = margin(2,0,0,8)),
#               legend.text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
#               legend.position = "bottom",
#               legend.key.width = unit(30, "pt"),
#               legend.justification="left",
#               legend.title = element_text(family = "Times New Roman", size = 16, face = 'bold', color = "black", angle=0),
#               plot.margin = unit(c(-8,6,0,2), "pt")
# )
# show(g)

# ggsave(filename="ParetoSO2.png", plot=g)

# # Table 1
# tab1 = df[, c('.level', 'label', 'global.moran.i','global.pvalue.moran.i','local.moran.i.cvar')]
# tab1$global.moran.i = round(tab1$global.moran.i, 2)
# tab1$local.moran.i.cvar = round(tab1$local.moran.i.cvar, 2)
# colnames(tab1) = c('Frontier','Scale',"Moran's I",'p.value','LISA (CV)')
# tab1
# write.csv(tab1, 'table-pareto.csv')


# # ########################################################
# # Figure 8 and Table 2
# # Robustness of the Pareto-optimal areal units using the bootstrap method with 1,000 replications
# # export: width=1100 height=520
# # # ########################################################

# if (!require("dplyr")) install.packages("dplyr")
# if (!require("data.table")) install.packages("data.table")
# if (!require("ggplot2")) install.packages("ggplot2")

# df.bootstrap.samples = data.frame()

# # pareto-solution of the bootstrap samples
# for(i in seq(1:1000)) {
#   file = paste0("/home/leonildo/California/pareto_optimality/pareto_front_US_pollution_", i, ".csv")
#   df = as.data.frame(fread(file, sep = ",", dec = "."))
#   df['.level'] = paste0("#",df$`low(local.moran.i.cvar) * high(global.moran.i)`)
#   df['label'] = paste0(strsplit(df$areal.unit,"km"), " km")
#   df['file'] = file
#   df.bootstrap.samples = rbind(df.bootstrap.samples, df[df$.level == '#1',])
# }

# df.bootstrap.samples$global.moran.i = round(df.bootstrap.samples$global.moran.i, 2)
# df.bootstrap.samples$local.moran.i.cvar = round(df.bootstrap.samples$local.moran.i.cvar, 2)

# # Table 2
# df.freq = as.data.frame(dplyr::count(df.bootstrap.samples, vars = c(label), name = 'freq'))
# df.freq = df.freq[order(-df.freq$freq),] 
# df.freq$per = round(df.freq$freq/sum(df.freq$freq), 4) * 100
# tab2 = df.freq

# OutVals = boxplot(df.bootstrap.samples$global.moran.i, plot = FALSE)$out
# df.bootstrap.samples = df.bootstrap.samples[!df.bootstrap.samples$global.moran.i %in% OutVals, ]
# OutVals = boxplot(df.bootstrap.samples$local.moran.i.cvar, plot = FALSE)$out
# df.bootstrap.samples = df.bootstrap.samples[!df.bootstrap.samples$local.moran.i.cvar %in% OutVals, ]

# # Table 2
# df.freq = as.data.frame(dplyr::count(df.bootstrap.samples, vars = c(label), name = 'freq'))
# df.freq = df.freq[order(-df.freq$freq),] 
# df.freq$per = round(df.freq$freq/sum(df.freq$freq), 4) * 100
# tab2.out.removed = df.freq

# global.morans.i.text = expression(paste("Global Moran's ", italic("I")))
# local.morans.i.text = expression(paste("Coefficient of variation of the Local Moran's ", italic("I")))


# #g = ggplot(df.bootstrap.samples, aes(x = local.moran.i.cvar, y = global.moran.i))
# g = ggplot(df.bootstrap.samples, aes(x = local.moran.i.cvar, y = global.moran.i, 
#                                      shape = factor(label, 
#                                                     levels = rev(c("50 km","60 km","70 km", 
#                                                     	"80 km", "90 km", "100 km"))))) 
# g = g + geom_point(size = 2.4, alpha = .8)
# #g = g + scale_y_continuous(breaks = seq(0, .5, .02))
# #g = g + scale_x_continuous(breaks = seq(0, 5, .2))
# g = g + guides(linetype = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))
# g = g + scale_shape_manual(values = rev(c(1,2,3,4,5,8,9,10,11,13)))
# g = g + theme_classic()
# g = g + labs(title = "", y = global.morans.i.text, x = local.morans.i.text, shape = "Areal units")
# g = g + theme(text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
#               axis.text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
#               axis.title = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
#               axis.title.y = element_text(margin = margin(0,8,0,2)),
#               axis.title.y.right = element_text(margin = margin(2,0,0,8)),
#               legend.text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
#               legend.position = "bottom",
#               legend.key.width = unit(0, "pt"),
#               legend.justification="left",
#               legend.title = element_text(family = "Times New Roman", size = 16, face = 'bold', color = "black", angle=0),
#               plot.margin = unit(c(-8,6,0,2), "pt")
# )
# show(g)
# # ggsave(filename="robust-paretto.png", plot=g)

# # Table 2
# tab2
# tab2.out.removed
# write.csv(tab2, "robust-paretto.csv")

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
files = lappend(files, c("/home/leonildo/California/50km/hex-grid.shp","/home/leonildo/California/50km/final-file.csv"))
files = lappend(files, c("/home/leonildo/California/60km/hex-grid.shp","/home/leonildo/California/60km/final-file.csv"))
files = lappend(files, c("/home/leonildo/California/70km/hex-grid.shp","/home/leonildo/California/70km/final-file.csv"))
files = lappend(files, c("/home/leonildo/California/80km/hex-grid.shp","/home/leonildo/California/80km/final-file.csv"))
files = lappend(files, c("/home/leonildo/California/90km/hex-grid.shp","/home/leonildo/California/90km/final-file.csv"))
files = lappend(files, c("/home/leonildo/California/100km/hex-grid.shp","/home/leonildo/California/100km/final-file.csv"))

#date.start = as.POSIXct("2016-11-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
#date.end = as.POSIXct("2017-11-30 23:59:59", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

grids = lapply(files, function(file) {
  grid = readOGR(dsn = file[1] , verbose = FALSE, encoding = 'iso-8859-1', stringsAsFactors = FALSE)
  df = as.data.frame(fread(file[2], sep = ",", dec = ".")[, c('w', 'DateLocalCount', 'NO2AQI' ,'O3AQI', 
  	'SO2AQI','COAQI')])
  colnames(df) = c('ID', 'DateLocalCount', 'NO2AQI' ,'O3AQI', 'SO2AQI','COAQI')
  
  df['scale'] = paste(strsplit(strsplit(file[2], "/")[[1]][3], "km")[[1]][1], "km")
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
  city = readOGR(dsn = "./California/city/grid.shp", verbose = FALSE)
  coords = fortify(model = grid, region = 'ID')
  
  breaks_qt = classIntervals(grid@data[,attrName], n = 4, style = "quantile")
  breaks_qt = breaks_qt$brks
  breaks_qt = unique(breaks_qt)
  
  values = c("#deebf7","#9ecae1","#4292c6","#08306b")
  labels = c(expression(paste("1"^st)), expression(paste("2"^nd)), 
             expression(paste("3"^rd)), expression(paste("4"^th)), "No data")
  grid@data[,'class'] = cut(grid@data[, attrName], breaks = breaks_qt, include.lowest = TRUE)
  colnames(coords) = c('long','lat','order','hole','piece', 'ID', 'group')
  df = join(coords, grid@data, by = c("ID", "ID"))
  
  colnames(df) = c('long','lat','order','hole','piece','id','group','DateLocalCount','NO2AQI','O3AQI','SO2AQI',
                   'COAQI','scale','zone','class')
  g = ggplot(data = df, aes(x = long, y = lat, group = group, fill = df[, 'class']))
  g = g + geom_map(map = df, aes(map_id = id),  colour = "grey40", size = .5)
  g = g + scale_fill_manual(values = values, 
                            # name = "Quantiles of the ‘odds ratio measure’ of the frequency of tweets", 
                            name = attrName, 
                            na.value = "#000000",
                            labels = labels)
  #g = g + expand_limits()
  g = g + coord_equal()
  g = g + geom_path(color = "white", size = .15)
  g = g + geom_polygon(data = city, aes(x = long, y = lat, group = group), fill = NA, color = "black", size=.25)
  g = g + facet_grid(facets = . ~ scale, margins = FALSE)
  g = g + theme_bw()
  g = g + labs(fill='')
#  g = g + annotate(geom = "text", x = -46.47, y = -23.74, size = 4.1,
#                   label = paste("\u2113 = ",round(sqrt((as.numeric(strsplit(unique(grid@data$scale)," ")[[1]][1]) * 2) / (3 * sqrt(3))), 1), "km"))
  a = paste("\u2113 = ", strsplit(unique(grid@data$scale),"km²")[[1]], 'km')
  g = g + annotate(geom = "text", x = -46.47, y = -23.74, size = 4.1, label = a)
  g = g + theme(axis.text = element_blank(),
                axis.ticks = element_blank(),
                axis.title = element_blank(),
                strip.background = element_blank(),
                strip.text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
                legend.position = "bottom",
                panel.spacing = unit(0, "lines"),
                panel.grid.major.x = element_blank(),
                panel.grid=element_blank(),
                panel.border = element_rect(colour = "white"),
                legend.key.width = unit(16, "pt"),
                legend.key.height = unit(16, "pt"),
                legend.text = element_text(family = "Times New Roman", size = 16, color = "black", angle=0),
                legend.title = element_text(family = "Times New Roman", size = 16, face = 'bold', color = "black", angle=0),
                plot.margin = unit(c(-25,0,0,0), "pt")
                
  )
  
  return(g)
})

# print(plots)

g = grid_arrange_shared_legend(plots[[6]],
								plots[[5]], 
                               plots[[4]], 
                               plots[[3]], 
                               plots[[2]], 
                               plots[[1]],
                               ncol = 6, nrow = 1, position = "bottom")
show(g)
ggsave(filename="maps.png", plot=g)
north2(g,.98, scale = 0.15, symbol = 3.2)
