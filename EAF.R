############################### EAF #####################################


if (!require("eaf")) install.packages("eaf")
if (!require("dplyr")) install.packages("dplyr")

if (!require("dplyr")) install.packages("dplyr")
if (!require("data.table")) install.packages("data.table")
if (!require("ggplot2")) install.packages("ggplot2")

df.bootstrap.samples = data.frame()

# pareto-solution of the bootstrap samples
for(i in seq(1:1000)) {
  file = paste0("/data/doutorado-leonildo/data/O3-2019/US/pareto_optimality/pareto_front_US_pollution_", i, ".csv")
  df = as.data.frame(fread(file, sep = ",", dec = "."))
  df['.level'] = paste0("#",df$`low(local.moran.i.cvar) * high(global.moran.i)`)
  df['label'] = paste0(strsplit(df$areal.unit,"km"), " km")
  df['file'] = file
  df.bootstrap.samples = rbind(df.bootstrap.samples, df[df$.level == '#1',])
}

df.bootstrap.samples$global.moran.i = round(df.bootstrap.samples$global.moran.i, 2)
df.bootstrap.samples$local.moran.i.cvar = round(df.bootstrap.samples$local.moran.i.cvar, 2)

# assuming df.bootstrap.samples is your data.frame
y = dplyr::select(df.bootstrap.samples, local.moran.i.cvar,global.moran.i)
#x = as.matrix(sapply(y, as.numeric))
x = matrix(as.numeric(unlist(y)),nrow=length(df.bootstrap.samples$local.moran.i.cvar))
label = dplyr::select(df.bootstrap.samples, label)

print(label)
print(x)

eafplot(y, aes(x = local.moran.i.cvar, y = global.moran.i))