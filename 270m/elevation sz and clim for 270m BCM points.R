library(dismo);library(rgdal);library(ggplot2);library(data.table)
# 
# 
r = raster("/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/1981_2010/tavg1981_2010_ave_HST.tif")

ca_bound = readOGR("/Users/joaaelst/Documents/GIS_Lib/Boundaries/California_Boundary_2013census/","ca_bound")

ca_bound = spTransform(ca_bound, CRS(projection(r)))
# r = crop(r, ca_bound)
# values(r) = 1
# 
# Pts = rasterToPoints(r, spatial=T)  # NA values are skipped, so make no values NA
# Pts$elev = extract(elev_r, Pts) # takes a few minutes
# 
# # saveRDS(Pts,"/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/270m/elevation for 270m BCM points.RDS")
# Pts = readRDS("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/270m/elevation for 270m BCM points.RDS")
# 
# Pts@data = data.frame(elev = Pts@data$elev)
# Pts@data$elev[Pts@data$elev < -5e37] = NA
# Pts@data$elev = round(Pts@data$elev* 3.28084) # convert m to ft
# 
# 
# 
# 
# 
# values(r) = Pts@data$elev
# 
# plot(r)
# plot(ca_bound,add=T)
# 
# 
# sz = readOGR("/Users/joaaelst/Dropbox/SeedTransferProject/GIS Lib/Seed zones/","seed zones")
# sz = spTransform(sz, CRS(projection(r)))
# sz@data = data.frame(SEED_ZONE = sz@data$SEED_ZONE)
# 
# o <- Pts %over% sz
# str(o)
# Pts@data$sz = o$SEED_ZONE
# 
# str(Pts)
# saveRDS(Pts,"/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/270m/elevation and seed zone for 270m BCM points.RDS")
Pts = readRDS("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/270m/elevation and seed zone for 270m BCM points.RDS")
data = Pts@data

ceiling_500 = function(x) ceiling(x/500)*500
data$el_bnd_mx = ceiling_500(data$elev)
data$el_bnd = paste0(data$el_bnd_mx -500, " — ", data$el_bnd_mx, "ft")
e = seq(0, 14500, 500)
data$el_bnd = factor(data$el_bnd, levels = paste0(e -500, " — ", e, "ft"))
data = data.table(data[,c("sz","el_bnd")])



# Hist Clim
mat_81_10 = raster("/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/1981_2010/tavg1981_2010_ave_HST.tif")
mat_81_10 = crop(mat_81_10, ca_bound)
data$mat_81_10 = values(mat_81_10)

mat_1921_1950 = stack("/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Averages_10y_30y/tmn1921_1950_ave_HST.tif", "/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Averages_10y_30y/tmx1921_1950_ave_HST.tif")
mat_1921_1950 = mean(mat_1921_1950)
mat_1921_1950 = crop(mat_1921_1950, ca_bound)
data$mat_1921_1950 = values(mat_1921_1950)
# plot(mat_1921_1950)

mat_1951_1980 = stack("/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Averages_10y_30y/tmn1951_1980_ave_HST.tif", "/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Averages_10y_30y/tmx1951_1980_ave_HST.tif")
mat_1951_1980 = mean(mat_1951_1980)
mat_1951_1980 = crop(mat_1951_1980, ca_bound)
data$mat_1951_1980 = values(mat_1951_1980)
plot(mat_1951_1980)

mat_1961_1970 = stack("/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Averages_10y_30y/tmn1961_1970_ave_HST.tif", "/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Averages_10y_30y/tmx1961_1970_ave_HST.tif")
mat_1961_1970 = mean(mat_1961_1970)
mat_1961_1970 = crop(mat_1961_1970, ca_bound)
data$mat_1961_1970 = values(mat_1961_1970)
plot(mat_1961_1970)

mat_2009_2018 = stack("/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Averages_10y_30y/tmn2009_2018_ave_HST.tif", "/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Averages_10y_30y/tmx2009_2018_ave_HST.tif")
mat_2009_2018 = mean(mat_2009_2018)
mat_2009_2018 = crop(mat_2009_2018, ca_bound)
data$mat_2009_2018 = values(mat_2009_2018)
plot(mat_2009_2018)


# future scenarios miroc85 ####

mat_2025_miroc85 = stack("/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/miroc_esm/rcp85_2010_2039/tmn2010_2039_ave_miroc_esm_rcp85.tif", "/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/miroc_esm/rcp85_2010_2039/tmx2010_2039_ave_miroc_esm_rcp85.tif")
mat_2025_miroc85 = mean(mat_2025_miroc85)
mat_2025_miroc85 = crop(mat_2025_miroc85, ca_bound)
data$mat_2025_miroc85 = values(mat_2025_miroc85)


mat_2055_miroc85 = stack("/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/miroc_esm/rcp85_2040_2069/tmn2040_2069_ave_miroc_esm_rcp85.tif", "/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/miroc_esm/rcp85_2040_2069/tmx2040_2069_ave_miroc_esm_rcp85.tif")
mat_2055_miroc85 = mean(mat_2055_miroc85)
mat_2055_miroc85 = crop(mat_2055_miroc85, ca_bound)
data$mat_2055_miroc85 = values(mat_2055_miroc85)


mat_2085_miroc85 = stack("/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/miroc_esm/rcp85_2070_2099/tmn2070_2099_ave_miroc_esm_rcp85.tif", "/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/miroc_esm/rcp85_2070_2099/tmx2070_2099_ave_miroc_esm_rcp85.tif")
mat_2085_miroc85 = mean(mat_2085_miroc85)
mat_2085_miroc85 = crop(mat_2085_miroc85, ca_bound)
data$mat_2085_miroc85 = values(mat_2085_miroc85)


# future scenarios miroc45 ####

mat_2025_miroc45 = stack("/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/miroc_esm/rcp45_2010_2039/tmn2010_2039_ave_miroc_esm_rcp45.tif", "/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/miroc_esm/rcp45_2010_2039/tmx2010_2039_ave_miroc_esm_rcp45.tif")
mat_2025_miroc45 = mean(mat_2025_miroc45)
mat_2025_miroc45 = crop(mat_2025_miroc45, ca_bound)
data$mat_2025_miroc45 = values(mat_2025_miroc45)


mat_2055_miroc45 = stack("/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/miroc_esm/rcp45_2040_2069/tmn2040_2069_ave_miroc_esm_rcp45.tif", "/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/miroc_esm/rcp45_2040_2069/tmx2040_2069_ave_miroc_esm_rcp45.tif")
mat_2055_miroc45 = mean(mat_2055_miroc45)
mat_2055_miroc45 = crop(mat_2055_miroc45, ca_bound)
data$mat_2055_miroc45 = values(mat_2055_miroc45)


mat_2085_miroc45 = stack("/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/miroc_esm/rcp45_2070_2099/tmn2070_2099_ave_miroc_esm_rcp45.tif", "/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/miroc_esm/rcp45_2070_2099/tmx2070_2099_ave_miroc_esm_rcp45.tif")
mat_2085_miroc45 = mean(mat_2085_miroc45)
mat_2085_miroc45 = crop(mat_2085_miroc45, ca_bound)
data$mat_2085_miroc45 = values(mat_2085_miroc45)

# future scenarios cnrm85 ####

mat_2025_cnrm85 = stack("/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/cnrm_cm5/rcp85_2010_2039/tmn2010_2039_ave_cnrm_cm5_rcp85.tif", "/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/cnrm_cm5/rcp85_2010_2039/tmx2010_2039_ave_cnrm_cm5_rcp85.tif")
mat_2025_cnrm85 = mean(mat_2025_cnrm85)
mat_2025_cnrm85 = crop(mat_2025_cnrm85, ca_bound)
data$mat_2025_cnrm85 = values(mat_2025_cnrm85)


mat_2055_cnrm85 = stack("/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/cnrm_cm5/rcp85_2040_2069/tmn2040_2069_ave_cnrm_cm5_rcp85.tif", "/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/cnrm_cm5/rcp85_2040_2069/tmx2040_2069_ave_cnrm_cm5_rcp85.tif")
mat_2055_cnrm85 = mean(mat_2055_cnrm85)
mat_2055_cnrm85 = crop(mat_2055_cnrm85, ca_bound)
data$mat_2055_cnrm85 = values(mat_2055_cnrm85)


mat_2085_cnrm85 = stack("/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/cnrm_cm5/rcp85_2070_2099/tmn2070_2099_ave_cnrm_cm5_rcp85.tif", "/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/cnrm_cm5/rcp85_2070_2099/tmx2070_2099_ave_cnrm_cm5_rcp85.tif")
mat_2085_cnrm85 = mean(mat_2085_cnrm85)
mat_2085_cnrm85 = crop(mat_2085_cnrm85, ca_bound)
data$mat_2085_cnrm85 = values(mat_2085_cnrm85)

# future scenarios cnrm45 ####

mat_2025_cnrm45 = stack("/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/cnrm_cm5/rcp45_2010_2039/tmn2010_2039_ave_cnrm_cm5_rcp45.tif", "/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/cnrm_cm5/rcp45_2010_2039/tmx2010_2039_ave_cnrm_cm5_rcp45.tif")
mat_2025_cnrm45 = mean(mat_2025_cnrm45)
mat_2025_cnrm45 = crop(mat_2025_cnrm45, ca_bound)
data$mat_2025_cnrm45 = values(mat_2025_cnrm45)


mat_2055_cnrm45 = stack("/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/cnrm_cm5/rcp45_2040_2069/tmn2040_2069_ave_cnrm_cm5_rcp45.tif", "/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/cnrm_cm5/rcp45_2040_2069/tmx2040_2069_ave_cnrm_cm5_rcp45.tif")
mat_2055_cnrm45 = mean(mat_2055_cnrm45)
mat_2055_cnrm45 = crop(mat_2055_cnrm45, ca_bound)
data$mat_2055_cnrm45 = values(mat_2055_cnrm45)


mat_2085_cnrm45 = stack("/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/cnrm_cm5/rcp45_2070_2099/tmn2070_2099_ave_cnrm_cm5_rcp45.tif", "/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/Future/cnrm_cm5/rcp45_2070_2099/tmx2070_2099_ave_cnrm_cm5_rcp45.tif")
mat_2085_cnrm45 = mean(mat_2085_cnrm45)
mat_2085_cnrm45 = crop(mat_2085_cnrm45, ca_bound)
data$mat_2085_cnrm45 = values(mat_2085_cnrm45)





data = data[!is.na(data$sz) & !is.na(data$mat_2025_miroc85),]   # subset

data = data[seq(1, nrow(data), 10),] # use only every 10th value



library(tidyr)
head(data)
data_long <- gather(data, period, mat, mat_81_10:mat_2085_cnrm45, factor_key=F)


data_long$period[data_long$period == "mat_1921_1950"] <- "1921-1950"
data_long$period[data_long$period == "mat_1951_1980"] <- "1951-1980"
data_long$period[data_long$period == "mat_1961_1970"] <- "1961-1970"
data_long$period[data_long$period == "mat_81_10"] <- "1981-2010"
data_long$period[data_long$period == "mat_2009_2018"] <- "2009-2018"


data_long$period[data_long$period == "mat_2025_miroc85"] <- "2010-2039 HDHE"
data_long$period[data_long$period == "mat_2055_miroc85"] <- "2040-2069 HDHE"
data_long$period[data_long$period == "mat_2085_miroc85"] <- "2070-2099 HDHE"

data_long$period[data_long$period == "mat_2025_miroc45"] <- "2010-2039 HDLE"
data_long$period[data_long$period == "mat_2055_miroc45"] <- "2040-2069 HDLE"
data_long$period[data_long$period == "mat_2085_miroc45"] <- "2070-2099 HDLE"

data_long$period[data_long$period == "mat_2025_cnrm85"] <- "2010-2039 WWHE"
data_long$period[data_long$period == "mat_2055_cnrm85"] <- "2040-2069 WWHE"
data_long$period[data_long$period == "mat_2085_cnrm85"] <- "2070-2099 WWHE"

data_long$period[data_long$period == "mat_2025_cnrm45"] <- "2010-2039 WWLE"
data_long$period[data_long$period == "mat_2055_cnrm45"] <- "2040-2069 WWLE"
data_long$period[data_long$period == "mat_2085_cnrm45"] <- "2070-2099 WWLE"




unique(data_long$period)




class(data_long)
str(data_long)



saveRDS(data_long, "/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/shiny/CA_Seed_Zone_CC_BCM/lib/data_long_2.RDS")


head(data_long)

system.time({ 
  scenarios = c("1961-1970", "2009-2018")
  d = data_long[data_long$period %in% scenarios,]
  sz = "526"
  d = d[d$sz == sz,]
  str(d)
  ggplot(aes(y = mat, x = el_bnd, fill = period), data = d) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle(paste("Seed Zone", sz)) + xlab("elevational band") + ylab("mean anual temperature [°C]")  + 
    theme(axis.title = element_text(size = rel(1.5))) +
    theme(axis.text = element_text(size = rel(1.3))) + 
    theme(legend.text = element_text(size = rel(1.3))) +
    theme(legend.title = element_text(size = rel(1.3))) 
})





table(d$el_bnd)

























