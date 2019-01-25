# library(dismo);library(rgdal);library(ggplot2)
# 
# 
# r = raster("/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/1981_2010/tavg1981_2010_ave_HST.tif")
# 
# ca_bound = readOGR("/Users/joaaelst/Documents/GIS_Lib/Boundaries/California_Boundary_2013census/","ca_bound")
# 
# ca_bound = spTransform(ca_bound, CRS(projection(r)))
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

mat_81_10 = raster("/Users/joaaelst/Documents/GIS_Lib/_climate/BCM_Surfaces_2015/1981_2010/tavg1981_2010_ave_HST.tif")
mat_81_10 = crop(mat_81_10, ca_bound)

data = Pts@data
data$mat_81_10 = values(mat_81_10)


data = data[!is.na(data$sz),]

max(data$elev, na.rm=T)



ceiling_500 = function(x) ceiling(x/500)*500
data$el_bnd_mx = ceiling_500(data$elev)
data$el_bnd = paste0(data$el_bnd_mx -500, " — ", data$el_bnd_mx, "ft")
e = seq(0, 14500, 500)

data$el_bnd = factor(data$el_bnd, levels = paste0(e -500, " — ", e, "ft"))




str(data)

system.time({ 
  sz = "526"
d = data[data$sz == sz,]
str(d)
ggplot(aes(y = mat_81_10, x = el_bnd), data = d) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle(paste("Seed Zone", sz)) + xlab("elevational band") + ylab("mean anual temperature [°C]")  + 
  theme(axis.title = element_text(size = rel(1.5))) +
  theme(axis.text = element_text(size = rel(1.3))) + 
  theme(legend.text = element_text(size = rel(1.3))) +
  theme(legend.title = element_text(size = rel(1.3))) 
})





















