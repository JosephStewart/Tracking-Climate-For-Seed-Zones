library(dismo);library(data.table);library(dplyr)



Pts = readRDS("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/270m/elevation and seed zone for 270m BCM points.RDS")
Pts = spTransform(Pts, CRS(projection("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))

data = as.data.frame(Pts)

ceiling_500 = function(x) ceiling(x/500)*500
data$el_bnd_mx = ceiling_500(data$elev)
# data$el_bnd = paste0(data$el_bnd_mx -500, " — ", data$el_bnd_mx, "ft")
# e = seq(0, 14500, 500)
# data$el_bnd = factor(data$el_bnd, levels = paste0(e -500, " — ", e, "ft"))

data$elev_m = data$elev / 3.28084 # cover ft to meters
data = data.table(data[,c("sz", "el_bnd_mx",  "x", "y", "elev_m")])


data = subset(data, !is.na(sz)  & ! is.na(el_bnd_mx))



ss = 50 # max samples per elevation band in each sz
df2 <- lapply(split(data, paste0(data$sz, data$el_bnd_mx)),
              function(subdf){if(nrow(subdf)>ss)  subdf <- sample_n(subdf, size = ss); subdf} 
)
data_ss50 = do.call('rbind', df2)
nrow(data_ss50)
table(data_ss50$sz, data_ss50$el_bnd_mx)
saveRDS(data_ss50, "/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/pts with coords from each unit/up to 50 pts with coords from each unit.RDS")
write.csv(data_ss50, "/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/pts with coords from each unit/up to 50 pts with coords from each unit.csv")

ss = 100 # max samples per elevation band in each sz
df100 <- lapply(split(data, paste0(data$sz, data$el_bnd_mx)),
              function(subdf){if(nrow(subdf)>ss)  subdf <- sample_n(subdf, size = ss); subdf} 
)
data_ss100 = do.call('rbind', df100)
nrow(data_ss100)
table(data_ss100$sz, data_ss100$el_bnd_mx)
saveRDS(data_ss100, "/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/pts with coords from each unit/up to 100 pts with coords from each unit.RDS")
write.csv(data_ss100, "/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/pts with coords from each unit/up to 100 pts with coords from each unit.csv")






