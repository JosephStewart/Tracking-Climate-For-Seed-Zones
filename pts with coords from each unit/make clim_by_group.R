library(tidyverse)
data = read.csv("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/pts with coords from each unit/climateNA extract/up to 50 pts with coords from each unit_Normal_1981_2010Y.csv")
data$period = "1981-2010"


# try with just cur period

data$el_bnd = paste0(data$ID2 -500, " — ", data$ID2, "ft")
e = seq(0, 14500, 500)
data$el_bnd = factor(data$el_bnd, levels = paste0(e -500, " — ", e, "ft"))

# vars: MAT, PPT, Temperature Seasonality, Lat

data = data[,c("ID1", "Latitude", "MAT", "TD", "MAP", "el_bnd", "period")]
names(data)[1] = "sz"
mat_sd = sd(data$MAT)
log_map_sd = sd(log(data$MAP))
TD_sd = sd(data$TD)
lat_sd = sd(data$Latitude)


clim_by_group = data %>% 
  group_by(el_bnd, sz) %>%
  summarise(scaled_mat = median(MAT)/mat_sd, 
            scaled_log_map = log(median(MAP))/log_map_sd, 
            scaled_td = median(TD)/TD_sd, 
            scaled_lat =  median(Latitude)/lat_sd
            )

unique(paste0(clim_by_group$el_bnd, clim_by_group$sz))
unique(paste0(data$el_bnd, data$sz))



# aggegate all periods/scenarios
data = read.csv("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/pts with coords from each unit/climateNA extract/up to 50 pts with coords from each unit_Normal_1981_2010Y.csv")
data$period = "1981-2010"
data = data[,c("ID1", "ID2", "Latitude", "MAT", "TD", "MAP", "period", "Elevation")]

d2 = read.csv("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/pts with coords from each unit/climateNA extract/up to 50 pts with coords from each unit_Normal_1901_1930Y.csv")
d2$period = "1901-1930"
d2 = d2[,c("ID1", "ID2", "Latitude", "MAT", "TD", "MAP", "period", "Elevation")]
data = rbind(data, d2)

d2 = read.csv("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/pts with coords from each unit/climateNA extract/up to 50 pts with coords from each unit_Normal_1941_1970Y.csv")
d2$period = "1941-1970"
d2 = d2[,c("ID1", "ID2", "Latitude", "MAT", "TD", "MAP", "period", "Elevation")]
data = rbind(data, d2)

#fut ensemble
d2 = read.csv("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/pts with coords from each unit/climateNA extract/up to 50 pts with coords from each unit_15GCM-Ensemble_rcp85_2025Y.csv")
d2$period = "2010-2039 ENS HE"
d2 = d2[,c("ID1", "ID2", "Latitude", "MAT", "TD", "MAP", "period", "Elevation")]
data = rbind(data, d2)

d2 = read.csv("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/pts with coords from each unit/climateNA extract/up to 50 pts with coords from each unit_15GCM-Ensemble_rcp85_2055Y.csv")
d2$period = "2040-2069 ENS HE"
d2 = d2[,c("ID1", "ID2", "Latitude", "MAT", "TD", "MAP", "period", "Elevation")]
data = rbind(data, d2)

d2 = read.csv("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/pts with coords from each unit/climateNA extract/up to 50 pts with coords from each unit_15GCM-Ensemble_rcp85_2085Y.csv")
d2$period = "2070-2099 ENS HE"
d2 = d2[,c("ID1", "ID2", "Latitude", "MAT", "TD", "MAP", "period", "Elevation")]
data = rbind(data, d2)

# miroc
d2 = read.csv("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/pts with coords from each unit/climateNA extract/up to 50 pts with coords from each unit_MIROC-ESM_rcp85_2025Y.csv")
d2$period = "2010-2039 HD HE"
d2 = d2[,c("ID1", "ID2", "Latitude", "MAT", "TD", "MAP", "period", "Elevation")]
data = rbind(data, d2)

d2 = read.csv("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/pts with coords from each unit/climateNA extract/up to 50 pts with coords from each unit_MIROC-ESM_rcp85_2055Y.csv")
d2$period = "2040-2069 HD HE"
d2 = d2[,c("ID1", "ID2", "Latitude", "MAT", "TD", "MAP", "period", "Elevation")]
data = rbind(data, d2)

d2 = read.csv("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/pts with coords from each unit/climateNA extract/up to 50 pts with coords from each unit_MIROC-ESM_rcp85_2085Y.csv")
d2$period = "2070-2099 HD HE"
d2 = d2[,c("ID1", "ID2", "Latitude", "MAT", "TD", "MAP", "period", "Elevation")]
data = rbind(data, d2)

# cnrm
d2 = read.csv("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/pts with coords from each unit/climateNA extract/up to 50 pts with coords from each unit_CNRM-CM5_rcp85_2025Y.csv")
d2$period = "2010-2039 WW HE"
d2 = d2[,c("ID1", "ID2", "Latitude", "MAT", "TD", "MAP", "period", "Elevation")]
data = rbind(data, d2)

d2 = read.csv("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/pts with coords from each unit/climateNA extract/up to 50 pts with coords from each unit_CNRM-CM5_rcp85_2055Y.csv")
d2$period = "2040-2069 WW HE"
d2 = d2[,c("ID1", "ID2", "Latitude", "MAT", "TD", "MAP", "period", "Elevation")]
data = rbind(data, d2)

d2 = read.csv("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/pts with coords from each unit/climateNA extract/up to 50 pts with coords from each unit_CNRM-CM5_rcp85_2085Y.csv")
d2$period = "2070-2099 WW HE"
d2 = d2[,c("ID1", "ID2", "Latitude", "MAT", "TD", "MAP", "period", "Elevation")]
data = rbind(data, d2)


# agregate by group
data$el_bnd = paste0(data$ID2 -500, " — ", data$ID2, "ft")
e = seq(0, 14500, 500)
data$el_bnd = factor(data$el_bnd, levels = paste0(e -500, " — ", e, "ft"))

data = data[,c("ID1", "Latitude", "MAT", "TD", "MAP", "el_bnd", "period")]
names(data)[1] = "sz"
mat_sd = sd(data$MAT[data$period == "1981-2010"])
log_map_sd = sd(log(data$MAP[data$period == "1981-2010"]))
TD_sd = sd(data$TD[data$period == "1981-2010"])
lat_sd = sd(data$Latitude[data$period == "1981-2010"])




clim_by_group2 = data %>% 
  group_by(period, el_bnd, sz) %>%
  summarise(scaled_mat = median(MAT)/mat_sd, 
            scaled_log_map = log(median(MAP))/log_map_sd, 
            scaled_td = median(TD)/TD_sd, 
            scaled_lat =  median(Latitude)/lat_sd
  )

library(stringr)



clim_by_group2$sz = factor(str_pad(clim_by_group2$sz, 3, pad = "0"))
# clim_by_group2$period = factor(clim_by_group2$period)
# clim_by_group = readRDS("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/shiny/SeedZoneClimateTracker/lib/clim_by_group.RDS")
# 
# str(clim_by_group)
# str(clim_by_group2) 
# 
# unique(paste0(clim_by_group2$el_bnd, clim_by_group2$sz))



saveRDS(clim_by_group2, "/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/pts with coords from each unit/climateNA extract/clim_by_group.RDS")

hist(clim_by_group$scaled_lat)
hist(clim_by_group$scaled_mat)
hist(clim_by_group$scaled_log_map)
hist(clim_by_group$scaled_td)








# with period
clim_by_group = data %>% 
  group_by(period, el_bnd, sz) %>%
  summarise(scaled_mat = median(mat)/mat_sd, scaled_log_map = log(median(map))/log_map_sd)





