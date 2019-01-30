library(dismo);library(rgdal);library(ggplot2)
zones = readOGR("/Users/joaaelst/Dropbox/SeedTransferProject/GIS Lib/Seed Zone Contour Union/", "CA_countours_seedz_disolve")

#### 1961:1970 ####
yr = 1961:1970
mat1970 = stack(paste0("/Users/joaaelst/Documents/GIS_Lib/_climate/PRISM/Monthly_CAI/PRISM_monthly_CAI/PRISM_tmean_stable_4kmM2_",yr,"_all_bil/PRISM_tmean_stable_4kmM2_",yr,"_bil.bil"))


mat1970 = crop(mat1970, extent(zones))

mat1970 = mean(mat1970)

plot(mat1970)
mat1970_extr = extract(mat1970, zones, small = T)

sz = mapply(rep, zones$SEED_ZONE, lapply(mat1970_extr, length))
el = mapply(rep, zones$max_elev, lapply(mat1970_extr, length))

mat1970_df = data.frame(SEED_ZONE = unlist(sz), el_bnd_mx = unlist(el), mat = unlist(mat1970_extr))




# 2008:2017 ####
yr = 2008:2017
mat2017 = stack(paste0("/Users/joaaelst/Documents/GIS_Lib/_climate/PRISM/Monthly_CAI/PRISM_monthly_CAI/PRISM_tmean_stable_4kmM2_",yr,"_all_bil/PRISM_tmean_stable_4kmM2_",yr,"_bil.bil"))

mat2017 = crop(mat2017, extent(zones))

mat2017 = mean(mat2017)

plot(mat2017)
mat2017_extr = extract(mat2017, zones, small = T)

sz = mapply(rep, zones$SEED_ZONE, lapply(mat2017_extr, length))
el = mapply(rep, zones$max_elev, lapply(mat2017_extr, length))

mat2017_df = data.frame(SEED_ZONE = unlist(sz), el_bnd_mx = unlist(el), mat = unlist(mat2017_extr))



boxplot(mat ~ SEED_ZONE, mat1970_df)

boxplot(mat ~ el_bnd_mx, subset(mat1970_df, SEED_ZONE == "526"))


# combine 1970 and 2017 df
mat2017_df$year = "2017"
mat1970_df$year = "1970"
zone_mat_df = rbind(mat1970_df, mat2017_df)


ceiling_500 = function(x) ceiling(x/500)*500
ceiling_500(c(1,2))
zone_mat_df$el_bnd_mx = ceiling_500(as.numeric(as.character(zone_mat_df$el_bnd_mx)))
zone_mat_df$el_bnd = paste0(zone_mat_df$el_bnd_mx -500, " — ", zone_mat_df$el_bnd_mx, "ft")
e = seq(0, 14000, 500)
zone_mat_df$el_bnd = factor(zone_mat_df$el_bnd, levels = paste0(e -500, " — ", e, "ft"))

write.csv(zone_mat_df ,"/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/shiny/CA_Seed_Zone_CC/lib/seedzone_elev_band_mat_extract_10yma.csv")


head(zone_mat_df)

ggplot(aes(y = mat, x = el_bnd, fill = year), data = subset(zone_mat_df, SEED_ZONE == "526")) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Seed Zone 526") + xlab("elevational band") + ylab("mean anual temperature [°C]") + 
  theme(axis.title.y = element_text(size = rel(1.5), angle = 90)) +
  theme(axis.title.x = element_text(size = rel(1.5), angle = 0))







#### 2055 ####

mat2055 = raster("/Users/joaaelst/Documents/GIS_Lib/_climate/ClimateNA/California/4km based on PRISM/MAT_15GCM-Ensemble_rcp85_2055Y.tif")


mat2055 = crop(mat2055, extent(zones))

mat2055 = mean(mat2055)

plot(mat2055)
mat2055_extr = extract(mat2055, zones, small = T)

sz = mapply(rep, zones$SEED_ZONE, lapply(mat2055_extr, length))
el = mapply(rep, zones$max_elev, lapply(mat2055_extr, length))

mat2055_df = data.frame(SEED_ZONE = unlist(sz), el_bnd_mx = unlist(el), mat = unlist(mat2055_extr))


#### 2025 ####

mat2025 = raster("/Users/joaaelst/Documents/GIS_Lib/_climate/ClimateNA/California/4km based on PRISM/MAT_15GCM-Ensemble_rcp85_2025Y.tif")


mat2025 = crop(mat2025, extent(zones))

mat2025 = mean(mat2025)

plot(mat2025)
mat2025_extr = extract(mat2025, zones, small = T)

sz = mapply(rep, zones$SEED_ZONE, lapply(mat2025_extr, length))
el = mapply(rep, zones$max_elev, lapply(mat2025_extr, length))

mat2025_df = data.frame(SEED_ZONE = unlist(sz), el_bnd_mx = unlist(el), mat = unlist(mat2025_extr))



#### 2085 ####

mat2085 = raster("/Users/joaaelst/Documents/GIS_Lib/_climate/ClimateNA/California/4km based on PRISM/MAT_15GCM-Ensemble_rcp85_2085Y.tif")


mat2085 = crop(mat2085, extent(zones))

mat2085 = mean(mat2085)

plot(mat2085)
mat2085_extr = extract(mat2085, zones, small = T)

sz = mapply(rep, zones$SEED_ZONE, lapply(mat2085_extr, length))
el = mapply(rep, zones$max_elev, lapply(mat2085_extr, length))

mat2085_df = data.frame(SEED_ZONE = unlist(sz), el_bnd_mx = unlist(el), mat = unlist(mat2085_extr))

# combine future ensemble projections
mat2085_df$year = "2085"
mat2055_df$year = "2055"
mat2025_df$year = "2025"

zone_mat_df_fut_ens = rbind(mat2025_df, mat2055_df)
zone_mat_df_fut_ens = rbind(zone_mat_df_fut_ens, mat2085_df)


ceiling_500 = function(x) ceiling(x/500)*500
ceiling_500(c(1,2))
zone_mat_df_fut_ens$el_bnd_mx = ceiling_500(as.numeric(as.character(zone_mat_df_fut_ens$el_bnd_mx)))
zone_mat_df_fut_ens$el_bnd = paste0(zone_mat_df_fut_ens$el_bnd_mx -500, " — ", zone_mat_df_fut_ens$el_bnd_mx, "ft")
e = seq(0, 14000, 500)
zone_mat_df_fut_ens$el_bnd = factor(zone_mat_df_fut_ens$el_bnd, levels = paste0(e -500, " — ", e, "ft"))

write.csv(zone_mat_df_fut_ens ,"/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/shiny/CA_Seed_Zone_CC/lib/seedzone_elev_band_mat_extract_fut_ensemble.csv")



# combine historical and future 
zone_mat_df = read.csv("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/shiny/CA_Seed_Zone_CC/lib/seedzone_elev_band_mat_extract_10yma.csv")

zone_mat_df_fut_ens = read.csv("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/shiny/CA_Seed_Zone_CC/lib/seedzone_elev_band_mat_extract_fut_ensemble.csv")


df = rbind(zone_mat_df, zone_mat_df_fut_ens)

df$period = NA

table(df$year)
df$period[df$year == 1970] = "1961-1970"
df$period[df$year == 2017] = "2008-2017"
df$period[df$year == 2025] = "2011-2040"
df$period[df$year == 2055] = "2041-2070"
df$period[df$year == 2085] = "2071-2100"

write.csv(df, "/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/shiny/CA_Seed_Zone_CC/lib/seedzone_elev_band_mat_extract_hist_fut.csv")



## PPT ####

#### 2055 ####

mat2055 = raster("/Users/joaaelst/Documents/GIS_Lib/_climate/ClimateNA/California/4km based on PRISM/MAT_15GCM-Ensemble_rcp85_2055Y.tif")


mat2055 = crop(mat2055, extent(zones))

mat2055 = mean(mat2055)

plot(mat2055)
mat2055_extr = extract(mat2055, zones, small = T)

sz = mapply(rep, zones$SEED_ZONE, lapply(mat2055_extr, length))
el = mapply(rep, zones$max_elev, lapply(mat2055_extr, length))

mat2055_df = data.frame(SEED_ZONE = unlist(sz), el_bnd_mx = unlist(el), mat = unlist(mat2055_extr))


