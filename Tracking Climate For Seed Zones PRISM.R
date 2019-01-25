library(dismo);library(rgdal);library(ggplot2)
zones = readOGR("/Users/joaaelst/Dropbox/SeedTransferProject/GIS Lib/Seed Zone Contour Union/", "CA_countours_seedz_disolve")


yr = 1961:1970
mat1970 = stack(paste0("/Users/joaaelst/Documents/GIS_Lib/_climate/PRISM/Monthly_CAI/PRISM_monthly_CAI/PRISM_tmean_stable_4kmM2_",yr,"_all_bil/PRISM_tmean_stable_4kmM2_",yr,"_bil.bil"))

mat1970 = crop(mat1970, extent(zones))

mat1970 = mean(mat1970)

plot(mat1970)
mat1970_extr = extract(mat1970, zones, small = T)

sz = mapply(rep, zones$SEED_ZONE, lapply(mat1970_extr, length))
el = mapply(rep, zones$max_elev, lapply(mat1970_extr, length))

mat1970_df = data.frame(SEED_ZONE = unlist(sz), el_bnd_mx = unlist(el), mat = unlist(mat1970_extr))





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















