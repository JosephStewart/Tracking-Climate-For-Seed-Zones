data_long_mat = readRDS("lib/data_long_mat.RDS")
data_long_map = readRDS("lib/data_long_map.RDS")
table(paste0(data_long_mat$sz, data_long_mat$el_bnd, data_long_mat$period) == paste0(data_long_map$sz, data_long_map$el_bnd, data_long_map$period)) # all data in same order, ok to combine
data_long = data_long_mat
data_long$map = data_long_map$mat

mat_sd = sd(subset(data_long, period == per_dest)$mat)
log_map_sd = sd(log(subset(data_long, period == per_dest)$map))


# seed destination ####
seedzone = "526"
el = "2500 — 3000ft"
per_dest = "1981-2010"
unique(data_long_mat$el_bnd[data_long_mat$sz == seedzone])

# seed source 
per_source = "1921-1950"
unique(data_long_mat$period)


#
library(MASS)


# multivariate distance

max_dist = .25

library(tidyverse)
clim_by_group = data_long %>% 
  group_by(period, el_bnd, sz) %>%
  summarise(scaled_mat = median(mat)/mat_sd, scaled_log_map = log(median(map))/log_map_sd)

saveRDS(clim_by_group, "/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/shiny/test/reactivedropdown/lib/clim_by_group.RDS")


clim_target = subset(clim_by_group, period == per_dest & el_bnd == el & sz == seedzone)[,c("scaled_mat","scaled_log_map")]


clim_by_group_for_source_period <- subset(clim_by_group, period == per_source)
mat_dif = clim_by_group_for_source_period$scaled_mat - clim_target$scaled_mat 
map_dif = clim_by_group_for_source_period$scaled_log_map - clim_target$scaled_log_map


multi_var_dist = sqrt(mat_dif^2 + map_dif^2)


matches = clim_by_group_for_source_period[multi_var_dist < max_dist, c( "el_bnd",     "sz") ]
names(matches)[2] = "SEED_ZONE"
matches$match = T











library(leaflet);library(rgdal)
seed_zones = readRDS("lib/seed zones disolve wgs84.RDS") # .03 seconds
seed_zones_el = readOGR("/Users/joaaelst/Dropbox/SeedTransferProject/GIS Lib/seedzone and elevation/","cont_sz_CA_wrclm_simp_400m")

ceiling_500 = function(x) ceiling(x/500)*500
el_bnd_mx = ceiling_500(seed_zones_el$ContourMax)
seed_zones_el$el_bnd = paste0(el_bnd_mx -500, " — ", el_bnd_mx, "ft")
seed_zones_el@data <- seed_zones_el@data[,c("el_bnd", "SEED_ZONE")]
saveRDS(seed_zones_el, "/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/shiny/test/reactivedropdown/lib/seedzone_elev_400m_simp.RDS")


matches2 = left_join(seed_zones_el@data, matches,  by = c("el_bnd", "SEED_ZONE"), copy = T )
matches2$match[is.na(matches2$match)] <- F


sz_matches = seed_zones_el[matches2$match,]

sz_matches$label = paste0(sz_matches$SEED_ZONE, ", ", sz_matches$el_bnd)

leaflet() %>% addTiles() %>%
  addPolygons(data = sz_matches, color = "#444444", weight = 1, smoothFactor = 0.5,
opacity = 1.0, fillOpacity = 0,
popup=~label, label= ~label,
highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = TRUE)

)



addPolygons



# old ####



# wilcoxon test

mat_target = subset(data_long_mat, el_bnd == el & period == per_dest & sz == seedzone)$mat
which_match <- subset(data_long_mat, period == per) %>%
  group_by(sz, el_bnd) %>%
  summarize(mean = mean(mat, na.rm = TRUE), median = median(mat, na.rm = TRUE),
            p = wilcox.test(mat_target, mat)$p.value
  )
table(which_match$p > .8)
which_match[which_match$p > .8,]









# multivariate
mat_target = subset(data_long_mat,period == per_dest & el_bnd == el & sz == seedzone)$mat
map_target = subset(data_long_map,period == per_dest & el_bnd == el & sz == seedzone)$mat

mat_non_target = subset(data_long_mat,period == per_dest & (el_bnd != el | sz != seedzone))$mat
map_non_target = subset(data_long_map,period == per_dest & (el_bnd != el | sz != seedzone))$mat


data = data_frame(zone = c(rep("same", length(mat_target)), rep("dif", length(mat_non_target))),
                  mat = c(mat_target, mat_non_target), 
                  map = c(map_target, map_non_target))

plot(data$mat, data$map)
points(data$mat[data$zone == "same"], data$map[data$zone == "same"], col = "purple")


# lda
mod = lda(zone ~ . , data)
p = predict(mod, data)
table(p$class)
head(p$posterior)



kde.result = kde2d(mat_target, map_target)
image(kde.result)
max(kde.result$z  )

targets=round(c(seq(0.05,0.95,0.05),0.99),2)

Dmin <- min(kde.result$z)
Dmax <- max(kde.result$z)
Dmid <- (Dmin + Dmax) / 2

cont.result = contourLines(kde.result$x, kde.result$y,  kde.result$z, levels=c(.002))

