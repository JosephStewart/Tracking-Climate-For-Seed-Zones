library(dplyr)


# mat ####
data = readRDS("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/270m/elevation mat and seed zone for 270m BCM points.RDS")
head(data)

ss = 50 # max samples per elevation band in each sz
df2 <- lapply(split(data, paste0(data$sz, data$el_bnd)),
              function(subdf){if(nrow(subdf)>ss)  subdf <- sample_n(subdf, size = ss); subdf} 
)
data = do.call('rbind', df2)

# data <- data %>% group_by(paste0(sz, el_bnd)) %>% sample_n(size = 50, replace = T)
# data = data[seq(1, nrow(data), 200),] # subsample every 100th value

library(tidyr)
head(data)
data_long <- gather(data, period, mat, mat_1981_2010:mat_2085_cnrm45, factor_key=F)


data_long$period[data_long$period == "mat_1921_1950"] <- "1921-1950"
data_long$period[data_long$period == "mat_1951_1980"] <- "1951-1980"
data_long$period[data_long$period == "mat_1961_1970"] <- "1961-1970"
data_long$period[data_long$period == "mat_1981_2010"] <- "1981-2010"
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

saveRDS(data_long, "/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/270m/data long/ss50/data_long_mat.RDS", compress = F)




### cwd ####
data = readRDS("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/270m/elevation cwd and seed zone for 270m BCM points.RDS")

head(data)

ss = 50 # max samples per elevation band in each sz
df2 <- lapply(split(data, paste0(data$sz, data$el_bnd)),
              function(subdf){if(nrow(subdf)>ss)  subdf <- sample_n(subdf, size = ss); subdf} 
)
data = do.call('rbind', df2)


# data <- data %>% group_by(paste0(sz, el_bnd)) %>% sample_n(size = 50, replace = T)


# data = data[seq(1, nrow(data), 200),] # subsample every 200th value

library(tidyr)
head(data)
data_long <- gather(data, period, mat, cwd_1981_2010:cwd_2085_cnrm45, factor_key=F)


data_long$period[data_long$period == "cwd_1921_1950"] <- "1921-1950"
data_long$period[data_long$period == "cwd_1951_1980"] <- "1951-1980"
data_long$period[data_long$period == "cwd_1961_1970"] <- "1961-1970"
data_long$period[data_long$period == "cwd_1981_2010"] <- "1981-2010"
data_long$period[data_long$period == "cwd_2009_2018"] <- "2009-2018"


data_long$period[data_long$period == "cwd_2025_miroc85"] <- "2010-2039 HDHE"
data_long$period[data_long$period == "cwd_2055_miroc85"] <- "2040-2069 HDHE"
data_long$period[data_long$period == "cwd_2085_miroc85"] <- "2070-2099 HDHE"

data_long$period[data_long$period == "cwd_2025_miroc45"] <- "2010-2039 HDLE"
data_long$period[data_long$period == "cwd_2055_miroc45"] <- "2040-2069 HDLE"
data_long$period[data_long$period == "cwd_2085_miroc45"] <- "2070-2099 HDLE"

data_long$period[data_long$period == "cwd_2025_cnrm85"] <- "2010-2039 WWHE"
data_long$period[data_long$period == "cwd_2055_cnrm85"] <- "2040-2069 WWHE"
data_long$period[data_long$period == "cwd_2085_cnrm85"] <- "2070-2099 WWHE"

data_long$period[data_long$period == "cwd_2025_cnrm45"] <- "2010-2039 WWLE"
data_long$period[data_long$period == "cwd_2055_cnrm45"] <- "2040-2069 WWLE"
data_long$period[data_long$period == "cwd_2085_cnrm45"] <- "2070-2099 WWLE"


saveRDS(data_long, "/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/270m/data long/ss50/data_long_cwd.RDS", compress = F)

### aet ####
data = readRDS("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/270m/elevation aet and seed zone for 270m BCM points.RDS")

head(data)

ss = 50 # max samples per elevation band in each sz
df2 <- lapply(split(data, paste0(data$sz, data$el_bnd)),
              function(subdf){if(nrow(subdf)>ss)  subdf <- sample_n(subdf, size = ss); subdf} 
)
data = do.call('rbind', df2)


# data <- data %>% group_by(paste0(sz, el_bnd)) %>% sample_n(size = 50, replace = T)


# data = data[seq(1, nrow(data), 200),] # subsample every 200th value

library(tidyr)
head(data)
data_long <- gather(data, period, mat, aet_1981_2010:aet_2085_cnrm45, factor_key=F)


data_long$period[data_long$period == "aet_1921_1950"] <- "1921-1950"
data_long$period[data_long$period == "aet_1951_1980"] <- "1951-1980"
data_long$period[data_long$period == "aet_1961_1970"] <- "1961-1970"
data_long$period[data_long$period == "aet_1981_2010"] <- "1981-2010"
data_long$period[data_long$period == "aet_2009_2018"] <- "2009-2018"


data_long$period[data_long$period == "aet_2025_miroc85"] <- "2010-2039 HDHE"
data_long$period[data_long$period == "aet_2055_miroc85"] <- "2040-2069 HDHE"
data_long$period[data_long$period == "aet_2085_miroc85"] <- "2070-2099 HDHE"

data_long$period[data_long$period == "aet_2025_miroc45"] <- "2010-2039 HDLE"
data_long$period[data_long$period == "aet_2055_miroc45"] <- "2040-2069 HDLE"
data_long$period[data_long$period == "aet_2085_miroc45"] <- "2070-2099 HDLE"

data_long$period[data_long$period == "aet_2025_cnrm85"] <- "2010-2039 WWHE"
data_long$period[data_long$period == "aet_2055_cnrm85"] <- "2040-2069 WWHE"
data_long$period[data_long$period == "aet_2085_cnrm85"] <- "2070-2099 WWHE"

data_long$period[data_long$period == "aet_2025_cnrm45"] <- "2010-2039 WWLE"
data_long$period[data_long$period == "aet_2055_cnrm45"] <- "2040-2069 WWLE"
data_long$period[data_long$period == "aet_2085_cnrm45"] <- "2070-2099 WWLE"


saveRDS(data_long, "/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/270m/data long/ss50/data_long_aet.RDS", compress = F)

### map ####
data = readRDS("/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/270m/elevation map and seed zone for 270m BCM points.RDS")

head(data)

ss = 50 # max samples per elevation band in each sz
df2 <- lapply(split(data, paste0(data$sz, data$el_bnd)),
              function(subdf){if(nrow(subdf)>ss)  subdf <- sample_n(subdf, size = ss); subdf} 
)
data = do.call('rbind', df2)


# data <- data %>% group_by(paste0(sz, el_bnd)) %>% sample_n(size = 50, replace = T)


# data = data[seq(1, nrow(data), 200),] # subsample every 200th value

library(tidyr)
head(data)
data_long <- gather(data, period, mat, map_1981_2010:map_2085_cnrm45, factor_key=F)


data_long$period[data_long$period == "map_1921_1950"] <- "1921-1950"
data_long$period[data_long$period == "map_1951_1980"] <- "1951-1980"
data_long$period[data_long$period == "map_1961_1970"] <- "1961-1970"
data_long$period[data_long$period == "map_1981_2010"] <- "1981-2010"
data_long$period[data_long$period == "map_2009_2018"] <- "2009-2018"


data_long$period[data_long$period == "map_2025_miroc85"] <- "2010-2039 HDHE"
data_long$period[data_long$period == "map_2055_miroc85"] <- "2040-2069 HDHE"
data_long$period[data_long$period == "map_2085_miroc85"] <- "2070-2099 HDHE"

data_long$period[data_long$period == "map_2025_miroc45"] <- "2010-2039 HDLE"
data_long$period[data_long$period == "map_2055_miroc45"] <- "2040-2069 HDLE"
data_long$period[data_long$period == "map_2085_miroc45"] <- "2070-2099 HDLE"

data_long$period[data_long$period == "map_2025_cnrm85"] <- "2010-2039 WWHE"
data_long$period[data_long$period == "map_2055_cnrm85"] <- "2040-2069 WWHE"
data_long$period[data_long$period == "map_2085_cnrm85"] <- "2070-2099 WWHE"

data_long$period[data_long$period == "map_2025_cnrm45"] <- "2010-2039 WWLE"
data_long$period[data_long$period == "map_2055_cnrm45"] <- "2040-2069 WWLE"
data_long$period[data_long$period == "map_2085_cnrm45"] <- "2070-2099 WWLE"


saveRDS(data_long, "/Users/joaaelst/Dropbox/SeedTransferProject/Tracking Climate For Seed Zones/270m/data long/ss50/data_long_map.RDS", compress = F)

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


table(d$el_bnd)
?sample
