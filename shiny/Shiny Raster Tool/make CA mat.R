library(dismo)
library(rgdal)


tmax = raster("/Users/joaaelst/Documents/GIS_Lib/_climate/PRISM/Normals/1981_2010_30yrNormals/tmax/PRISM_tmax_30yr_normal_800mM2_annual_bil.tif")
tmin = raster("/Users/joaaelst/Documents/GIS_Lib/_climate/PRISM/Normals/1981_2010_30yrNormals/tmin/PRISM_tmin_30yr_normal_800mM2_annual_bil.tif")

plot(tmax)



CA_Bound = readOGR("/Users/joaaelst/Documents/GIS_Lib/Boundaries/California_Boundary_2013census/","ca_bound")

CA_Bound = spTransform(CA_Bound, CRS(projection(tmax)))

plot(CA_Bound, add=T)
tmax = crop(tmax, extent(CA_Bound))
tmin = crop(tmin, extent(CA_Bound))

mat = (tmax + tmin)/2

plot(mat)

mat = mask(mat, CA_Bound)
plot(mat)
writeRaster(mat, "/Users/joaaelst/Dropbox/SeedTransferProject/Shiny Tool/V0.01/lib/mat.tif")



