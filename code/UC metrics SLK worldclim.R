library(raster)
library(sf)

tempdata = stack()
raindata = stack()

for (i in 1:12) {
  tempdata <- addLayer(tempdata, paste("/Users/ej/CFFRC/04-Research/hemp slk/in/wc0.5_28/tmean",i ,"_28.bil", sep = ""))
  raindata <- addLayer(raindata, paste("/Users/ej/CFFRC/04-Research/hemp slk/in/wc0.5_28/prec",i ,"_28.bil", sep = ""))
}

slkbnd <- st_read("./in/gadm41_LKA_shp/gadm41_LKA_0.shp")

tempdata = mask(crop(tempdata,extent(slkbnd)), slkbnd)
raindata = mask(crop(raindata,extent(slkbnd)), slkbnd)

outpath = "/Users/ej/CFFRC/04-Research/UC metrics/UCMetrics-SLK/in/worldclim/"
for (i in 1:12){
  writeRaster(tempdata[[i]], paste0(outpath, names(tempdata)[i],"_slk"), format = "GTiff",overwrite=TRUE)
  writeRaster(raindata[[i]], paste0(outpath, names(raindata)[i],"_slk"), format = "GTiff",overwrite=TRUE)
}
