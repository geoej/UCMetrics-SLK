library(raster)
library(sf)

phdata = stack()
sandata = stack()
claydata = stack()

phpath ="/Users/ej/CFFRC/04-Research/Soil/climatesoilindex/cmbndsuitindx/data/soil/"
otherpath = "/Users/ej/CFFRC/05-Projects/Bera/Activity 3/Tool/TTSR/soil/Soils/"
for (i in 1:7) phdata = addLayer(phdata, raster(paste(phpath,"PHIHOX_M_sl",  i, "_250m_ll.tif", sep = "")))
for (i in 1:7) claydata = addLayer(claydata, raster(paste(otherpath,"CLYPPT_M_sl",  i, "_250m_ll.tif", sep = "")))
for (i in 1:7) sandata = addLayer(sandata, raster(paste(otherpath,"SNDPPT_M_sl",  i, "_250m_ll.tif", sep = "")))
depthdata = raster(paste(phpath,"BDRICM_M_250m_ll.tif", sep = ""))

slkbnd <- st_read("./in/gadm41_LKA_shp/gadm41_LKA_0.shp")

phdata = mask(crop(phdata,extent(slkbnd)), slkbnd)
sandata = mask(crop(sandata,extent(slkbnd)), slkbnd)
claydata = mask(crop(claydata,extent(slkbnd)), slkbnd)
depthdata = mask(crop(depthdata,extent(slkbnd)), slkbnd)

outpath = "/Users/ej/CFFRC/04-Research/UC metrics/UCMetrics-SLK/in/soilgrids/"
for (i in 1:7){
  writeRaster(phdata[[i]], paste0(outpath, names(phdata)[i],"_slk"), format = "GTiff",overwrite=TRUE)
  writeRaster(sandata[[i]], paste0(outpath, names(sandata)[i],"_slk"), format = "GTiff",overwrite=TRUE)
  writeRaster(claydata[[i]], paste0(outpath, names(claydata)[i],"_slk"), format = "GTiff",overwrite=TRUE)
}
writeRaster(depthdata, paste0(outpath, names(depthdata),"_slk"), format = "GTiff",overwrite=TRUE)
  
  


