# making a grid across SLK
library(sf)
library(ggplot2)

slkbnd <- st_read("./in/gadm41_LKA_shp/gadm41_LKA_0.shp")

# 
#  boundary  <-  st_as_sf(data, coords = c("Latitude", "Longitude"),
#                     crs = 4326)
# 
# https://stackoverflow.com/questions/41787313/how-to-create-a-grid-of-spatial-points

# grid high 

 grid <- slkbnd %>%
   st_make_grid(cellsize = 0.1, what = "centers") #%>% # grid of points
#  st_intersection(DT_sf)
 
# grid low 
 gridlo <- slkbnd %>%
   st_make_grid(cellsize = 0.25, what = "centers") #%>% # grid of points
 
 grid
 ggplot() +
   geom_sf(data=gridlo)

 grid1 = st_intersection(gridlo, slkbnd)
 grid1

 ggplot() +
   geom_sf(data=grid1)

 st_write(grid1, "./in/grid/slk_gridlo543.shp")
 

#slk = st_read("./in/", "slk_grid")
#ggplot() +
#  geom_sf(data=slk)
