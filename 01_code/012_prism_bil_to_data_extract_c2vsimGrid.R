rm(list=ls())

library(raster)
library(rgdal)
library(sp)

wd = list()
wd$data = 'C:/sarfaraz/Project_Drought_Recovery/Prism_data/data/PRISM_ppt_stable_4kmM3_198101_202002_bil/'
wd$coord = 'C:/sarfaraz/Project_Drought_Recovery/18_PRISM_extract_C2VSIM_grids/data/'
wd$output = 'C:/sarfaraz/Project_Drought_Recovery/18_PRISM_extract_C2VSIM_grids/output/'

setwd(wd$coord)
coord1 = coord.tmp1 = read.csv('c2vsim_element_centroid_coordinates_wgs84.csv')[,7:8]
coord2 = coord.tmp2 = read.csv('c2vsim_small_watershed_centroid_coordinates_wgs84.csv')[,5:6]

coord = coord.tmp = rbind(coord1,coord2 )
coordinates(coord.tmp)= ~ x_centroid + y_centroid
crs(coord.tmp) = "+proj=longlat +datum=WGS84"
coord.tmp2=spTransform(coord.tmp,crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'))

# e = drawExtent()
setwd(wd$data)
# saveRDS(e, 'extent_for_extract.rds')
extent = readRDS('extent_for_extract.rds')
# extent = readOGR("C:/sarfaraz/Project_GW_Cure/ppt_mean_livneh_calculate/data/shapefile/CA_state_boundary.shp")

r.timeseries = 0

for (i in 2016:2020) {
  print(i)
  #reading first month of the year i
  r = raster(paste0('PRISM_ppt_stable_4kmM3_',i,'01_bil.bil'))
  
  #clipping raster using shapefile
  r2 <- crop(r, extent)
  r3 <- r2#mask(r2, extent)
  
  rasValue=extract(r3, coord.tmp2)/25.4
  
  r.timeseries = cbind(r.timeseries,data.frame(rasValue))
  
  if(i<2020){
    
    for (j in 2:12) {
      
      if(j>9){
        r = brick(paste0('PRISM_ppt_stable_4kmM3_',i,j,'_bil.bil'))
        
        #clipping raster using shapefile
        r2 <- crop(r, extent)
        r3 <- r2#mask(r2, extent)
        
        rasValue=extract(r3, coord.tmp2)/25.4
        r.timeseries = cbind(r.timeseries,data.frame(rasValue))}
      
      if(j<=9){
        r = brick(paste0('PRISM_ppt_stable_4kmM3_',i,'0',j,'_bil.bil'))
        
        #clipping raster using shapefile
        r2 <- crop(r, extent)
        r3 <- r2#mask(r2, extent)
        
        rasValue=extract(r3, coord.tmp2)/25.4
        r.timeseries = cbind(r.timeseries,data.frame(rasValue))}
    }
  }
  
  if(i==2020){
    for (j in 2:2) {
      r = brick(paste0('PRISM_ppt_stable_4kmM3_',i,'0',j,'_bil.bil'))
      
      #clipping raster using shapefile
      r2 <- crop(r, extent)
      r3 <- r2#mask(r2, extent)
      
      rasValue=extract(r3, coord.tmp2)/25.4
      r.timeseries = cbind(r.timeseries,data.frame(rasValue))}
  }
  
  # plot(r3)
  # plot(extent,add=T)
}

r.timeseries = r.timeseries[,-1]

r.timeseries = data.frame(r.timeseries)

# write.csv(r.timeseries, paste0(wd$output,'/P_prism_mon_C2vsimPoint_2016_2020_mm.csv'),
#           row.names = F)
write.csv(r.timeseries, paste0(wd$output,'/P_prism_mon_C2vsimPoint_2016_2020_inch.csv'),
          row.names = F)
