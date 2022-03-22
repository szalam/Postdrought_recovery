rm(list=ls())

#library
library(raster)
library(rgdal)

#directory
wd = list()
wd$data = 'location of .bil files' #data not uploaded in github. Available at PRISM website
wd$output = 'C:/sarfaraz/Project_Drought_Recovery/Prism_data/output/'
wd$shape = 'C:/sarfaraz/Project_Drought_Recovery/gis/subregion_shape/'

setwd(wd$data)

#read shape
extent = readOGR(paste0(wd$shape,"CV_subregion.shp"))
crs_tmp = crs(extent)

r.timeseries = 0

# for (i in 1895:2019) {
for (i in 1981:2019) {
  print(i)
  #reading first month of the year i
  r = raster(paste0('PRISM_ppt_stable_4kmM3_',i,'01_bil.bil'))
  r.crs = crs(r) 
  extent=spTransform(extent, r.crs)
  
  #clipping raster using shapefile
  r2 <- crop(r, extent)
  r3 <- mask(r2, extent)
  
  r.timeseries = c(r.timeseries,cellStats(r3, 'mean'))
  
  # if(i<2019){
    
    for (j in 2:12) {
      
      if(j>9){
        r = brick(paste0('PRISM_ppt_stable_4kmM3_',i,j,'_bil.bil'))
        
        #clipping raster using shapefile
        r2 <- crop(r, extent)
        r3 <- mask(r2, extent)
        r.timeseries = c(r.timeseries,cellStats(r3, 'mean'))}
      
      if(j<=9){
        r = brick(paste0('PRISM_ppt_stable_4kmM3_',i,'0',j,'_bil.bil'))
        
        #clipping raster using shapefile
        r2 <- crop(r, extent)
        r3 <- mask(r2, extent)
        r.timeseries = c(r.timeseries,cellStats(r3, 'mean'))}
    }
}

r.timeseries = r.timeseries[-1]

r.timeseries = data.frame(r.timeseries)

# write.csv(r.timeseries, paste0(wd$output,'/precipitation_prism_monthly_1895-1980_mm.csv'),
          # row.names = F)

write.csv(r.timeseries, paste0(wd$output,'/01_precipitation_prism_monthly_1981-2019_mm_CV.csv'),
          row.names = F)
