rm(list=ls())

#library
library(raster)
library(rgdal)

#directory
wd = list()
wd$data = 'C:/sarfaraz/Project_Drought_Recovery/15_ETa_SSEBOP_process/data/1_SSEBOP_monthly/'
wd$output = 'C:/sarfaraz/Project_Drought_Recovery/15_ETa_SSEBOP_process/output/'
setwd(wd$data)


# extent = readOGR("C:/sarfaraz/Project_GW_Cure/ppt_mean_livneh_calculate/data/shapefile/CA_state_boundary.shp")
extent = readOGR("C:/sarfaraz/Project_Drought_Recovery/gis/subregion_shape/CV_subregion.shp")
geo.prj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
extent = spTransform(extent, geo.prj)

# r = brick('TRMM_3B43_199801-201812.nc')
# r2 <- t(flip(r, direction='y' ))
# 
# geo.prj <- "+proj=longlat +datum=WGS84"
# crs(r2) = crs(geo.prj)
# 
# 
# r2 <- mask(crop(r2, RedSea_shp_Watershed), extent)


# mn = rep(1:12,times = 39)
r.timeseries = 0
yr = mn = 0

for (i in 2000:2019) {
  print(i)
    for (j in 1:12) {
      if(i==2019 & j==11){break()}
      if(j>9){r = brick(paste0('m',i,j,'.modisSSEBopET.tif'))}
      if(j<=9){r = brick(paste0('m',i,'0',j,'.modisSSEBopET.tif'))}
     
      #clipping raster using shapefile
      r2 <- crop(r, extent)
      r3 <- mask(r2, extent)
      r.timeseries = c(r.timeseries,cellStats(r3, 'mean'))
      
      yr = c(yr,i); mn = c(mn,j)
    }
}

r.timeseries = r.timeseries[-1]
mn = mn[-1]; yr = yr[-1]

r.timeseries = data.frame(year = yr, mn = mn, ET = r.timeseries)



write.csv(r.timeseries, paste0(wd$output,'/ETa_SSEBOP_monthly_2000-2019_mm_CV.csv'),
          row.names = F)
