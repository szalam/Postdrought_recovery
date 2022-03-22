rm(list=ls())

#library
library(raster)
library(rgdal)
library(sp)

#flag
flag = 2 # 1: 1895-1980, 2: 1981-2019

#directory
wd = list()
if(flag == 1){wd$data = 'C:/sarfaraz/Project_Drought_Recovery/Prism_data/data/'}
if(flag == 2){wd$data = 'C:/sarfaraz/Project_Drought_Recovery/Prism_data/data/PRISM_ppt_stable_4kmM3_198101_202002_bil/'}
wd$shapefile = 'C:/sarfaraz/Project_Drought_Recovery/gis/HUC_4_shapefile/'
wd$output = 'C:/sarfaraz/Project_Drought_Recovery/Prism_data/output/'

setwd(wd$data)

shp1 = readOGR(paste0(wd$shapefile,'NHD_H_1802_HU4_Shape/WBDHU4.shp'))
shp2 = readOGR(paste0(wd$shapefile,'NHD_H_1803_HU4_Shape/WBDHU4.shp'))
shp3 = readOGR(paste0(wd$shapefile,'NHD_H_1804_HU4_Shape/WBDHU4.shp'))
shp.all =list()
shp.all[[1]] = shp1
shp.all[[2]] = shp2
shp.all[[3]] = shp3

p_all_df = 0
for (n in 1:3) {
  r.timeseries = 0
  if(flag == 1){
    b = 1895
    e = 1980
  }
  if(flag == 2){
  b = 1981
  e = 2020
  }
  # if(flag == 1){}
  # for (i in 1895:1980) {
  for (i in b:e) {
    
    print(i)
    #reading first month of the year i
    if(flag == 1){r = raster(paste0('PRISM_ppt_stable_4kmM2_',i,'01_bil.bil'))}
    if(flag == 2){r = raster(paste0('PRISM_ppt_stable_4kmM3_',i,'01_bil.bil'))}
    
    #clipping raster using shapefile
    r2 <- crop(r, shp.all[[n]])
    r3 <- mask(r2, shp.all[[n]])
    
    r.timeseries = c(r.timeseries,cellStats(r3, 'mean'))
    
    if(i<2020){
      
      for (j in 2:12) {
        
        if(j>9){
          if(flag == 1){r = brick(paste0('PRISM_ppt_stable_4kmM2_',i,j,'_bil.bil'))}
          if(flag == 2){r = brick(paste0('PRISM_ppt_stable_4kmM3_',i,j,'_bil.bil'))}
          
          #clipping raster using shapefile
          r2 <- crop(r, shp.all[[n]])
          r3 <- mask(r2, shp.all[[n]])
          r.timeseries = c(r.timeseries,cellStats(r3, 'mean'))}
        
        if(j<=9){
          if(flag == 1){r = brick(paste0('PRISM_ppt_stable_4kmM2_',i,'0',j,'_bil.bil'))}
          if(flag == 2){r = brick(paste0('PRISM_ppt_stable_4kmM3_',i,'0',j,'_bil.bil'))}
          
          #clipping raster using shapefile
          r2 <- crop(r, shp.all[[n]])
          r3 <- mask(r2, shp.all[[n]])
          r.timeseries = c(r.timeseries,cellStats(r3, 'mean'))}
      }
    }
    
    if(i==2020){
      for (j in 2:2) {
        r = brick(paste0('PRISM_ppt_stable_4kmM3_',i,'0',j,'_bil.bil'))
        
        #clipping raster using shapefile
        r2 <- crop(r, shp.all[[n]])
        r3 <- mask(r2, shp.all[[n]])
        r.timeseries = c(r.timeseries,cellStats(r3, 'mean'))}
    }
    
    # plot(r3)
    # plot(extent,add=T)
  }
  
  r.timeseries = r.timeseries[-1]
  p_all_df = cbind(p_all_df, data.frame(r.timeseries))
}

p_all_df = p_all_df[,-1]

p_all_df = data.frame(p_all_df)
colnames(p_all_df) = c('SC','SJ','TL')

if(flag == 1){write.csv(p_all_df, paste0(wd$output,'/precipitation_prism_monthly_1895-1980_mm_HUC4.csv'),
                                  row.names = F)}
if(flag == 2){write.csv(p_all_df, paste0(wd$output,'/precipitation_prism_monthly_1981-2020_mm_HUC4.csv'),
                        row.names = F)}

