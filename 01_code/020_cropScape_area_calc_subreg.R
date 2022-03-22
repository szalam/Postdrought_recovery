rm(list=ls())

wd=list()
wd$data = 'C:/sarfaraz/Project_Drought_Recovery/6_CropArea_cropscape/data/crop_prixel_number/'
wd$output = 'C:/sarfaraz/Project_Drought_Recovery/5_all_processed_data/'
setwd(wd$data)

fil.all = list.files(pattern = '*.csv')

df.c = df.t = df.r = df.tot =0
for (i in 1:length(fil.all)) {
  df = read.csv(paste0('subregion_',i,'_summary.csv'))
  # head(df)
  tmp.crop = df[df$crop_category=='C',5:ncol(df)]
  tmp.tree = df[df$crop_category=='T',5:ncol(df)]
  tmp.rice = df[df$crop_category=='R',5:ncol(df)]
  # head(tmp.tree)
  
  crop.area = colSums(tmp.crop,na.rm = T)
  tree.area = colSums(tmp.tree,na.rm = T)
  rice.area = colSums(tmp.rice,na.rm = T)
  
  #grid size for year 2007 is 56 m. convert the area to km2
  crop.area[1] = crop.area[1]*56*56/1000000
  tree.area[1] = tree.area[1]*56*56/1000000
  rice.area[1] = rice.area[1]*56*56/1000000
  
  #grid size for all others are 30 m. convert the area to km2
  crop.area[2:length(crop.area)] = crop.area[2:length(crop.area)]*30*30/1000000
  tree.area[2:length(tree.area)] = tree.area[2:length(tree.area)]*30*30/1000000
  rice.area[2:length(rice.area)] = rice.area[2:length(rice.area)]*30*30/1000000
  
  
  
  df.c = cbind(df.c,data.frame(crop.area))
  df.t = cbind(df.t,data.frame(tree.area))
  df.r = cbind(df.t,data.frame(rice.area))
  df.tot = rbind(df.tot, (crop.area+ tree.area +rice.area))
}

df.c = cbind(year = 2007:2018,df.c[,-1])
df.t = cbind(year = 2007:2018,df.t[,-1])
df.r = cbind(year = 2007:2018,df.r[,-1])
df.tot = rbind(year = 2007:2018,df.tot[-1,])
# write.csv(df.tot, 'C:/sarfaraz/Project_Drought_Recovery/15_ETc_ETnatveg_calculate/area_2.csv')

colnames(df.c) = colnames(df.t) = colnames(df.r) = c('year',paste0('sub_',1:21))

setwd(wd$output)
write.csv(df.c, '8_rowcrop_area_cropScape_subregion_2007_2018_km2.csv')
write.csv(df.t, '8_treecrop_area_cropScape_subregion_2007_2018_km2.csv')
write.csv(df.t, '8_rice_area_cropScape_subregion_2007_2018_km2.csv')