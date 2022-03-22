library(ggplot2)

rm(list=ls())

# library
library(dplyr)
library(lubridate)

#blank list
data.all = wd = list()
# Directories
wd$data = 'reservoir data folder'
wd$output = 'results folder'

setwd(wd$data)

res.con = read.csv('res_lookup.csv')

df.all = read.table(paste0(res.con[1,2],'.txt'),fill=T,header=T)
df.all = df.all[,c('Dat','STORAGE_AF')]
colnames(df.all) = c('Date',res.con[1,2])

for(i in 2:nrow(res.con)){
  print(i)
  df.tmp = read.table(paste0(res.con[i,2],'.txt'),fill=T,header=T)
  df.tmp = df.tmp[,c('Dat','STORAGE_AF')]
  colnames(df.tmp) = c('Date',res.con[i,2])
  
  df.all = left_join(df.all, df.tmp, by = 'Date')
  
  }

# write.csv(df.all, paste0(wd$output,'all_reservoir.csv'))

# df.all[,1] = as.Date((df.all[,1]), format = '%m/%Y')

df.all.tmp = rowSums(df.all[,2:ncol(df.all)],na.rm=T)

df.all = data.frame(Date = df.all[,1],Storage_km3 = df.all.tmp*1.23348e-6)
head(df.all)
plot(df.all[,2])
write.csv(df.all, paste0(wd$output,'reservoir_storage_km3_mon2.csv'))