rm(list=ls())

library(lubridate)
library(dplyr)
library(tools)

wd = list()
wd$data = 'C:/sarfaraz/Project_Drought_Recovery/11_CV_inflow_USGS/data/'
wd$output = 'C:/sarfaraz/Project_Drought_Recovery/11_CV_inflow_USGS/output/'

setwd(wd$data)

#reading data
d.list = list.files()
f.names = file_path_sans_ext(d.list)

d.all = list()

for(i in 1:length(d.list)){
  start_read= 'agency_cd'
  
  data=readLines(d.list[i])
  start_lines=grep(start_read,data)
  
  df.tmp = read.table(d.list[i],skip = (start_lines+1), fill=T)
  summary(df.tmp)
  head(df.tmp)
  df.tmp = data.frame(date = df.tmp[,3], Q = df.tmp[,4])
  df.tmp$date = as.character(df.tmp$date)
  
  dt = seq(from =as.Date('01/01/1970', format = '%m/%d/%Y'), to=as.Date('01/04/2020', format = '%m/%d/%Y'), by =1)
  dt = data.frame(date = as.character(dt))
  df.tmp2 = left_join(dt,df.tmp, by = 'date' )
  # head(df.tmp2)
  
  colnames(df.tmp2) = c('date', f.names[i])
  
  d.all[[i]] = df.tmp2
}


df.tmp = d.all[[1]]
for (j in 2:length(d.all)) {
  df.tmp = cbind(df.tmp, d.all[[j]][,2])
}

colnames(df.tmp) = c('date',f.names)
head(df.tmp)

setwd(wd$output)
write.csv(df.tmp, 'CV_inflow_USGS_cfs.csv')
