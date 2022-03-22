rm(list=ls())

library(lubridate)
library(dplyr)
library(tools)

wd = list()
wd$data = 'C:/sarfaraz/Project_Drought_Recovery/11_CV_inflow_USGS/output/'
wd$output = 'C:/sarfaraz/Project_Drought_Recovery/11_CV_inflow_USGS/output/'

setwd(wd$data)

df.comb = list()

#reading data
df = read.csv('CV_inflow_USGS_cfs.csv')
head(df)
df$date = as.Date(df$date,format = '%Y-%m-%d')
mn = month(df$date)
yr = year(df$date)
df = cbind(df,yr,mn)
dt = data.frame(yr = rep(1970:2020,each=12), mn = rep(1:12,times=(2020-1970+1)))

#calculate monthly average
for (i in 1:32) {
  df.tmp = aggregate(df[,(2+i)]~ yr + mn , df, FUN = mean, na.rm=T)
  df.comb[[i]] = left_join(dt,df.tmp, by = c('yr','mn'))
}

df.all = df.comb[[1]]
for (i in 2:length(df.comb)) {
  df.all = cbind(df.all, df.comb[[i]][,3])
}

head(df.all)
colnames(df.all) = c('Year','Month',colnames(df)[3:34])

dys.in.mon = days_in_month(as.Date(paste0(df.all$Month,'/15/',df.all$Year),format = '%m/%d/%Y'))

# convert cfs to km3/month
for(i in 3:ncol(df.all)){
  df.all[,i] = df.all[,i]*2.83168e-11*86400*dys.in.mon 
}

head(df.all)
# df.mon2 = df.mon[with(df.mon, order(Year, Month)), ]
# head(df.mon2)

setwd(wd$output)
write.csv(df.all , 'CV_inflow_USGS_km3_month.csv', row.names = F)