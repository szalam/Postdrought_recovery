rm(list=ls())

library(lubridate)

wd = list()
wd$data = 'C:/sarfaraz/Project_Drought_Recovery/12_Dayflow_data_process/data/'
wd$output = 'C:/sarfaraz/Project_Drought_Recovery/12_Dayflow_data_process/output/'

setwd(wd$data)

#reading data
df.cfs = read.csv('dayflow-1970_2019_cfs.csv')

head(df.cfs,5)
class(df.cfs$Date)
df.cfs$Date = as.Date(df.cfs$Date, format = '%m/%d/%Y')


#calculate monthly average
df.mon = aggregate(. ~ Year + Month , df.cfs, FUN = mean)
head(df.mon)

df.mon = df.mon[,-3]

dys.in.mon = days_in_month(as.Date(paste0(df.mon$Month,'/15/',df.mon$Year),format = '%m/%d/%Y'))

# convert cfs to km3/month
for(i in 3:ncol(df.mon)){
  df.mon[,i] = df.mon[,i]*2.83168e-11*86400*dys.in.mon 
}

head(df.mon)
df.mon2 = df.mon[with(df.mon, order(Year, Month)), ]
head(df.mon2)

setwd(wd$output)
write.csv(df.mon2 , 'Dyflow_km3PerMon.csv', row.names = F)
