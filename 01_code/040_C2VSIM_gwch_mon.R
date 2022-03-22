rm(list=ls())

library(readxl)
library(writexl)
library(lubridate)

wd=list()
wd$data='C:/sarfaraz/Project_Drought_Recovery/9_data_prepare_from_C2VSIM_output/output/'
wd$output='C:/sarfaraz/Project_Drought_Recovery/9_data_prepare_from_C2VSIM_output/output/'

setwd(wd$data)
temp_str=0
data_list=c('3_CVground_budget_AF_beta.xlsx')
df.all = 0

for(subreg in 1:21)
{
  
  base_gw=read_excel(data_list[1],sheet = paste0('Sheet',subreg))
  dt = base_gw[,1]
  
  #gw change in a given month. Also converting AF to km3
  gw.ch.mon = (base_gw[,4] - base_gw[,3])/810714

  #combine all changes
  df.all = cbind(df.all, gw.ch.mon)
}

df.all = df.all[,-1]
colnames(df.all) = paste0('sub_',1:21)

cv_tot = rowSums(df.all)
df.all = data.frame(date = dt , df.all, CV_ch = cv_tot)
head(df.all)
df.all[,1] = as.Date(df.all[,1],format = '%m/%d/%Y_24:00')

write.csv(df.all, paste0('3_C2VSIM_gw_change_monthly_km3_beta_WRRMAR.csv'),row.names = F)


#Converting data to water year
mn = month(df.all[,1])
yr = year(df.all[,1])
df.dt = data.frame(yr = yr, mn = mn)
df.dt$mn = df.dt$mn + 3

id.tmp = which(df.dt$mn>12)
df.dt[id.tmp,1] = df.dt[id.tmp,1] + 1
df.dt[id.tmp,2] = df.dt[id.tmp,2]-12
head(df.dt)



#Calculate annual changes
df.all2 = aggregate(df.all[,2:ncol(df.all)],by = list(df.dt$yr),FUN = sum)

write.csv(df.all2, paste0('3_C2VSIM_gw_change_annual_WY_km3_beta.csv'),row.names = F)

