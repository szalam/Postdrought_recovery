rm(list=ls())

library(lubridate)

wd = list()
wd$data = 'data folder'
wd$output = 'results folder4_pumping_restriction_percentile/'

wb.comp = read.csv(paste0(wd$data,'WB_components_km3.csv'))
wb.comp = wb.comp[,-1]
gw.change = wb.comp$P-wb.comp$ET-wb.comp$SWE_change-wb.comp$SM_change + 
  wb.comp$inflow-wb.comp$CV_out_km3_mn

gw.pump = gw.change[gw.change<0]

gw.pump.per = quantile(-gw.pump, probs = c(.9,.8, 0.7, 0.6,.5))
hist(gw.pump)
quantile(-gw.change, probs = c(.9,.8, 0.7, 0.6,.5))
perc.all = c(90,80, 70, 60,50)
gw.pump.per = -gw.pump.per
tot.rep = 20
cum.gws.all = wb.comp$cum_gws

wb.comp$dt.tmp = as.Date(wb.comp$dt.tmp,format = '%Y-%m-%d')
yr.all = year(wb.comp$dt.tmp); mn.all = month(wb.comp$dt.tmp)

for(perc in 1:length(gw.pump.per)){
  
  #peak years for which random sampling will be done
  cum.gws.avgYr = excess.et.20yr =excess.et.avgyr =0
  yr.sel = c(2003:2019)
  tot.rep = 20
  for(n in 1:20000){
    del_GWS.rand.4yr = cum.gws.all.tmp = 0
    for(i in 1:tot.rep){
      print(paste0('n = ',n,', i = ',i))
      sel.rand = sample(yr.sel, 1)
      
      id.strt = which(yr.all == (sel.rand-1) & mn.all == 10)
      id.end = which(yr.all == (sel.rand) & mn.all == 9)
      
      wb.comp.tmp = wb.comp[id.strt:id.end,]
      
      del_GWS.rand = wb.comp.tmp$P - wb.comp.tmp$ET - wb.comp.tmp$SWE_change - wb.comp.tmp$SM_change + 
        wb.comp.tmp$inflow - wb.comp.tmp$CV_out_km3_mn
      id.tmp = which(del_GWS.rand<gw.pump.per[perc])
      if(length(id.tmp)>=1){
        excess.et = del_GWS.rand[id.tmp]-gw.pump.per[perc]
        excess.et = sum(excess.et)
        del_GWS.rand[id.tmp] = gw.pump.per[perc]
      }
      if(length(id.tmp)<=0){excess.et=0}
      
      excess.et.20yr = c(excess.et.20yr,excess.et)
      del_GWS.rand.4yr = c(del_GWS.rand.4yr,del_GWS.rand)
    }
    
    excess.et.20yr = sum(excess.et.20yr[-1])/20
    del_GWS.rand.4yr = del_GWS.rand.4yr[-1]
    cum_GWS.rand = cumsum(del_GWS.rand.4yr)
    cum_GWS.rand = cum_GWS.rand + cum.gws.all[length(cum.gws.all)]
    
    cum.gws.all.tmp = c(cum.gws.all,cum_GWS.rand);cum.gws.all.tmp = cum.gws.all.tmp[-1]
    
    cum.gws.avgYr = data.frame(cbind(cum.gws.avgYr, cum.gws.all.tmp))
    excess.et.avgyr = c(excess.et.avgyr,excess.et.20yr)
  }
  
  cum.gws.avgYr = cum.gws.avgYr[,-1]
  excess.et.avgyr = excess.et.avgyr[-1]
  
  # write.csv(cum.gws.avgYr, paste0(wd$output, 'cum_GWS_',n,'iter_',tot.rep,'yrs_avgYr_recovery.csv'))
  saveRDS(cum.gws.avgYr, paste0(wd$output, 'cum_GWS_',n,'iter_',tot.rep,'yrs_anyYr2002_2019_recovery_',perc.all[perc],'th_perc_pumpLimit.rds'))
  saveRDS(excess.et.avgyr, paste0(wd$output, 'excess_ET_',n,'iter_',tot.rep,'yrs_anyYr2002_2019_recov_',perc.all[perc],'th_perc_pumpLimit.rds'))
  
}
# plot(cum.gws.all)
# setwd(wd$output)
# list.files()
# for(perc in 1:length(gw.pump.per)){
#   tmp = readRDS(paste0('excess_ET_20000iter_',tot.rep,'yrs_anyYr2002_2019_recov_',perc.all[perc],'th_perc_pumpLimit.rds'))
#   print(paste0(perc.all[perc],'%; Median = ',median(tmp),'; SD = ', sd(tmp)))
# }


