rm(list=ls())

library(lubridate)

wd = list()
wd$data = 'data folder'
wd$output = 'results folder'

wb.comp = read.csv(paste0(wd$data,'WB_components_km3.csv'))
which(wb.comp[,2]=='2015-10-15')
wb.comp = wb.comp[,-1]

cum.gws.all = wb.comp$cum_gws

wb.comp$dt.tmp = as.Date(wb.comp$dt.tmp,format = '%Y-%m-%d')
yr.all = year(wb.comp$dt.tmp); mn.all = month(wb.comp$dt.tmp)

#peak years for which random sampling will be done
cum.gws.avgYr = 0
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
    
    del_GWS.rand.4yr = c(del_GWS.rand.4yr,del_GWS.rand)
  }
  
  del_GWS.rand.4yr = del_GWS.rand.4yr[-1]
  cum_GWS.rand = cumsum(del_GWS.rand.4yr)
  cum_GWS.rand = cum_GWS.rand + cum.gws.all[length(cum.gws.all)]
  
  cum.gws.all.tmp = c(cum.gws.all,cum_GWS.rand);cum.gws.all.tmp = cum.gws.all.tmp[-1]
  
  cum.gws.avgYr = data.frame(cbind(cum.gws.avgYr, cum.gws.all.tmp))
}

cum.gws.avgYr = cum.gws.avgYr[,-1]

# write.csv(cum.gws.avgYr, paste0(wd$output, 'cum_GWS_',n,'iter_',tot.rep,'yrs_avgYr_recovery.csv'))
saveRDS(cum.gws.avgYr, paste0(wd$output, 'cum_GWS_',n,'iter_',tot.rep,'yrs_anyYr2002_2019_recovery.rds'))

# plot(cum.gws.all)
  