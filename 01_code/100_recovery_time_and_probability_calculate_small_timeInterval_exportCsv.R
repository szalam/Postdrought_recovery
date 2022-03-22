rm(list=ls())

library(lubridate)
library(reshape2)
library(ggplot2)

wd = list()
wd$data = 'results folder'
wd$data2 = 'results folder4_pumping_restriction_percentile/'
wd$data3 = 'results folderexcess_flow_recharge/'
wd$data4 = 'results folder5_excess_flow_recharge_with_upLim/'
wd$data5 = 'results folder5_excess_flow_recharge_with_upLim_40percEfficiency/'
wd$figure = 'C:/sarfaraz/Project_Drought_Recovery/28_numerical_experiment_waterBalance_with_precip_sampling/figure/'
wd$output = 'results folder6_3_recovery_time_and_probability_calc_small_time_interval/'
setwd(wd$data)

# df.tmp = read.csv('cum_GWS_20000iter_20yrs_recovery.csv')
# saveRDS(df.tmp,'cum_GWS_20000iter_20yrs_recovery.rds')

# df.tmp = readRDS('cum_GWS_20000iter_20yrs_anyYr2002_2019_recovery.rds')
df.tmp = readRDS('cum_GWS_20000iter_20yrs_2016_2019_recovery.rds')
# df.tmp = readRDS('cum_GWS_20000iter_20yrs_nonDrghtYr_recovery.rds')

# df.tmp = readRDS(paste0(wd$data2,'cum_GWS_20000iter_20yrs_anyYr2002_2019_recovery_90th_perc_pumpLimit.rds'))
# df.tmp = readRDS(paste0(wd$data2,'cum_GWS_20000iter_20yrs_anyYr2002_2019_recovery_80th_perc_pumpLimit.rds'))
# df.tmp = readRDS(paste0(wd$data2,'cum_GWS_20000iter_20yrs_anyYr2002_2019_recovery_70th_perc_pumpLimit.rds'))
# df.tmp = readRDS(paste0(wd$data2,'cum_GWS_20000iter_20yrs_anyYr2002_2019_recovery_60th_perc_pumpLimit.rds'))
# df.tmp = readRDS(paste0(wd$data2,'cum_GWS_20000iter_20yrs_anyYr2002_2019_recovery_50th_perc_pumpLimit.rds'))

# df.tmp = readRDS(paste0(wd$data3,'cum_GWS_20000iter_20yrs_anyYr2002_2019_recovery_95th_MAR.rds'))
# df.tmp = readRDS(paste0(wd$data4,'cum_GWS_20000iter_20yrs_anyYr2002_2019_recovery_85th_MAR_upLim.rds'))
# df.tmp = readRDS(paste0(wd$data5,'cum_GWS_20000iter_20yrs_anyYr2002_2019_recovery_80th_MAR_upLim_40percEfficiency.rds'))

##---------Carefully check the multiplied factors before ------ remove when needed
del_gw_07_09 = (df.tmp[91,1]- df.tmp[55,1])#*.55
del_gw_10_11 = df.tmp[115,1]- df.tmp[91,1]
del_gw_12_15 = (df.tmp[163,1]- df.tmp[115,1])#*.36
del_gw_16_19 = df.tmp[210,1]- df.tmp[163,1]

del_gw_10_11/abs(del_gw_07_09)*100
del_gw_16_19/abs(del_gw_12_15)*100

df.tmp= df.tmp[211:nrow(df.tmp),]

# df.tmp = df.tmp[,-1]

recov.0709= recov.1215 = 0
for (i in 1:ncol(df.tmp)) {
  print(i)
  tmp = df.tmp[,i]; tmp = tmp-tmp[1]
  id.1 = which(tmp>=abs(del_gw_07_09))[1]
  recov.0709 = c(recov.0709,id.1)
  id.2 = which(tmp>=abs(del_gw_12_15))[1]
  recov.1215 = c(recov.1215,id.2)
  
  # # tmp = c(median(as.numeric(as.character(df.tmp[i,]))),sd(as.numeric(as.character(df.tmp[i,]))))
  # tmp = c(quantile(as.numeric(as.character(df.tmp[i,])), probs = c(0,.25, 0.5, 0.75,1)))
  # stat.all = rbind(stat.all,tmp)
}

recov.0709 = recov.0709[-1]
recov.0709 = recov.0709[!is.na(recov.0709)]

recov.1215 = recov.1215[-1]
recov.1215 = recov.1215[!is.na(recov.1215)]


# mon.rec = c(60,120, 180, 240)
mon.rec = seq(from = 0, to = 240,by = 6)
rec.prob.0709 = rec.prob.1215 = 0

for(j in mon.rec){
  recov.0709.tmp = which(recov.0709<=j)
  
  recov.1215.tmp = which(recov.1215<=j)
  length(recov.1215.tmp)
  
  rec.prob.0709 = c(rec.prob.0709, length(recov.0709.tmp)/ncol(df.tmp)*100)
  rec.prob.1215 = c(rec.prob.1215, length(recov.1215.tmp)/ncol(df.tmp)*100)
}
rec.prob.0709 = rec.prob.0709[-1]; rec.prob.1215 = rec.prob.1215[-1]

# yrs.cond = c('5 or less', '10 or less', '15 or less', '20 or less')
yrs.cond = mon.rec#c(seq(from = 0, to = 20,by = 1))

df.comb = data.frame(yrs.cond = yrs.cond,rec.prob.0709 = rec.prob.0709, rec.prob.1215 = rec.prob.1215)
df.comb[,1] = as.factor(df.comb[,1])
df.comb[,1] <- factor(df.comb[,1], levels=c(yrs.cond), labels=c(as.character(yrs.cond)))

setwd(wd$output)

# write.csv(df.comb, 'recov_probability_precip_2002-1019_small_interval.csv')
write.csv(df.comb, 'recov_probability_precip_2016-1019_small_interval.csv')
# write.csv(df.comb, 'recov_probability_precip_nonDrghtYrs_small_interval.csv')


# write.csv(df.comb, 'recov_probability_GWrestriction_90thpercentile.csv')
# write.csv(df.comb, 'recov_probability_GWrestriction_80thpercentile.csv')
# write.csv(df.comb, 'recov_probability_GWrestriction_70thpercentile.csv')
# write.csv(df.comb, 'recov_probability_GWrestriction_60thpercentile.csv')
# write.csv(df.comb, 'recov_probability_GWrestriction_50thpercentile.csv')


# write.csv(df.comb, 'recov_probability_200709_43_201215_38.csv')
# write.csv(df.comb, 'recov_probability_200709_55_201215_36.csv')





# df.comb.2 = melt(df.comb, id.vars = c('yrs.cond'))
# 
# label.name = c('2007-2009', '2012-2015')
# 
# ggplot(df.comb.2, aes(x=yrs.cond, y=value, fill=variable)) + 
#   geom_bar(stat="identity", color="black", position=position_dodge())+
#   theme_bw()+ylab('Probability of recovery (%)')+
#   xlab('Years')+
#   scale_color_discrete(labels = label.name)+
#   ylim(c(0,100))

