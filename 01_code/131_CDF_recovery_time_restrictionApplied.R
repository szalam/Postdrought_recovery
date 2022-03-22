rm(list=ls())

library(ggplot2)

wd=list()
wd$data = 'C:/sarfaraz/Project_Drought_Recovery/35_CDF_recovery_time/data/'
wd$output = 'C:/sarfaraz/Project_Drought_Recovery/35_CDF_recovery_time/output/'
wd$figure = 'C:/sarfaraz/Project_Drought_Recovery/35_CDF_recovery_time/figure/'

jk = 2
df.all = 0
setwd(wd$data)
for(kk in 1:3){
  # df = read.table('ts1_cedar.txt',header = F)
  if(jk == 1){per = '07_09'; per.name = '2007-09'}; if(jk==2){per ='12_15';per.name = '2012-15'}
  if(kk == 1){qnt = 50;qnt_name = '50th perc.'}; if(kk==2){qnt =60;qnt_name = '40th perc.'};if(kk==3){qnt =70;qnt_name = '30th perc.'}
  df = read.csv(paste0('3_recov_',per,'_overdraft_months_wetClimatology_restrict_',qnt,'th_perc.csv'))
  df2 = read.csv(paste0('3_recov_',per,'_overdraft_months_noDroughtClimat_restrict_',qnt,'th_perc.csv'))
  df3 = read.csv(paste0('3_recov_',per,'_overdraft_months_recentClimatology_restrict_',qnt,'th_perc.csv'))
  df4 = read.csv(paste0('3_recov_',per,'_overdraft_months_LongTermClimatology_restrict_',qnt,'th_perc.csv'))
  
  head(df)
  
  
  # qn = qnorm(df[,2], 0,1)
  # head(qn)
  
  x=df[,2]; # reads second column of time series
  z=df2[,2]; if(length(z)<20000){z = c(z,rep(300,times = (20000-length(z))))}
  k=df3[,2]; if(length(k)<20000){k = c(k,rep(300,times = (20000-length(k))))}
  l=df4[,2]; if(length(l)<20000){l = c(l,rep(300,times = (20000-length(l))))}
  
  n = length(x); n1 = length(z); n2 = length(k);n3 = length(l)
  
  
  # Sort x values and calculate range
  sx= sort(x);
  sz= sort(z);
  sk= sort(k);
  sl= sort(l);
  minx1  = min(sx);
  maxx1  = max(sx);
  minx2  = min(sz);
  maxx2  = max(sz);
  minx3  = min(sk);
  maxx3  = max(sk);
  minx4  = min(sl);
  maxx4  = max(sl);
  # if(minx1>minx2){
  #   minx=minx2
  # }else{
  #   minx=minx1;
  # }
  # if(maxx1>maxx2){
  #   maxx=maxx1;
  # }else{
  #   maxx=maxx2;
  # }
  maxx=max(maxx1,maxx2,maxx3,maxx4)
  minx=min(minx1,minx2,minx3,minx4)
  range = maxx-minx
  
  
  # For plotting
  if(range>0){
    minxaxis  = minx-0.025*range;
    maxxaxis  = maxx+0.025*range;
  }else{
    minxaxis  = minx - 1;
    maxxaxis  = maxx + 1;
  }
  
  # Plot log-normal distribution, mean=0, sigma=1, calculated at probabilities
  # based on length of data set
  eprob = seq(1/(n+1),n/(n+1),by = 1/(n+1)); eprob1 = seq(1/(n1+1),n1/(n1+1),by = 1/(n1+1));
  eprob2 = seq(1/(n2+1),n2/(n2+1),by = 1/(n2+1));eprob3 = seq(1/(n3+1),n3/(n3+1),by = 1/(n3+1));
  y  = qnorm(eprob,0,1); y1  = qnorm(eprob1,0,1); y2  = qnorm(eprob2,0,1);y3  = qnorm(eprob3,0,1);
  
  minyaxis  = qnorm(0.25/n,0,1);
  maxyaxis  = qnorm((n-0.25)/n,0,1)
  
  
  # Relate probabilities to return period in years
  p     = c(0.001, 0.01, 0.05 ,0.2, 0.5,0.75, 0.90, 0.98, 0.999);
  # corresponding return periods in years
  label = c('0.001','0.01','0.05','0.2','0.5','0.75','0.90','0.98','0.999');
  
  tick  = qnorm(p,0,1);
  
  # calculate percentiles
  q1x = quantile(x,.25);
  q3x = quantile(x,.75);
  q1y = quantile(y,.25);
  q3y = quantile(y,.75);
  qx = c(q1x, q3x);
  qy = c(q1y, q3y);
  
  # calculate best fit line
  dx = q3x - q1x;
  dy = q3y - q1y;
  slope = dy/dx;
  centerx = (q1x + q3x)/2;
  centery = (q1y + q3y)/2;
  maxx = max(x);
  minx = min(x);
  maxy = centery + slope*(maxx - centerx);
  miny = centery - slope*(centerx - minx);
  
  mx = c(minx, maxx);
  my = c(miny, maxy);
  
  plot(y,sx,col = 'red',ylim=c(0,240),xaxt='n', ann=FALSE)
  points(y1,sz,col = 'blue')
  points(y2,sk,col = 'green')
  axis(1, at=tick, labels=label)
  
  
  df.plot = data.frame(prob = c(y,y1,y2,y3), recov_t = c(sx,sz,sk,sl),
                       restrict = paste0((100-qnt),'th perc.'),
                       type = c(rep('wet_yr',times = length(y)),
                                rep('non_drt',times = length(y1)),
                                rep('avg_yr',times = length(y2)),
                                rep('longterm_yr',times = length(y3))),
                       per_name = per.name
  )
  df.all = rbind(df.all,df.plot)
}

df.all = df.all[-1,]
head(df.all)
df.all = df.all[(df.all$prob>=qnorm(.01)),]
labels <- c(avg_yr = "Recent clim.", non_drt = "No-drought", wet_yr = "Wet clim.",longterm_yr= 'Long term' )


p =ggplot(df.all, aes(y=recov_t, x=prob, color=factor(restrict))) +
  geom_line(aes(linetype = (type)),size=1)+
  # geom_point(aes(shape = type),size = 1.1)+
  theme_bw()+xlab('Cumulative probability')+
  ylab('Post-drought months')+
  scale_color_manual(values = c("#56B4E9","#E69F00","#FC4E07"))+#,labels = labels
  scale_linetype_manual(values=c("dashed", "dotted", 'twodash','solid'),labels = labels)+
  # scale_color_manual(values = c('red','blue'),labels = labels)+
  # scale_x_continuous(breaks = seq(0, 100, by=10), limits=c(0,100),expand = c(.025, 0))
  # scale_x_continuous(breaks = seq(0, 1, by=.1), limits=c(0,1),expand = c(0, 0))+
  scale_x_continuous(breaks=c(tick),
                   labels=as.character(label))+
  scale_y_continuous(breaks = seq(0, 240, by=24), limits=c(0,240),expand = c(0, 0))+
  # scale_shape_manual(values=c(1, 3,2))+
  theme(axis.text.y = element_text(color = "black", size = 14),
        axis.title.y = element_text(color = "black", size = 14),
        axis.text.x = element_text(color = "black", size = 14, angle = 0, hjust = .5, vjust = .5),
        axis.title.x = element_text(color = "black", size = 14),
        legend.title = element_blank()) +
  theme(legend.position = c(0.7, 0.3),
        legend.background = element_rect(linetype='solid', color='black', size =.5),
        legend.text = element_text(size=14))+
  theme(
    legend.position = "none",
        panel.grid.minor = element_blank())
p


ggsave(p,filename=paste0(wd$figure,'1_2_CDF_recovery_combine_overdraft_restrict_',per.name,'.png'), width = 16, height =15, units = 'cm')

df.all2 = df.all
df.all2$prob = pnorm(df.all2$prob)
# df.all2$prob = round(df.all2$prob,4)
head(df.all2)

write.csv(df.all2, paste0(wd$output, 'Recov_time_Clim_scenarios_restrict_apply.csv'))
