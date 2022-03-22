rm(list=ls())

# library
library(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(xts)

flag_ma = 1 # 1: moving average true; 2: moving average false

#blank list
data.all = wd = list()
# Directories
wd$data = 'D:/git_uploads/Postdrought_recovery/02_data/'
wd$figure = 'D:/git_uploads/Postdrought_recovery/05_figure/'

#=========================================================================================
#plot change in magnitude
#=========================================================================================
gws.comb = read.csv(paste0(wd$data,'GW_change_mag.csv'))[,-1]
colnames(gws.comb) = c('yr_type', 'GRACE', 'C2VSIM','WB','Well')
mdat = melt(gws.comb,id.var = list('yr_type'))
head(mdat)

y = c("GRACE","WB","C2VSIM","Well")
mdat = mdat[order(match(mdat[,2], y)),]
mdat[,2] = factor(as.character(mdat[,2]), levels = unique(mdat[,2]))

p = ggplot(mdat, aes(x = yr_type, y = value, fill = variable))+
  geom_bar(stat="identity",color='black', position = "dodge")+
  scale_fill_manual(values=c(  "#FC4E07","#E7B800",'grey',"#00AFBB"))+
  xlab('Periods') + ylab(expression(paste("GWS change ",(km^3)))) + theme_bw()+
  theme(text = element_text(size=19),
        axis.text.x = element_text(angle=0, hjust=.5),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = seq(-50, 25, by=10), limits=c(-50,25))+
  theme(legend.title = element_blank())
p

ggsave(p,filename=paste0(wd$figure,"GWS_change_barplot.png"),
       width = 19, height = 14, units = "cm")

#=========================================================================================
#recovery percent change
#=========================================================================================

gws.comb.rec = read.csv(paste0(wd$data,'GW_change_perc.csv'))

gws.rec.perc.tmp = gws.comb.rec[,-2]

mdat = melt(gws.rec.perc.tmp,id.var = list('recov_per'))
head(mdat)
mdat$recov_per = as.factor(mdat$recov_per)

y = c("GRACE","WB","C2VSIM","Well")
mdat = mdat[order(match(mdat[,2], y)),]
mdat[,2] = factor(as.character(mdat[,2]), levels = unique(mdat[,2]))

p =ggplot(mdat, aes(x = recov_per, y = value, fill = variable))+
  geom_bar(stat="identity",color='black', position = "dodge")+
  scale_fill_manual(values=c(  "#FC4E07","#E7B800",'grey',"#00AFBB"))+
  xlab('Periods') + ylab("Post-drought overdraft recovery (%)") + theme_bw()+
  theme(text = element_text(size=19),
        axis.text.x = element_text(angle=0, hjust=.5),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = seq(0, 100, by=10), limits=c(0,100))+
  theme(legend.title = element_blank())
p
ggsave(p,filename=paste0(wd$figure,"GWS_recov_perc_barplot.png.png"),
       width = 19, height = 14, units = "cm")

