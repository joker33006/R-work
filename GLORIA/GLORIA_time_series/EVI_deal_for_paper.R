library(data.table)
library(ggplot2)
library(scales) #call the date_breaks()
library(cowplot)
library(gganimate)
library(ggpubr) #function theme_classic2 for ggplot
path <- 'E:/忍者/GLORIA_個人處理/paper_準備/'
name_list <- list.files(paste0(path,'EVI/EVI_ts_data/'))
EVI_r <- NULL
for (i in 1:length(name_list)){
  evi <- fread(paste0(path,'EVI/EVI_ts_data/',name_list[i]))
  evi[,summit:=substr(name_list[i],start=1,stop=3)]
  EVI_r <- rbind(EVI_r,evi)
}

EVI_r[,summit:=factor(summit,levels=c('SEN','TSW','DSH','YAT','JNJ','SUN'))]
write.csv(EVI_r,paste0(path,'/EVI_ts_date_2003_2020.csv'))
EVI_r_t <- EVI_r[!is.na(trend)]
ggplot(data=EVI_r_t,aes(x=date_w,y=trend,color=summit))+
  geom_line(size=0.9)+
  theme_classic2()+
  scale_x_date(breaks=date_breaks("2 year"),date_labels = "%Y")+
  labs(x='Year',y='EVI trend')+
  scale_color_viridis_d()+
  transition_time(date_w)
p + 
  geom_point() +
  transition_reveal(date_w)

ggsave(paste0(path,'/plot/EVI_trend_2003_2020.jpeg'),
       width=7,height=4,dpi=600)
##################plot the seasonal trend
ggplot(data=EVI_r[year(date_w)==2011],aes(x=date_w,y=Season,color=summit))+
  geom_line(size=0.9)+
  theme_classic2()+
  scale_x_date(breaks=date_breaks("month"),date_labels = "%m")+
  labs(x='Month',y='EVI seasonal trend')
ggsave(paste0(path,'/plot/EVI_season_trend_2003_2020.jpeg'),
       width=7,height=4,dpi=600)
###############################################
###########################the monthly mean EVI
EVI_r[,month:=month(date_w)][,year:=year(date_w)]
EVI_r[,period:=NULL]
EVI_r[year %in% c(2004:2009),period:='2004-2009']
EVI_r[year %in% c(2010:2015),period:='2010-2015']
EVI_r[year %in% c(2016:2019),period:='2016-2019']
EVI_m <- EVI_r[,.(EVI=mean(EVI),EVI_sd=sd(EVI)),by=.(summit,month,period)]
write.csv(EVI_m,paste0(path,'/EVI/EVI_period_of_summit.csv'))
area <- c('SEN','TSW','DSH','YAT','JNJ','SUN')
number <- c('a','d','e','b','f','c')
EVI_m <- EVI_m[!is.na(period)]
for (i in 1:6){
p <- ggplot(data=EVI_m[summit==area[i],],aes(x=month,y=EVI,color=period))+
  geom_line(size=0.5)+
  theme_classic2()+
  scale_x_continuous(breaks = seq(1,12,1))+
  labs(x='Month',y='EVI')+
  scale_color_brewer(palette="Dark2")+
  ggtitle(paste0('(',number[i],') ',area[i]))+
  theme(plot.title = element_text(size=16,hjust = 0.05,vjust=-4))
assign(paste0('EVI_',area[i]),p)
  ggsave(paste0(paste0(path,'/plot/','EVI_period_',area[i],'.jpeg')),
         plot=p,
         width=6,height=3,dpi=600)
}
p <- ggplot(data=EVI_m[summit==area[i],],aes(x=month,y=EVI,color=period))+
  geom_line(size=0.5)+
  theme_classic2()+
  scale_x_continuous(breaks = seq(1,12,1))+
  labs(x='Month',y='EVI')+
  scale_color_brewer(palette="Dark2")+
  ggtitle(paste0('(',number[i],') ',area[i]))+
title <- theme(plot.title = element_text(size=16,hjust = 0.05,vjust=-4),legend.position="top")



legend <- get_legend(p)
ggsave(paste0(paste0(path,'/plot/','EVI_legend_SYU_.jpeg')),
       plot=legend)
