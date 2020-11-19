library(data.table)
library(ggplot2)
library(ggpubr)
DAS <- fread('E:/忍者/GLORIA_個人處理/Weather_data/ERA5_monthly/1979_2019_DAS_rain.csv')
SYU <- fread('E:/忍者/GLORIA_個人處理/Weather_data/ERA5_monthly/1979_2019_SYU_rain.csv')
Sys.setlocale("LC_TIME", "English")

DAS[,region:='DAS']
SYU[,region:='SYU']
rdata <- rbind(DAS,SYU)

colnames(rdata) <- c('date','precipitation','region')
rdata[,date:=as.Date(date,format='%B %d, %Y')]
rdata[,month:=month(date)][,year:=year(date)]
rdata[month%in%3:5,season:='Spring'][
        month%in%6:8,season:='Summer'][
          month%in%9:11,season:='Fall'][
            is.na(season),season:='Winter']
rdata[,year.s:=year][month==12,year.s:=year+1]
s <- c('Winter','Spring','Summer','Fall')

r_season <- rdata[,.(precipitation=sum(precipitation)*1000),by=.(year.s,season,region)]
r_year <- rdata[,.(precipitation=sum(precipitation)*1000),by=.(year,region)]
write.csv(r_season,paste0('E:/忍者/GLORIA_個人處理/paper_準備/ERA_rain_season_1979_2019_',region[j],'.csv'))

#plot the seasonal rain
reg <- c('DAS','SYU')
for(j in 1:2){
for (i in 1:4){
  avg <- r_season[season==s[i]&region==reg[j],mean(precipitation)]
  p <- ggplot(r_season[season==s[i]&year.s%in%c(2008:2019)&region==reg[j]],aes(x=year.s,y=precipitation))+
    geom_col( width = 0.5,fill='cyan4')+
    labs(x='Year',y='Precipitation (mm)')+
    geom_line(y=avg,linetype='dashed',color='gray50')+
    theme_classic()+
    scale_x_continuous(breaks = seq(2008,2020,2))+
    theme(plot.title = element_text(size=16,hjust = 0.05,vjust=-2))+
    ggtitle(paste0('(',letters[i+4*(j-1)],') ',s[i]))
assign(paste0(reg[j],"_",letters[i]),p)
  ggsave(paste0('E:/忍者/GLORIA_個人處理/paper_準備/plot/rain_avg_',reg[j],'_',s[i],'.jpeg'),plot=p,
         width=4,height=3,dpi=600)
}#finish the i loop
}
###### plot the combine plot
c_p <- ggarrange(DAS_a,DAS_b,DAS_c,DAS_d,SYU_a,SYU_b,SYU_c,SYU_d,ncol=2,nrow=4)
ggsave('E:/忍者/GLORIA_個人處理/paper_準備/plot/rain_avg_total.jpeg',plot=c_p,
       width=8,height=12,dpi=600)
#plot the annual rain
for(j in 1:2){
    avg <- r_year[region==reg[j],mean(precipitation)]
    ggplot(r_year[region==reg[j]],aes(x=year,y=precipitation))+
      geom_col( width = 0.5,fill='cyan4')+
      labs(x='Year',y='Precipitation (mm)')+
      #geom_line(y=avg,linetype='dashed',color='gray50')+
      theme_classic()+
      scale_x_continuous(breaks = seq(1979,2020,5))+
      theme(plot.title = element_text(size=16,hjust = 0.05,vjust=-2))+
      ggtitle(paste0('(',letters[j],') ',reg[j]))
    
    ggsave(paste0('E:/忍者/GLORIA_個人處理/paper_準備/plot/rain_avg_',reg[j],'.jpeg'),
           width=7,height=3,dpi=600)
  }#finish the i loop
