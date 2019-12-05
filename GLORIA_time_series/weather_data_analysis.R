library(RColorBrewer)
library(data.table)
library(ggpubr)
library(ggplot2)
svpath <- "C:/Users/user/Desktop/GLORIA_EVI/weather_plot/"
rdata <- fread("C:/Users/user/Desktop/GLORIA_EVI/1979-2019_monthly_weather_data.csv")
rdata[,date:=as.Date(date)][,year:=year(date)][,month:=month(date)]
rdata[,quarter:=ceiling((as.numeric(month)+1)/3)][month==12,quarter:=1][,
      year_q:=year][month==12,year_q:=year+1]
write.csv(rdata,"C:/Users/user/Desktop/GLORIA_EVI/1979-2019_quarter_weather_data.csv")
y_sum <-rdata[,.(temp=mean(temp_avg),pre=sum(pre_total)),by=year][year!=2019,]
ylab <- expression('Temperature ('*~degree*C*')')
##########plot to monthly weather
pt_sum <- ggplot(y_sum,aes(x=year,y=temp))+
  geom_line(color= brewer.pal(7, "Dark2")[1])+
  ylab(ylab)+
  xlab("Year")+
  scale_x_continuous(breaks = seq(1979,2019, 2))
pr_sum <- ggplot(y_sum,aes(year,pre))+
  geom_bar(stat = "identity",width=0.5,fill= brewer.pal(7, "Dark2")[1])+
  ylab("Precipitation(mm)")+
  xlab("Year")+
  scale_x_continuous(breaks = seq(1979,2019, 2))
ggarrange(pt_sum + rremove("x.text")+ rremove("xlab"), 
          pr_sum + rremove("xlab"),
          ncol = 1)
ggsave(paste0(svpath,"monthly_weather.jpeg"),width=7*1.5,height=4*1.5,dpi=600)
####### analyze the quarter data
for (i in c(1:4)){
  yq<- rdata[quarter==i,.(temp=mean(temp_avg),pre=sum(pre_total)),by=year_q][
    ,year_q:=as.numeric(year_q)]
  yq[,tavg:=mean(temp)][,pavg:=mean(pre)]
  assign(paste0("yp",i),yq)
  pt <- ggplot(yq,aes(x=year_q,y=temp))+
    geom_line(color= brewer.pal(7, "Set1")[i+1])+
    geom_line(aes(y=tavg),color="gray")+
    ylab(ylab)+
    xlab("Year")+
    scale_x_continuous(breaks = seq(1979,2019, 2))
   assign(paste0("ptq",i),pt)
  pr <- ggplot(yq,aes(year_q,pre))+
    geom_bar(stat = "identity",width=0.5,fill= brewer.pal(7, "Set1")[i+1])+
    ylab("Precipitation(mm)")+
    geom_line(aes(y=pavg),color="gray")+
    xlab("Year")+
    scale_x_continuous(breaks = seq(1979,2019, 2))
  assign(paste0("prq",i),pr)
}

##### plot temp

pt_comb <- ggarrange(ptq1 + rremove("x.text")+ rremove("xlab")+ rremove("ylab"), 
          ptq2+ rremove("x.text")+ rremove("xlab")+ rremove("ylab"), 
          ptq3+ rremove("x.text")+ rremove("xlab")+ rremove("ylab"), 
          ptq4+ rremove("ylab")+ rremove("xlab"),
          labels="AUTO",vjust=1.7,hjust=-4,align="v",heights=c(1,1,1,1),
          ncol = 1)
annotate_figure(pt_comb,bottom="Year",left=text_grob(ylab,rot = 90)) #### creat xlab and ylab
ggsave(paste0(svpath,"quarter_temp.jpeg"),width=7*1.5,height=4*1.5,dpi=600)
##### plot rainfall
pr_comb <- ggarrange(prq1 + rremove("x.text")+ rremove("xlab")+ rremove("ylab"), 
                     prq2+ rremove("x.text")+ rremove("xlab")+ rremove("ylab"), 
                     prq3+ rremove("x.text")+ rremove("xlab")+ rremove("ylab"), 
                     prq4+ rremove("ylab")+ rremove("xlab"),
                     labels="AUTO",vjust=1.7,hjust=-4,align="v",heights=c(1,1,1,1),
                     ncol = 1)
annotate_figure(pr_comb,bottom="Year",left=text_grob("Precipitation(mm)",rot = 90))
ggsave(paste0(svpath,"quarter_pre.jpeg"),width=7*1.5,height=4*1.5,dpi=600)
