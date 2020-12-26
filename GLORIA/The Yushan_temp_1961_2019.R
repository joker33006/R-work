#using the Yushan Weather Station to be the background weather data.
library(data.table)
library(ggplot2)
library(broom)
library(ggpubr)
library(mondate)
path <- 'E:/忍者/GLORIA_個人處理/paper_準備/'
wd <- fread(paste0(path,'Yushan_temp/1961_2019_Yushan_temp.csv'))

ggplot(data=wd,aes(x=`Year/Month`,y=Avg))+
  geom_point()+
  geom_smooth(method = lm)+
  theme_classic2()+
  labs(x='Year',y='Annual average temperature (°C)',size=16)+
  ylim(c(0,7))+
  geom_vline(aes(xintercept=2008), colour="#BB0000", linetype="dashed")
ggsave(paste0(paste0(path,'/plot/Temp_Yushan_1961_2019.jpeg')),
       width=4,height=3,dpi=300)
####################Compare the data between Yhushan station and ERA5

r_wd <- melt(wd,id.vars = "Year/Month",
             measure.vars = c("1","2","3",'4','5','6','7','8','9','10','11','12'), #轉置之名稱，可用pattern+regx 做查找
             variable.name = "month")
r_wd[,yymm:=paste0(Year,'-',month)]
setnames(r_wd,"Year/Month",'Year')
name <- list.files('E:/忍者/GLORIA_個人處理/GBIF_data/ERA_Yushan')
era_f <- NULL
for (i in 1:2){
era5 <- fread(paste0('E:/忍者/GLORIA_個人處理/GBIF_data/ERA_Yushan/',name[i]))
era_f <- rbind(era_f,era5)
}
colnames(era_f) <- c('time','temp')
Sys.setlocale("LC_TIME", "English")
era_f[,date:=as.Date(time,format='%b %d, %Y')]
era_f[,year:=year(date)][,month:=month(date)]
era_f[,temp:=temp-273.13]
era_m <- era_f[,.(temp=mean(temp)),by=.(year,month)]
era_m[,ym:=paste0(year,'-',month)]
c_data<- era_m[r_wd,on=.(ym=yymm)][!is.na(year)]
m1 <- lm(value~temp,c_data)
summary.aov(m1)
####################rainfall
fr <- NULL
for (i in 2005:2019){
  rd <- fread(paste0(path,'Yushan_temp/467550-',i,'.csv'),encoding = 'UTF-8')
as.vector(rd[1,])
colnames(rd) <- as.character(rd[1,])
rd <- rd[ObsTime!='ObsTime']
r <- rd[,.(Y=i,ObsTime,Precp)]
fr <- rbind(fr,r)
}
fr
era5_p <- fread(paste0(path,'Yushan_temp/2005_2019_Yushan_rain.csv'))
colnames(era5_p) <- c('date','pre')
c_r <- cbind(era5_p,fr)
c_r[,Precp:=as.numeric(Precp)][,pre:=pre*1000]
m1 <- lm(Precp~pre,data=c_r)
ggplot(data=c_r,aes(x=pre,y=Precp))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(x="ERA5 precp.",y='Yushan precp.')
ggsave(paste0(path,'Yushan_temp/ERA5vsYushan_rain.jpeg'),width = 6,height = 4,dpi=600)
summary(m1)
