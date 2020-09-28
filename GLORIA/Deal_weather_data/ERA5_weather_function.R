library(imputeTS)
library(data.table)
library(broom) #get the lm coefficient
library(ggplot2)
library(wesanderson)
library(ggpubr)
Sys.setlocale("LC_TIME", "English")
era_path <- 'E:/忍者/GLORIA_個人處理/paper_準備/weather_ERA5/'
svpath <- 'E:/忍者/GLORIA_個人處理/paper_準備/data_check'
rdata <- fread('E:/忍者/GLORIA_個人處理/2020/Temp_20200809/temp_20200809_corrected.csv')
rdata[,yyddhh:=as.POSIXct(timestamp,format="%Y-%m-%d %H:%M:%S",tz ="" )]
rdata <- rdata[!(id %in% c(181782:181783))]
rdata <- rdata[id!=1031714]
write.csv(rdata,'E:/忍者/GLORIA_個人處理/2020/Temp_20200809/temp_20200809_corrected.csv')
########
#######初步繪圖，檢查溫度資料
temp_dir <- function(s,w_date, svpath){
  Sys.setlocale("LC_TIME", "English") 
  library(data.table)
  library(ggplot2)
  rdata <- w_date
  prdata <- rdata[summit==s]
  n <- length(unique(prdata[,datalogger]))
  p <- ggplot(prdata,aes(yyddhh,temperature,color=direction))+
    geom_line()+
    facet_grid(datalogger ~ .)
  ggsave(paste0(svpath,'/logger_',s,'.jpeg'),
         plot =p,width=20,height=n*3,dpi=300)
  p_d <- ggplot(prdata,aes(yyddhh,temperature,color=direction))+
    geom_line()+
    facet_grid(direction ~ .)
  ggsave(paste0(svpath,'/dir_',s,'.jpeg'),
         plot =p_d,width=20,height=n*3,dpi=300)
}
summit <- c('SEN','SUN','YAT')
for(i in 1:3){
  temp_dir(summit[i],rdata,svpath)
}
####################
###########review
library(ggplot2)
library(scales)
YAT<- rdata[summit=='YAT']
unique(YAT[,datalogger])
ggplot(YAT[year ==2011&direction=='S'&month==12],aes(yyddhh,temperature,color=direction,))+
  geom_line()+
  facet_grid(direction ~ .)+
  scale_x_datetime(date_labels = '%Y-%m-%d')
check <- YAT[year==2011&month==12&day%in%c(25:27)&direction=='S']
write.csv(check,paste0(svpath,'/check.csv'))
ggsave(paste0('E:/忍者/GLORIA_個人處理/2020/Temp_20200809/checking_plot/TSW_test.jpeg'),
       width=20,height=n*3,dpi=300)
#################################
######deal the ERA5 weather

a <- fread(paste0(era_path,'DAS/2008_2014_temp_rain.csv'))
b <- fread(paste0(era_path,'DAS/2015_2019_temp_rain.csv'))
c <- fread(paste0(era_path,'DAS/2008_2014_maxT_minT.csv'))
d <- fread(paste0(era_path,'DAS/2015_2019_maxT_minT.csv')) 
era5_tr <- rbind(a,b)
era_mnT <- rbind(c,d)
era5_d <-cbind(era5_tr,era_mnT) 
era5_d <- era5_d[,-4]
colnames(era5_d) <- c('date','temp','rain','maxT','minT')

era5_d[,date:=as.Date(date,format='%b %d, %Y')][
  ,temp:=temp-273.15][
  ,rain:=rain*1000][
  ,maxT:=maxT-273.15][
  ,minT:=minT-273.15]
head(era5_d)
write.csv(era5_d,'E:/忍者/GLORIA_個人處理/2020/Temp_20200809/DAS_ERA5_daily.csv')
##############################################
#############################counting the daily mean
GLA_temp_d <- function(reg,rdata){
t_h <- rdata[region==reg]
daily <- t_h[,.(temp=mean(temperature),max_t=max(temperature),
                    min_t=min(temperature),n=.N),
                 by=.(region,summit,direction,year,month,day,datalogger)]
daily[,date:=as.Date(paste(year,month,day,sep='-'))]
daily <- daily[n>23]#刪除未滿24小時的資料

write.csv(daily,'E:/忍者/GLORIA_個人處理/2020/Temp_20200809/SYU/daily_temp_data_by_logger.csv')
d_com <- daily[,.(temp=mean(temp),max_t=max(max_t),
                          min_t=min(min_t),n=.N),
                       by=.(region,summit,direction,date)]
return(d_com)
}
setkey(SYU_d_com,date)
setkey(era5_d,date)
syu_era <- SYU_d_com[era5_d]
write.csv(syu_era,'E:/忍者/GLORIA_個人處理/2020/Temp_20200809/SYU/cobim_w_data.csv')
#######################################################
############ regression and filling the miss value of temp
S <- c('DSH','JNJ','TSW')

m_fr <- NULL
w_result <- NULL
for (i in 1:3){
  rd <- syu_era[summit==S[i]]
  dir <- unique(rd[,direction])
  for (j in 1:length(unique(dir))){
    r2 <- rd[direction==dir[j],]
    m_t <- lm(temp~i.temp,data=r2)
    m_maxT <- lm(max_t~i.max_t,data = r2)
    m_minT<- lm(min_t~i.min_t,data = r2)
    ######save the model coefficient
    m_r <-cbind(data.table(model="Temp",summit=S[i],direction=dir[j]),glance(m_t)[1:6]) #glance from the package "broom"
    m_maxr <-cbind(data.table(model="max_t",summit=S[i],direction=dir[j]),glance(m_maxT)[1:6])
    m_minr <-cbind(data.table(model="min_t",summit=S[i],direction=dir[j]),glance(m_minT)[1:6])
    m_fr <- rbind(m_fr,m_r,m_maxr,m_minr)
    ################
    pre <- merge(r2[,1:7],era5_d,on=.(date=date),all=T)
    setnames(pre,c('temp.y','max_t.y','min_t.y'),c('i.temp','i.max_t','i.min_t'))
    pre[,temp.p:=predict(m_t,pre)][,max_t.p:=predict(m_maxT,pre)][,min_t.p:=predict(m_minT,pre)]
    pre[,summit:=S[i]][,direction:=dir[j]][,region:='SYU']
    pre[is.na(temp.x),c('temp.x','type'):=.(temp.p,"p") ]
    pre[is.na(min_t.x),c('min_t.x','max_t.x'):=.(min_t.p,max_t.p)]
    pre[,10:14:=NULL]
    pre[,8:=NULL]
    w_result <- rbind(w_result,pre)
  }
}
write.csv(w_result,'E:/忍者/GLORIA_個人處理/2020/Temp_20200809/SYU/temp_combin_daily.csv')
write.csv(m_fr,'E:/忍者/GLORIA_個人處理/2020/Temp_20200809/SYU/temp_model_coefficent.csv')
############################################################
##################### long-term trender of weather 
w_d <- fread('E:/忍者/GLORIA_個人處理/2020/Temp_20200809/SYU/temp_combin_daily.csv')
w_dd <- w_d[,.(temp=mean(temp.x),max_t=mean(max_t.x),
               min_t=mean(min_t.x),rain=sum(rain)/4),
            by=.(date,summit)]
area <- c('DSH','JNJ','TSW')
temp.sm <- NULL
for (i in 1:3){
  t <- as.data.table(loess(temp~as.numeric(date),data=w_dd[summit==area[i]],span=0.01)$fit)
  temp.sm <- rbind(temp.sm,t)
}
colnames(temp.sm) <- 'temp.sm'
w_dd[,temp.sm:=NULL]
w_dd <- cbind(w_dd,temp.sm)
w_dd[,year:=year(date)][,month:=month(date)]
w_dd <- w_dd[year!=2008]
w_dd[month%in%3:5,season:='Spring'][month%in%6:8,season:='Summer'][month%in%9:11,season:='Fall'][is.na(season),season:='Winter']
w_y <- w_dd[,.(temp=mean(temp),max_t=mean(max_t),
               min_t=mean(min_t),rain=sum(rain)),
            by=.(year,summit)] #for year
w_y <- w_y[year!=2009]
w_y <- w_y[year!=2020]
w_dd[,year.s:=year]
w_dd[month==12,year.s:=year+1] # December was the winter group of next year

w_s <- w_dd[,.(temp=mean(temp),max_t=mean(max_t),
               min_t=mean(min_t),rain=sum(rain)),
            by=.(year.s,season,summit)] #for year
w_s <- w_s[year.s>2009][year.s!=2020]
write.csv(w_s,"E:/忍者/GLORIA_個人處理/2020/SYU_EVIandcliment/temp_season.csv")
ggplot(w_y,aes(x=year,color=summit))+
  geom_line(aes(y=temp),size=1)+
  labs(x='Year',y='Mean annual temperature (°C)')+
  theme_classic()+
  scale_x_continuous(breaks = seq(2010,2020,2))
ggsave(paste0(paste0(path,'SYU/plot/Temp_year_avg.jpeg')),
       width=5,height=4,dpi=300)

ggplot(w_y,aes(x=year,color=summit))+
  geom_line(aes(y=max_t),size=1)+
  labs(x='Year',y='Maximum annual temperature (°C)')+
  theme_classic()+
  scale_x_continuous(breaks = seq(2010,2020,2))
ggsave(paste0(paste0(path,'SYU/plot/Temp_year_avgmax.jpeg')),
       width=5,height=4,dpi=300)

ggplot(w_y,aes(x=year,color=summit))+
  geom_line(aes(y=min_t),size=1)+
  labs(x='Year',y='Minimum annual temperature (°C)')+
  theme_classic()+
  scale_x_continuous(breaks = seq(2010,2020,2))
ggsave(paste0(paste0(path,'SYU/plot/Temp_year_avg_min.jpeg')),
       width=5,height=4,dpi=300)

w.era.m <- fread("E:/忍者/GLORIA_個人處理/2020/Temp/ERA5/SYU/DSH/1979_2020_monthly_weather.csv")
colnames(w.era.m) <- c("date",'Temp','rain')
w.era.m[,date:=as.Date(date,format="%b %d, %Y")]
w.era.m[,year:=year(date)]
w.era.m[,month:=month(date)]
w.era.m[month%in%3:5,season:='Spring'][
  month%in%6:8,season:='Summer'][
    month%in%9:11,season:='Fall'][
      is.na(season),season:='Winter']
w.era.m[,year.s:=year][month==12,year.s:=year+1]
w.era_1819 <- w.era.m[year==2018|year==2019,.(pre=sum(rain)),b=.(year,month)]
w.era_1819[,date:=as.Date(paste(year,month,"1",sep="-"))]
era_rain <- w.era.m[,.(pre=sum(rain)),by=year]
era_rain_season <- w.era.m[,.(pre=sum(rain)),by=.(year.s,season)]
avg <- mean(era_rain[,pre])*1000
ggplot(w.era_1819[year==2018],aes(x=date,y=pre*1000))+
  geom_col( width = 0.2,color='black')+
  geom_line(y=avg,linetype='dashed',color='gray50')+
  labs(x='Year',y='Precipitation (mm)')+
  theme_classic()#+
ggsave(paste0(path,'SYU/plot/rain_year_1979_2019.jpeg'),
       width=10,height=3,dpi=300)
write.csv(era_rain_season,"E:/忍者/GLORIA_個人處理/2020/SYU_EVIandcliment/rain_season.csv")
#########plot the season trend

s <- c('Winter',"Spring",'Summer','Fall')
####### temp avg

for (i in 1:4){
  ggplot(w_s[season==s[i]],aes(x=year.s,color=summit))+
    geom_line(aes(y=temp),size=1)+
    labs(x='Year',y='Mean temperature (°C)')+
    theme_classic2()+
    scale_x_continuous(breaks = seq(2010,2020,2))
  
  ggsave(paste0(path,'SYU/plot/temp_avg_',s[i],'.jpeg'),
         width=5,height=3,dpi=300)
  
}
######plot the precipitation
for (i in 1:4){
  i=4
  avg <- era_rain_season[season==s[i],mean(pre*1000)]
  
  ggplot(w_s[season==s[i]&summit=="TSW"],aes(x=year.s,y=rain))+
    geom_col( width = 0.5,fill='cyan4')+
    labs(x='Year',y='Precipitation (mm)')+
    geom_line(y=avg,linetype='dashed',color='gray50')+
    theme_classic()+
    scale_x_continuous(breaks = seq(2010,2020,2))
  
  ggsave(paste0(path,'SYU/plot/rain_avg_',s[i],'.jpeg'),
         width=4,height=3,dpi=300)
}
########################################################
#########plot


ggplot(w_result[summit=='JNJ'&is.na(type)],aes(x=date,y=temp.x,color=direction))+
  geom_line()+
  facet_grid(direction ~ .)+ 
  theme_classic()+
  labs(x="Date",y='Temperature(°C)')+
  xlim(c(as.Date('2012-01-01'),as.Date('2012-12-31')))

