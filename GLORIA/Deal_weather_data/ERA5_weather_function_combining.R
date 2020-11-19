library(imputeTS)
library(data.table)
library(broom) #get the lm coefficient
library(ggplot2)
library(wesanderson)
library(ggpubr)
Sys.setlocale("LC_TIME", "English")
era_path <- 'E:/忍者/GLORIA_個人處理/paper_準備/weather_ERA5/'
svpath <- 'E:/忍者/GLORIA_個人處理/paper_準備/data_check'
rdata <- fread('E:/忍者/GLORIA_個人處理/Temp_20200809/temp_20200809_corrected.csv')
rdata[,yyddhh:=as.POSIXct(timestamp,format="%Y-%m-%d %H:%M:%S",tz ="" )]
rdata <- rdata[!(id %in% c(181782:181783))]
rdata <- rdata[id!=1031714]
write.csv(rdata,'E:/忍者/GLORIA_個人處理/2020/Temp_20200809/temp_20200809_corrected.csv')
########

######deal the ERA5 weather

a <- fread(paste0(era_path,'DAS/2008_2014_temp_rain.csv'))
b <- fread(paste0(era_path,'DAS/2015_2019_temp_rain.csv'))
c <- fread(paste0(era_path,'DAS/2008_2014_maxT_minT.csv'))
d <- fread(paste0(era_path,'DAS/2015_2019_maxT_minT.csv')) 
era5_tr <- rbind(a,b)
era_mnT <- rbind(c,d)
era5_d <-cbind(era5_tr,era_mnT) 
era5_d <- era5_d[,-4]
colnames(era5_d) <- c('date','temp','rain','max_t','min_t')

era5_d[,date:=as.Date(date,format='%b %d, %Y')][
  ,temp:=temp-273.15][
  ,rain:=rain*1000][
  ,max_t:=max_t-273.15][
  ,min_t:=min_t-273.15]
head(era5_d)
write.csv(era5_d,'E:/忍者/GLORIA_個人處理/Temp_20200809/DAS_ERA5_daily.csv')
##############################################
#############################counting the daily mean
GLA_temp_d <- function(reg,rdata){
t_h <- rdata[region==reg]
daily <- t_h[,.(temp=mean(temperature),max_t=max(temperature),
                    min_t=min(temperature),n=.N),
                 by=.(region,summit,direction,year,month,day,datalogger)]
daily[,date:=as.Date(paste(year,month,day,sep='-'))]
daily <- daily[n>23]#刪除未滿24小時的資料
d_com <- daily[,.(temp=mean(temp),max_t=max(max_t),
                          min_t=min(min_t),n=.N),
                       by=.(region,summit,direction,date)]
return(d_com)
}
Das_d <- GLA_temp_d('DAS',rdata)
setkey(Das_d,date)
setkey(era5_d,date)
Das_era <- Das_d[era5_d]
write.csv(Das_era,'E:/忍者/GLORIA_個人處理/Temp_20200809/DAS/cobim_w_era_data.csv')
#######################################################
############ regression and filling the miss value of temp
S <- c('SEN','SUN','YAT')
reg <- "DAS" #test
wdata <- Das_era #test
#the wdata is the Das_era, 
#inculding the datalogger daily avg., max, and min temp. 
#Also, combining the ERA5 daily temp.
lm_era_real <- function(reg,wdata,era5_d,S){ 
m_fr <- NULL
w_result <- NULL
for (i in 1:3){
  rd <- wdata[summit==S[i]]
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
    ################ merge the rdata and predict
    pre <- merge(r2[,1:7],era5_d,on=.(date=date),all=T)
    setnames(pre,c('temp.y','max_t.y','min_t.y'),c('i.temp','i.max_t','i.min_t'))
    pre[,temp.p:=predict(m_t,pre)][,max_t.p:=predict(m_maxT,pre)][,min_t.p:=predict(m_minT,pre)]
    pre[,summit:=S[i]][,direction:=dir[j]][,region:=reg]
    pre[is.na(temp.x),c('temp.x','type'):=.(temp.p,"p") ]
    pre[is.na(min_t.x),c('min_t.x','max_t.x'):=.(min_t.p,max_t.p)]
    pre[,10:14:=NULL]
    pre[,8:=NULL]
    w_result <- rbind(w_result,pre)

    }#finished j looping
} #i looping
return(list(m_fr,w_result))
}
result <- lm_era_real(reg,Das_era,era5_d,S)

write.csv(result[2],'E:/忍者/GLORIA_個人處理/Temp_20200809/DAS/temp_combin_daily.csv')
write.csv(result[1],'E:/忍者/GLORIA_個人處理/Temp_20200809/DAS/temp_model_coefficent.csv')
############################################################
##################### long-term trender of weather 
w_d_das <- fread('E:/忍者/GLORIA_個人處理/weather_data/DAS/temp_combin_daily.csv')
w_d_syu <-  fread('E:/忍者/GLORIA_個人處理/weather_data/SYU/temp_combin_daily.csv')
w_d <- rbind(w_d_das,w_d_syu)
w_dd <- w_d[,.(temp=mean(temp.x),max_t=mean(max_t.x),
               min_t=mean(min_t.x),rain=sum(rain)/4),
            by=.(date,summit,region)]

#area <- c('DSH','JNJ','TSW')

area <- c('SEN','SUN','YAT','DSH','JNJ','TSW')
temp.sm <- NULL
for (i in 1:length(area)){
  t <- as.data.table(loess(temp~as.numeric(date),data=w_dd[summit==area[i]],span=0.01)$fit)
  temp.sm <- rbind(temp.sm,t)
}
colnames(temp.sm) <- 'temp.sm'
w_dd[,temp.sm:=NULL]
w_dd <- cbind(w_dd,temp.sm)
w_dd[,year:=year(date)][,month:=month(date)]
#w_dd <- w_dd[year!=2008]
w_dd[month%in%3:5,season:='Spring'][month%in%6:8,season:='Summer'][month%in%9:11,season:='Fall'][is.na(season),season:='Winter']
w_y <- w_dd[,.(temp=mean(temp),temp_sd=sd(temp),
               max_t=mean(max_t),max_t_sd=sd(max_t),
               min_t=mean(min_t),min_t_sd=sd(min_t),
               rain=sum(rain)),
            by=.(year,summit,region)] #for year
w_dd[,year.s:=year]
w_dd[month==12,year.s:=year+1] # December was the winter group of next year

w_s <- w_dd[,.(temp=mean(temp),max_t=mean(max_t),
               min_t=mean(min_t),rain=sum(rain)),
            by=.(year.s,season,summit,region)] #for season
w_s <- w_s[!year.s==2020]
w_y <- w_y[!((year==2008|year==2019)&region=='DAS')][
          !((year<2010|year==2020)&region=='SYU')] #Excluding the incomplete data
w_m <-w_dd[,.(temp=mean(temp),temp_sd=sd(temp),
                  max_t=mean(max_t),max_t_sd=sd(max_t),
                  min_t=mean(min_t),min_t_sd=sd(min_t),
                  rain=sum(rain)),
               by=.(year,month,summit,region)]
write.csv(w_s,"E:/忍者/GLORIA_個人處理/Temp_20200809/2008-2020_temp_season.csv")
write.csv(w_y,"E:/忍者/GLORIA_個人處理/Temp_20200809/2009-2020_temp_year.csv")
write.csv(w_m,"E:/忍者/GLORIA_個人處理/Temp_20200809/2009-2020_temp_month.csv")
