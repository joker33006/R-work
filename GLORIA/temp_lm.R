library(data.table)
library(ggplot2)
gpath <- "C:/Users/user/Desktop/GLORIA_EVI/gloria_soil_temp_2008-2019.csv"
Td <- fread(gpath)
rtemp <- fread("C:/Users/user/Desktop/GLORIA_EVI/plot/Sun_Tem.csv")
colnames(Td)
rtemp <- rtemp[,date:=as.Date(time)]
dt_das <- Td[region=="DAS",]
# caculate the temperature average by summit, year, month, and day.
dt_mean <- dt_das[,lapply(.SD,mean),
       by=c("summit","year","month","day"),
       .SDcols="temperature"][,date:=as.Date(paste(year,month,day,sep="-"))]
Tr <- dt_mean[rtemp,on=.(date=date)][temperature!="NA",][,temp_c_s:=Tem-273.15]
r_yat <-Tr[summit=="YAT",] 
r_sun <- Tr[summit=="SUN",]
r_sen <- Tr[summit=="SEN",]
plot(r_sun$temperature,r_sun$temp_c_s)
m_yat <- lm(temperature~temp_c_s,data=r_yat)
m_sun <- lm(temperature~temp_c_s,data=r_sun)
m_sen <- lm(temperature~temp_c_s,data=r_sen)
rtemp[,temp_c_s:=Tem-273.15]
result <- rtemp[,temp_yat_p:=predict(m_yat,rtemp)][
  ,temp_sun_p:=predict(m_sun,rtemp)][
  ,temp_sen_p:=predict(m_sen,rtemp)][
  ,temp_c:=NULL]
svpath <- "C:/Users/user/Desktop/GLORIA_EVI/temp_lm_result"
sink(file=paste0(svpath,"/m_yat.txt"))
print(summary(m_yat))
sink()
sink(file=paste0(svpath,"/m_sun.txt"))
print(summary(m_sun))
sink()
sink(file=paste0(svpath,"/m_sen.txt"))
print(summary(m_sen))
sink()
write.csv(result,paste0(svpath,"/temp_predict.csv"))
