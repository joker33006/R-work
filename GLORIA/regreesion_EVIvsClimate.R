library(data.table)
library(ggplot2)
library(drc)
rdata <-fread("C:/Users/user/Desktop/GLORIA_EVI/EVI_Temp_Pre_weekly.csv") 
sen_m <- lm(SEN.sm~sen_avg,data=rdata)
R_sq <- expression('R^2')
label <- paste0("y=",round(sen_m$coefficients[2],3),"x+",round(sen_m$coefficients[1],3))
label2 <- paste0(R_sq,"=",round(summary(sen_m)$r.squared,3)," p<0.001")
ggplot(rdata,aes(sen_avg,SEN.sm))+
  geom_point()+
  geom_smooth(method='lm', formula= y~x^2)+
  annotate("text", x=2, y=0.25, label=label)+
  annotate("text", x=2, y=0.23, label=label2)


rdata[,SEN_pevi:=predict(mod_sen,rdata)]
