library(data.table)
library(ggplot2)
library(ts)
library(quantreg)
library(gridExtra)
name <- c("Sen","Sun","Yat")
l_path <- "C:/Users/user/Desktop/GLORIA_EVI/plot"
for (i in c(1:3)){
   dt <- fread(paste0(l_path,"/",name[i],"_NDV.csv"))
     assign(paste0(name[i],"_NDV"),dt)
}
temp <- fread("C:/Users/user/Desktop/GLORIA_EVI/temp_lm_result/temp_predict.csv")
NDV <- Sen_NDV[Sun_NDV,on=.(date=date)]
NDV <- NDV[Yat_NDV,on=.(date=date)]
setnames(NDV,c("var_f","i.var_f","i.var_f.1","smo_sp","i.smo_sp","i.smo_sp.1"),
             c("Sen_ndv","Sun_ndv","Yat_ndv","Sen_ndvs","Sun_ndvs","Yat_ndvs")) 
NDV <- NDV[temp,on=.(date=date)]
NDV[,date:=as.Date(date)]
NDV[,yat_temp_sm:=smooth.spline(temp_yat_p)$y][
  ,sun_temp_sm:=smooth.spline(temp_sun_p)$y][
  ,sen_temp_sm:=smooth.spline(temp_sen_p)$y  
  ]
write.csv(NDV,"C:/Users/user/Desktop/NDV.csv")

NDV_m_sm <- NDV[,lapply(.SD,mean),by=format(date,"%y%m"),
    .SDcol=c("sun_temp_sm","Sun_ndvs","sen_temp_sm","Sen_ndvs","yat_temp_sm","Yat_ndvs")]
NDV_m <- NDV[,lapply(.SD,mean),by=format(date,"%y%m"),
            .SDcol=c("temp_sun_p","Sun_ndv","temp_sen_p","Sen_ndv","temp_yat_p","Yat_ndv")]
sun_m_p <- ggplot(data=NDV_m_sm,aes(sun_temp_sm,Sun_ndvs))+
  geom_point()+
  xlim(0,15)+ 
   stat_smooth(method="lm",formula =NDV_m_sm$Sun_ndvs~NDV_m_sm$sun_temp_sm,na.rm=T)
sun_m_p
sen_m_p<- ggplot(data=NDV_m_sm,aes(NDV_m[,2],NDV_m[,3]))+
  geom_point()+
  xlim(0,15)+
  stat_smooth()
yat_m_p<- ggplot(data=NDV_m,aes(temp_yat_p,Yat_ndv))+
  geom_point()+
  xlim(0,15)+
  stat_smooth()
grid.arrange(sun_m_p,sen_m_p,yat_m_p, ncol = 1)
