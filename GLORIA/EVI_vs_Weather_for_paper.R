library(data.table)
library(ggplot2)
library(lme4)
library(broom)
library(sjstats)
path <- 'E:/忍者/GLORIA_個人處理/paper_準備/'
EVI <- fread(paste0(path,'EVI/EVI_ts_date_2003_2020.csv'))
weather <- fread('E:/忍者/GLORIA_個人處理/Weather_data/2009-2020_temp_month.csv')
colnames(EVI)
colnames(weather)
EVI_m <- EVI[,.(EVI=mean(EVI)),by=.(summit,month,year,Season)]
rd <- EVI_m[weather,on=.(year=year,month=month,summit=summit),all=F]
rd <- rd[year!=2020]
rd[month %in% 3:5,season:='Spring'][
  month %in% 6:8,season:='Summer'][
    month %in% 9:11,season:='Fall'][
      is.na(season),season:='Winter']
rd_y <- rd[,.(EVI=mean(EVI),temp=mean(temp),rain=sum(rain)),by=.(year,summit)]
rd_s <- rd[,.(EVI=mean(EVI),temp=mean(temp),rain=sum(rain)),by=.(year,summit,season)]
#########

s <- c('Spring','Summer','Fall','Winter')


r <- NULL
for (i in 1:4){

  gm1 <- glmer(EVI~temp+log(rain)+temp+(1|summit),data=rd_s[season==s[i]],family = Gamma(link='log'))
  Results <- capture.output(summary(gm1))
 r <- rbind(r,cbind(s[i],Results[21:25]))
 performance::r2(gm1)
summary(gm1)
}
write.csv(r,paste0(path,"GLMM_seasonal_EVI_temp_lograin.csv"))
  ggplot(rd_s[season=='Spring'],aes(x=temp,y=EVI,color=summit))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(summit~.,cols=2)+
  theme_classic()+
  labs(x='temperature (°C)')
#######################season delay
####### test the quadratic term 
r <- NULL
    rd_s[,temp_q:=temp^2][,rain_q:=rain^2] 
    
    for (i in 1:4){
    gm1 <- glmer(EVI~temp+temp_q+log(rain)*temp+(1|summit),
                data=rd_s[season=='Spring'],family = Gamma(link='log'))
  summary(gm1)
   Results <- capture.output(summary(gm1))
  r <- rbind(r,cbind(s[i],Results[c(8:9,21:27)]))
  }
    write.csv(r,paste0(path,"GLMM_seasonal_EVI_temp_test_3_temp^2.csv"))
