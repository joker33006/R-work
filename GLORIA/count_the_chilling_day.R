library(data.table)
library(ggplot2)
library(ggpubr)
library(scales)
library(tidyverse)
library(rstatix)#for ANOVA
w_d <- fread('E:/忍者/GLORIA_個人處理/weather_data/資料填補基本結果/temp_combin_daily.csv')
w_dd <- w_d[,.(temp=mean(temp)),
            by=.(date,summit,region)]
w_dd[,year:=year(date)]
chilling <- w_dd[temp<5,.N,by=.(year,summit,region)]
chilling[,summit:=factor(summit,levels=c('SEN','TSW','DSH','YAT','JNJ','SUN'))]
chilling <- chilling[year>2009&year!=2020][
  year%in%2010:2014,group:='A'][year%in%2015:2019,group:='B']
chil_mean <-chilling[,.(day=mean(N),day_sd=sd(N)),by=.(summit,region,group)] 
fwrite(chilling,"E:/忍者/GLORIA_個人處理/Paper_準備/chilling_day_yearly.csv")
######nomality and homogeneity test
chilling[,group_S:=as.factor(paste0(group,"_",summit))]
chill_test_result <- chilling %>%
  group_by(group,summit) %>%
  shapiro_test(N) 
chill_Homogeneity_result <- chilling %>%
  levene_test(N~group_S)
t_tes_r <- chilling%>%group_by(summit)%>%
  t_test(N~group)%>%as.data.table()
t_tes_r[p<0.05,sign:='*']
fwrite(chill_test_result,"E:/忍者/GLORIA_個人處理/Paper_準備/chill_nom_test_result.csv")
fwrite(chill_Homogeneity_result,"E:/忍者/GLORIA_個人處理/Paper_準備/chill_homo_test_result.csv")
label.df <- data.table(summit = c('SEN','TSW','DSH','YAT','JNJ','SUN'),
                       day= c(100,130,120,80,85,60))

ggplot(chil_mean,aes(x=summit,y=day))+
  geom_bar(aes(fill=group),stat="identity", position=position_dodge(),width = 0.8)+
  geom_errorbar(aes(fill=group,ymin=day-day_sd, ymax=day+day_sd), position = position_dodge(width = 0.8),width = 0.2)+
  theme_classic2()+
  labs(fill = "Period")+
  labs(x='Year',y='Chilling days')+
  scale_fill_discrete(labels=c('2010-2014','2015-2019'))+
  #geom_text(data =label.df, label = c('ns','ns','ns','*','*','*'))
  geom_text(data =label.df, label = c('ns','ns','ns','p=0.006','p=0.038','p=0.013'))

ggsave('E:/忍者/GLORIA_個人處理/Paper_準備/plot/chilling_day_t_test.jpeg',
       width=6,height=4,dpi=600)
