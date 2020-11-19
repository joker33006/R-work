library(data.table)
library(ggplot2)
library(ggpubr)
library(scales)
w_d_das <- fread('E:/忍者/GLORIA_個人處理/Weather_data/DAS/temp_combin_daily.csv')
w_d_syu <-  fread('E:/忍者/GLORIA_個人處理/Weather_data/SYU/temp_combin_daily.csv')
w_d <- rbind(w_d_das,w_d_syu)
w_dd <- w_d[,.(temp=mean(temp.x),max_t=mean(max_t.x),
               min_t=mean(min_t.x),rain=sum(rain)/4),
            by=.(date,summit,region)]
w_dd[,year:=year(date)]
chilling <- w_dd[temp<5,.N,by=.(year,summit,region)]
chilling[,summit:=factor(summit,levels=c('SEN','TSW','DSH','YAT','JNJ','SUN'))]
fwrite(chilling,"E:/忍者/GLORIA_個人處理/Paper_準備/chilling_day_yearly.csv")
rainbow(6)
ggplot(chilling[year>2009&year!=2020],aes(x=year,y=N,color=summit))+
  geom_line(size=1)+
  theme_classic2()+
  scale_x_continuous(breaks = seq(2008,2020,2))+
  labs(x='Year',y='Chilling days')+
  scale_colour_discrete("")
  #scale_color_manual(values=c('#2D55A6', "#51A4F0", "#F2E641",'#F28F38','#F24738','#912134'))
ggsave('E:/忍者/GLORIA_個人處理/Paper_準備/plot/present/chilling_day_for_SUYU.jpeg',
       width=6,height=3,dpi=600)
ggsave('E:/忍者/GLORIA_個人處理/Paper_準備/plot/chilling_day.jpeg',
       width=8,height=5,dpi=600)
