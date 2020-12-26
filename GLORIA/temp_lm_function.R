library(data.table)
library(ggplot2)
library(ggpubr)
library(wesanderson)
library(broom)
path <- 'E:/忍者/GLORIA_個人處理/paper_準備/'
w_s <- fread("E:/忍者/GLORIA_個人處理/Weather_data/2008-2020_temp_season.csv")
w_y <- fread("E:/忍者/GLORIA_個人處理/Weather_data/2009-2020_temp_year.csv")
season <- c("Winter","Spring","Summer","Fall")
season_model_r <- NULL

colnames(w_y)
at <- w_y[,.(at=mean(temp)),by=.(summit)]
########################lm for temp~year by season
area <- c('SEN','SUN','YAT','DSH','JNJ','TSW')
sea <- c('Winter','Spring','Summer','Fall')
reg <- c('DAS','SYU')
m_result <- NULL
m1_coe <- NULL
for(i in 1:length(area)){
  for(j in 1:length(sea)){
    m1 <- lm(temp~year.s,w_s[season==sea[j]&summit==area[i]&year.s>2009&year.s<2020])
  coe <- summary(m1)
    m1_coe <- cbind(data.table(summit=area[i],season=sea[j],coef.=coe$coefficients[2,1]),
                  glance(m1))
  m_result <- rbind(m_result,m1_coe)
}
}
write.csv(m_result,'E:/忍者/GLORIA_個人處理/paper_準備/summitXseson_model_coefficent.csv')
########################lm for temp~year
area <- c('SEN','SUN','YAT','DSH','JNJ','TSW')
reg <- c('DAS','SYU')
m_result <- NULL
m1_coe <- NULL
for(i in 1:length(area)){
    m1 <- lm(temp~year,w_y[summit==area[i]&year>2009&year<2020])
    coe <- summary(m1)
    m1_coe <- cbind(data.table(summit=area[i],coef.=coe$coefficients[2,1]),
                    glance(m1))
    m_result <- rbind(m_result,m1_coe)
}
write.csv(m_result,'E:/忍者/GLORIA_個人處理/paper_準備/summit_TxY_model_coefficent.csv')

########################################summit

w_y[,summit:=factor(summit,levels=c('SEN','TSW','DSH','YAT','JNJ','SUN'))]
at <- ggplot(data=w_y[year>2009],aes(x=year,y=temp,color=summit))+
  geom_point()+
  scale_x_continuous(breaks = seq(2008,2020,2))+
  geom_smooth(method = lm,size=0.5,alpha=0.1)+
  theme_classic2()+
  labs(x='Year',y='Annual average temperature (°C)',size=18)+
  scale_colour_discrete("") #discrete the line by variable
  #scale_color_brewer(palette="RdBu",direction = -1)
ggsave(plot=at,paste0(paste0(path,'/plot/summit_AT_trend.jpeg')),
       width=6,height=4,dpi=600)

############################################ winter
w_s[,summit:=factor(summit,levels=c('SEN','TSW','DSH','YAT','JNJ','SUN'))]
DAS_season <- ggplot(data=w_s[season=='Winter'&
                                !(year.s<2010|year.s==2020)&
                                summit %in% c('SEN','YAT','SUN')]
                     ,aes(x=year.s,y=temp,color=summit))+
  geom_point()+
  scale_x_continuous(breaks = seq(2008,2020,2))+
  geom_smooth(method = lm,size=0.6,se=T,alpha = 0.1)+
  theme_classic2()+
  labs(x='Year',y='Winter temperature (°C)',title='(a) DAS')+
  scale_colour_discrete("")+
  theme(legend.position="top")#+ #discrete the line by variable
  #scale_color_brewer(palette="RdBu",direction = -1)
SYU_season <- ggplot(data=w_s[season=='Winter'&!(year.s<2010|year.s==2020)&
                                summit %in% c('TSW','DSH','JNJ')],
                     aes(x=year.s,y=temp,color=summit))+
  geom_point()+
  scale_x_continuous(breaks = seq(2008,2020,2))+
  geom_smooth(method = lm,size=0.6,se=T,alpha = 0.1)+
  theme_classic2()+
  labs(x='Year',y='Winter temperature (°C)',title='(b) SYU')+
   theme(legend.position="top")+ #discrete the line by variable
  scale_color_brewer(palette="Dark2",direction = -1)
ggarrange(DAS_season,SYU_season)


ggsave(paste0(paste0(path,'/plot/Season_winter_trend.jpeg')),
       width=8,height=4,dpi=600)

#########################################################


#######################plot the annual temp. bar with standard deviation 

