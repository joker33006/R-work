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

########################lm for temp~year
area <- c('SEN','SUN','YAT','DSH','JNJ','TSW')
sea <- c('Winter','Spring','Summer','Fall')
reg <- c('DAS','SYU')
m_result <- NULL
m1_coe <- NULL
m1 <- lm(temp~year.s,w_s[summit=='DSH'&season=='Winter'&year.s>2009&year.s<2020])
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

########################################summit
w_y[,summit:=factor(summit,levels=c('SEN','TSW','DSH','YAT','JNJ','SUN'))]
at <- ggplot(data=w_y[!(year<2010|year==2020)],aes(x=year,y=temp,color=summit))+
  geom_point()+
  scale_x_continuous(breaks = seq(2008,2020,2))+
  geom_smooth(method = lm,size=0.5)+
  theme_classic2()+
  labs(x='Year',y='Annual average temperature (°C)')+
  scale_colour_discrete("") #discrete the line by variable
  #scale_color_brewer(palette="RdBu",direction = -1)
ggsave(plot=at,paste0(paste0(path,'/plot/summit_AT_trend.jpeg')),
       width=8,height=5,dpi=600)
ggsave(plot=at,paste0(paste0(path,'/plot//present/summit_AT_trend_for_SUYU.jpeg')),
       width=6,height=3,dpi=600)
############################################ winter
w_s[,summit:=factor(summit,levels=c('SEN','TSW','DSH','YAT','JNJ','SUN'))]
t_season_w <- ggplot(data=w_s[season=='Winter'&!(year.s<2010|year.s==2020)],aes(x=year.s,y=temp,color=summit))+
  geom_point()+
  scale_x_continuous(breaks = seq(2008,2020,2))+
  geom_smooth(method = lm,size=0.5)+
  theme_classic2()+
  labs(x='Year',y='Winter temperature (°C)')+
  scale_colour_discrete("")#+ #discrete the line by variable
  #scale_color_brewer(palette="RdBu",direction = -1)
  

ggsave(plot=t_season_w,paste0(paste0(path,'/plot/Season_winter_trend.jpeg')),
       width=8,height=5,dpi=600)
ggsave(plot=t_season_w,paste0(paste0(path,'/plot/present/Season_winter_trend_for_SUYU.jpeg')),
       width=6,height=3,dpi=600)
#########################################################


#######################plot the annual temp. bar with standard deviation 

