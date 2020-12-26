library(data.table)
library(agricolae)
library(vegan)
library(ggplot2)
library(iNEXT)
library(ggpubr)
library(tidyverse)
library(rstatix)#for ANOVA
library(patchwork)###combine the plot
library(broom)
path <- 'E:/忍者/GLORIA_個人處理/Paper_準備/'
rd <- fread('E:/忍者/GLORIA_個人處理/Paper_準備/2008_2020_DAS_SYU_p5p10_BBq.csv')
colnames(rd)
rd_s<- dcast(rd, year+summit+section~N_code,value.var = 'code',drop=FALSE,fun.aggregate = mean)

for (j in names(rd_s)){
  set(rd_s,which(is.nan(rd_s[[j]])),j,0)}

cha_r <- rd_s[,s:=apply(.SD,1,sum),.SDcols=patterns('SP')]
cha_r[s>0,chao1:=apply(.SD,1,function(x){ChaoRichness(x)$Estimator}),.SDcols=patterns('SP')]
cha1 <- cha_r[,chao1]

H <- diversity(rd_s[,4:ncol(rd_s)])
N <- specnumber(rd_s[,4:ncol(rd_s)])
D <- diversity(rd_s[,4:ncol(rd_s)],index='simpson')
E <-  H/log(N)
r <- cbind(rd_s[,1:3],N,H)
r <- r[N!=0]
######nomality test
r[,year:=as.character(year)]
d_nom_result <- r %>% group_by(year,summit)%>%shapiro_test(N)
d_nom_result <- r %>% group_by(year,summit)%>%shapiro_test(H)%>%rbind(d_nom_result)
######homogneity test
r[,group:=as.factor(paste0(year,'_',summit))]

Homog_result <-r %>% group_by(summit)%>%
  levene_test(N~group)%>%cbind(ind='N')
Homog_result <- r%>%  group_by(summit)%>%
  levene_test(H~group)%>%
  cbind(ind='H')%>%
  rbind(Homog_result)
Summ <- 'JNJ'

ggqqplot(r[summit==Summ&year==2020]$N)
ggsave(paste0(path,'plot/QQplot_N_',Summ,'.jpeg'),
       width = 4,height = 3,dpi=300)
fwrite(d_nom_result,'E:/忍者/GLORIA_個人處理/Paper_準備/diversity_nom_test.csv')
fwrite(Homog_result,'E:/忍者/GLORIA_個人處理/Paper_準備/diversity_HOMO_test.csv')

r<- melt(r,
            id.vars = c("year",'summit','section'),#保留之參數
            measure.vars = c('N','H'), #轉置之名稱，可用pattern+regx 做查找
            variable.name = "index") #給予新參數名稱




D_year <- r[,.(div=mean(value),div_sd=sd(value)),
             by=.(year,summit,index)]
div_ind <- c('N','H')

s <- c('SEN','SUN','YAT','DSH','JNJ','TSW')

c_r <- NULL
for (j in 1:2){#for index
for (i in 1:6){#for summit
  d <- aov(value~year,data=r[summit==s[i]&index==div_ind[j]])
  tuk <- HSD.test(d,'year')
rownames(tuk$groups)
cls <- as.data.table(cbind(data.table(summit=s[i],index=div_ind[j],year=rownames(tuk$groups),tuk$group)))
c_r <- rbind(c_r,cls)
  }

}
head(c_r)
c_r[,year:=as.character(year)]
D_year <- D_year[c_r,on=.(year=year,summit=summit,index=index)]

D_year[,year:=factor(year)]
D_year[year%in% c(2008,2013,2019),region:='DAS'][year%in% c(2009,2014,2020),region:='SYU']
phase <- data.table(year=c(2008,2009,2013,2014,2019,2020),period=rep(c('P1','P2','P3'),each=2))
phase[,year:=as.factor(year)]
D_year <- D_year[phase, on=.(year=year)]
write.csv(D_year,'E:/忍者/GLORIA_個人處理/paper_準備/Diversity_DAS_SYU.csv')
########################end
 ####analyze the richness between summit
year_c <- list(c(2008,2009),c(2013,2014),c(2019,2020))
P <- c('2008-2009','2013-2014','2019-2020')
alt <- data.table(summit=c('SEN','SUN','YAT','DSH','JNJ','TSW'),
                     alt=c(3610,3255,3363,3509,3299,3524),
                  avg_t=c(8.152,10.376,9.356,8.213,9.645,7.479))
r <- r[alt,on=.(summit=summit)]
c_r <- NULL
m_r <- NULL
ct_r <- NULL
mt_r <- NULL
for(j in 1:2){ 
  for (y in 1:3){
    #####################diversity~altitude
    d <- lm(value~alt,data=r[year%in%year_c[[y]]&index==div_ind[j]])
    summary(d)$coefficients
    m_cof <- cbind(period=P[y],index=div_ind[j],glance(d))
    m_r <- rbind(m_r,m_cof)
    cls <- cbind(data.table(period=P[y],index=div_ind[j],
                parameter=rownames(summary(d)$coefficients)),summary(d)$coefficients)
    c_r <- rbind(c_r,cls)
    #################### diversity~average temp
    dt <- lm(value~avg_t,data=r[year%in%year_c[[y]]&index==div_ind[j]])
    summary(dt)$coefficients
    mt_cof <- cbind(period=P[y],index=div_ind[j],glance(dt))
    mt_r <- rbind(mt_r,mt_cof)
    ctls <- cbind(data.table(period=P[y],index=div_ind[j],
                            parameter=rownames(summary(dt)$coefficients)),summary(dt)$coefficients)
    ct_r <- rbind(ct_r,ctls)
}
}
write.csv(c_r,'E:/忍者/GLORIA_個人處理/paper_準備/Diversity_altitude_lm_cof.csv')
write.csv(m_r,'E:/忍者/GLORIA_個人處理/paper_準備/Diversity_altitude_lm.csv')
write.csv(ct_r,'E:/忍者/GLORIA_個人處理/paper_準備/Diversity_temp_lm_cof.csv')
write.csv(mt_r,'E:/忍者/GLORIA_個人處理/paper_準備/Diversity_temp_lm.csv')
width <- 0.5 #difine the bar width

reg <- c('DAS','SYU')
v <- c(-5,-3)
for (j in 1:2){
for (i in 1:2){
  ggplot(D_year[index==div_ind[j]&region==reg[i]],aes(x=summit,y=div, fill=period))+
  geom_bar(stat="identity", position=position_dodge(),width = width)+
  geom_errorbar(aes(ymin=div-div_sd, ymax=div+div_sd), position = position_dodge(width =width),width = 0.2)+
  theme_classic2()+
  geom_text(aes(label=groups, group=year),
            position=position_dodge(width=width),vjust=v[j])+
  ylim(c(0,max(D_year[index==div_ind[j],div+div_sd])*1.2))+
  labs(y=div_ind[j],x=NULL,size=14)+
  theme(legend.position = 'none')
ggsave(paste0('E:/忍者/GLORIA_個人處理/Paper_準備/plot/2008_2019_',reg[i],'_dindex',div_ind[j],'.jpeg'),
       width=4,height=3,dpi=600)
 }
}  
################get the legend 
p <- ggplot(D_year[index==div_ind[j]&region==reg[i]],aes(x=summit,y=div, fill=period))+
  geom_bar(stat="identity", position=position_dodge(),width = width)+
  geom_errorbar(aes(ymin=div-div_sd, ymax=div+div_sd), position = position_dodge(width =width),width = 0.2)+
  theme_classic2()+
  geom_text(aes(label=groups, group=year),
            position=position_dodge(width=width),vjust=-3)+
  ylim(c(0,max(D_year[index==div_ind[j],div+div_sd])*1.2))+
  labs(y=div_ind[j],fill='Period')+
  theme(plot.title = element_text(size=16,hjust = 0.05,vjust=-4),legend.position="top")



legend <- get_legend(p)
ggsave('E:/忍者/GLORIA_個人處理/paper_準備/plot/legend_Species_diversity.jpeg',
       plot=legend,width=5,height=2,dpi=600)
