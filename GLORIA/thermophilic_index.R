library(data.table)
library(ggplot2)
path <- 'E:/忍者/GLORIA_個人處理/GBIF_data/'
#load the name_list
name_list <- fread(paste0(path,'Surveydata_and_niche/name_list.csv'))
#load the niche
w_nich <- fread(paste0(path,'niche/bio_niche.csv'))
wi <- fread(paste0(path,'niche/WI_niche.csv'))
colnames(wi)[4:8] <- paste0('wi_',colnames(wi)[4:8])
nich <- w_nich[wi,on=.(code=code,name=scientificname),all=T]
#combine the niche and namelist
base_d <- nich[name_list,on=.(code=GBIF_code),all=T]
base_d[,i.V1:=NULL][,i.code:=NULL]
setnames(base_d,'i.code.1','S_code')
#load the survey data
q_plot <- fread(paste0(path,'Surveydata_and_niche/SYU_DAS_1X1_total.csv'))
sec_plot <-fread(paste0(path,'Surveydata_and_niche/2008_2020_DAS_SYU_p5p10_BBq.csv'))
#######first: deal the 1X1 data
cq <- q_plot[base_d,on=.(code=S_code),nomatch=0]
colnames(cq)
setnames(cq,c('Summit code','Quadrat code','%-cover'),c('Summit','Dir_Q','cover'))
cq[,t_bio1_3:=cover*bio_1_3/10][
  ,t_bio1_2:=cover*bio_1_2/10][
    ,t_bio1_1:=cover*bio_1_1/10][
      ,t_bio1_5:=cover*bio_1_5/10][,
      t_bio1_4:=cover*bio_1_4/10]
    
therm <- cq[,.(ti_3=sum(t_bio1_3)/sum(cover),
              ti_1=sum(t_bio1_1)/sum(cover),
              ti_2=sum(t_bio1_2)/sum(cover),
              ti_4=sum(t_bio1_4)/sum(cover),
              ti_5=sum(t_bio1_5)/sum(cover)),
            by=.(period,Summit)]
ggplot(therm)+
  geom_boxplot(aes(x=Summit, ymin = ti_1,
                   lower =ti_2, middle = ti_3,
                   upper = ti_4,
                   ymax = ti_5,fill=as.factor(period)),stat="identity")
###################deal the section data
sp <- sec_plot[base_d,on=.(code=S_code),nomatch=0]
colnames(sp)
setnames(sp,c('Summit code','方位','Sunnit area section'),c('Summit','Dir','Sub_sec'))
sp[year%in% 2008:2009,period:='A'][
  year%in%2013:2014,period:='B'][
    year%in%2019:2020,period:='C']
sp[cover==30,cover:=25]
sp[,t_bio1_3:=cover*bio_1_3/10][
  ,t_bio1_2:=cover*bio_1_2/10][
    ,t_bio1_1:=cover*bio_1_1/10][
      ,t_bio1_5:=cover*bio_1_5/10][
        ,t_bio1_4:=cover*bio_1_4/10]


therm_sec <- sp[,.(ti_3=sum(t_bio1_3)/sum(cover),
               ti_1=sum(t_bio1_1)/sum(cover),
               ti_2=sum(t_bio1_2)/sum(cover),
               ti_4=sum(t_bio1_4)/sum(cover),
               ti_5=sum(t_bio1_5)/sum(cover)),
            by=.(period,Summit)]
###########WI
sp[,t_wi_3:=cover*wi_3][
  ,t_wi_2:=cover*wi_2][
    ,t_wi_1:=cover*wi_1][
      ,t_wi_5:=cover*wi_5][
        ,t_wi_4:=cover*wi_4]
therm_wi_sec <- sp[,.(wi_3=sum(t_wi_3)/sum(cover),
                   wi_1=sum(t_wi_1)/sum(cover),
                   wi_2=sum(t_wi_2)/sum(cover),
                   wi_4=sum(t_wi_4)/sum(cover),
                   wi_5=sum(t_wi_5)/sum(cover)),
                by=.(period,Summit)]
###########

ggplot(therm_sec)+
  geom_boxplot(aes(x=Summit, ymin = ti_1,
                   lower =ti_2, middle = ti_3,
                   upper = ti_4,
                   ymax = ti_5,fill=period),stat="identity")+
  labs(y='Thermophilic index')
ggsave(paste0(path,'plot/section_thermo_Ind.jpeg'))
###########pre
sp[,t_bio12_3:=cover*bio_12_3/10][
  ,t_bio12_2:=cover*bio_12_2/10][
    ,t_bio12_1:=cover*bio_12_1/10][
      ,t_bio12_5:=cover*bio_12_5/10][
        ,t_bio12_4:=cover*bio_12_4/10]


pre_sec <- sp[,.(ti_3=sum(t_bio12_3)/sum(cover),
                   ti_1=sum(t_bio12_1)/sum(cover),
                   ti_2=sum(t_bio12_2)/sum(cover),
                   ti_4=sum(t_bio12_4)/sum(cover),
                   ti_5=sum(t_bio12_5)/sum(cover)),
                by=.(period,Summit)]
ggplot(pre_sec)+
  geom_boxplot(aes(x=Summit, ymin = ti_1,
                   lower =ti_2, middle = ti_3,
                   upper = ti_4, ymax = ti_5,fill=period),stat="identity")+
  labs(y='Moist-philic index')
ggsave(paste0(path,'plot/section_moist_Ind.jpeg'))
################the distribution of rank x cover
S <- c('SUN','YAT','SEN','JNJ','DSH','TSW')
for (i in 1:6){
  
  p <- ggplot(sp[Summit==S[i]], aes(x=bio_12_3,y=bio_1_3/10))+ 
  geom_density_2d_filled(contour_var = 'ndensity')+
  geom_point()+
  facet_grid(.~period)+
  labs(x='Precipitation (mm)',y='Temperature(℃)')+
  ggtitle(S[i])+
  xlim(c(0,4000))+
  ylim(c(-2,20))
  ggsave(paste0(path,'plot/section_',S[i],'.jpeg'),
         plot=p,
         width=12,
         height =4,dpi = 600 )
}
##############################################
###################plot the species niche
