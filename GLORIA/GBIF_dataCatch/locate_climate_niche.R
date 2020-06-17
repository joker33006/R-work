library(data.table)
library(ggplot2)
library(agricolae)
path <- "E:/忍者/R-work/Git/R-work/GLORIA/GBIF_dataCatch/"
rdata <- fread("E:/忍者/GLORIA_個人處理/2019/2019資料輸入/資料分析/species_climate.csv")
r2 <- rdata[,names(rdata[,5:23]):=lapply(.SD,as.numeric),
            .SDcols=patterns('bio')]
rloc <- r2[,.(bio_1,bio_6,bio_12,code,name)][,code:=as.factor(code)]
############################### multiple comparison of temp
#aov1 <- aov(bio_1~code,data=rloc)
tuk <- with(rloc,Median.test(bio_6,code))
order_1 <-tuk$group
order_1 <- as.data.table(cbind(order_1,rownames(order_1),
                               c(1:length(order_1[,1]))))
colnames(order_1)[3:4] <-c('code','order')
order_1 <- order_1[order(order)]
write.csv(order_1,paste0(path,'species_group_MinTemp.csv'))
setkey(rloc,code)
setkey(order_1,code)

plot_b1 <- rloc[order_1]
plot_b1[,i.bio_6:=NULL]

plot_b1 <- plot_b1[order(order)]
ggplot(data=plot_b1,
       aes(x=as.factor(reorder(code,order)),
           y=bio_6/10,fill=groups))+
  geom_boxplot(outlier.colour=NA)+
  theme_classic()+
  labs(x='Species code',y='MTCM(°C)',fill = "group")#+
  #theme(legend.position = "none")#+
  #geom_hline(yintercept=c(5,8,11,14,17,23), linetype="dashed", color = "gray51")

ggsave(paste0(path,"plot_result/bio_6_1.jpeg"),width = 12,height = 6,dpi=600)
############################### multiple comparison of rainfall
aov12 <- aov(bio_12~code,data=rloc)
tuk <- HSD.test(aov12,"code")
order_12 <-tuk$group
order_12 <- as.data.table(cbind(order_12,rownames(order_12),c(1:length(order_12[,1]))))
colnames(order_12)[3:4] <-c('code','order')
order_12 <- order_12[order(order)]
write.csv(order_12,paste0(path,'species_group_rain.csv'))
setkey(rloc,code)
setkey(order_12,code)

plot_b12 <- rloc[order_12]
plot_b12[,i.bio_12:=NULL]

plot_b12 <- plot_b12[order(order)]
ggplot(data=plot_b12,aes(x=as.factor(reorder(code,order)),y=bio_12,fill=groups))+
  geom_boxplot(outlier.colour=NA)+
  theme_classic()+
  labs(x='Species code',y='Annual precipitation (mm)',fill = "group")+
  geom_hline(yintercept=c(250,500,1000,2000,4000), linetype="dashed", color = "gray51")#+
  #theme(legend.position = "none")

ggsave(paste0(path,"plot_result/bio_12_1.jpeg"),width = 13,height = 7,dpi=600)

