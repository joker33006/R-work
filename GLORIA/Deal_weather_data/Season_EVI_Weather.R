library(data.table)
library(ggplot2)
dt <- fread('E:/忍者/GLORIA_個人處理/2020/SYU_EVIandcliment/EVI_Weather.csv')
s <- c('Winter',"Spring",'Summer','Fall')
m1 <- lm()
for (i in 1:4){
  i=4
ggplot(data=dt[season==s[i]],aes(x=max_t,y=E.max,color=summit))+
  geom_point()+
  facet_wrap(season~.)
}