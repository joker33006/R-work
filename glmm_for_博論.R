library(data.table)
library(ggplot2)
library(statmod)
library(mgcv)
path <- "E:/忍者/發表中/博論發表_climate factor"
dt <- fread(paste0(path,"/GLMM_YAC_TS分析前.csv"))
glmm_db <- fread(paste0(path,"/YAC_GLMM_Time_each_Brunch/YAC_fig_glmm_data.csv"))
attach(dt)

ht <- dt[hi_t>0,]
ggplot(ht,aes(X="hi_t",Y="NAB2"))
plot(ht[,max_ws],ht[,NAB2])
fit <- gam(NAB2~te(avg_t)+te(pre)+te(max_ws)+te(wv_max)+te(hi_t)+te(lo_t), 
           data=dt)
glmm_ft <- gamm(avgf~s(T_max)+s(pre)+s(Ws_max)+s(Wv_max),
                random=list(TN=~1,Sea_dir=~1),
                correlation = corAR1(),
                data = glmm_db)
summary(glmm_ft$gam)
