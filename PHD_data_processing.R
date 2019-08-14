library(data.table)
library(glmmTMB)
d1 <- fread("E:/忍者/發表中/博論發表_climate factor/YAC_UselessFig1.csv")
d_fig <- fread("E:/忍者/發表中/博論發表_climate factor/YAC_Ttol_fig_avg.csv")
d_leaf <- fread("E:/忍者/發表中/博論發表_climate factor/YAC_Ttol_Leaf_avg.csv")
climate <- fread("E:/忍者/發表中/博論發表_climate factor/YAC_climate_data.csv")
S_Basic <- fread("E:/忍者/發表中/博論發表_climate factor/YAC_envir_base.csv")
d2 <- d1[,lapply(.SD,sum), by=TN][,BN:=NULL]
df <- melt(d_fig,id.vars="TN",measure.vars=c(2:178),
           variable.name = "date",value.name="avgf")
dL <- melt(d_leaf,id.var="TN",measure.vars=c(2:178),
           variable.name = "date",value.name="avgL")
d2.fig_ab <- melt(d2,id.vars="TN",measure.vars=c(2:178),
                  variable.name = "date",value.name="avg_nAB")
climate[,Date_2:=as.factor(Date)]
df_2 <- df[climate,on=.(date=Date_2)][S_Basic,on=.(TN=sn)]
attach(df_2)
colnames(df_2)
fit1 <- glmmTMB(avgf~T_max+pre+Wv_max+
                  (1|sea_dis)+(1|Sea_dir)+(1|h)+(1|coverage)+(1|competition_index),
                data = df_2,
                family = poisson)
fit1 <- glmmTMB(avgf~T_max+pre+Ws_max+Wv_max+
                  (1|sea_dis)+(1|Sea_dir)+(1|h)+(1|coverage)+(1|competition_index),
                data = df_2,
                family = poisson)
summary(fit1)
write.csv(df_2,"E:/忍者/發表中/博論發表_climate factor/YAC_GLMM_Time_each_Brunch/YAC_fig_glmm_data.csv")

