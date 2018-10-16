rm(list=ls(all=TRUE)) # clean

library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2) # plot the data
library(lme4); library(lmerTest) # used for mixed models
# 
a = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_0410_indiv_runs.csv")
# a = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_0416_indiv_runs_pmod.csv")

a$Condition = ifelse(a$Condition == 1, 'No-body', 'Body')


BNorm <-data.frame()
for (i in 1:20)
{
    temp <- subset(a, Subject == i)
    BNorm <- rbind(BNorm,scale(temp$Beta))
}
a$BNorm <- BNorm$V1

Norm_xFold <-data.frame()
for (j in 4:7)
{
  temp <- subset(a, xFold == j)
  temp2 <- cbind(Subject = temp$Subject,xFold= temp$xFold,Beta = temp$Beta, Norm_x = c(scale(temp$Beta)))
  Norm_xFold <- rbind(Norm_xFold, temp2)
}

a <- merge(a,Norm_xFold)


a$Subject = factor(a$Subject)
a$xFold = factor(a$xFold)

a_outlier = subset(a, a$BNorm > 3 | a$BNorm < -3)
a_excl = subset(a, a$BNorm < 3 & a$BNorm > -3)

a_outlier_xFold = subset(a, a$Norm_x > 3 | a$Norm_x < -3)
a_excl_xFold = subset(a, a$Norm_x < 3 & a$Norm_x > -3)

a_excl2 <- a;
for (i in 1:length(a_outlier[[1]]))
{
  a_excl2 <- subset(a_excl2, !(Subject == a_outlier[i,1] & Round == a_outlier[i,4]))
}

a_excl_xFold2 <- a;
for (i in 1:length(a_outlier_xFold[[1]]))
{
  a_excl_xFold2 <- subset(a_excl_xFold2, !(Subject == a_outlier_xFold[i,1] & Round == a_outlier_xFold[i,4]))
}

 
# a_excl_IQR <- data.frame()
# for (j in 4:7)
# {
#   temp <- subset(a, xFold == j)
#   threshold_IQR = IQR(temp$Beta)*3
#   temp2 <- subset(temp,Beta <= threshold_IQR & Beta >= -threshold_IQR)
#   a_excl_IQR <- rbind(a_excl_IQR,temp2)
# }

ggplot(a, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
ggplot(a, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) + facet_wrap(~Subject)
ggplot(a, aes(y=BNorm, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
ggplot(a, aes(y=Norm_x, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)

ggplot(a_excl2, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
ggplot(a_excl, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) + facet_wrap(~Subject)
ggplot(a_excl, aes(y=BNorm, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)

ggplot(a_excl_xFold2, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
ggplot(a_excl_xFold, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) + facet_wrap(~Subject)
ggplot(a_excl_xFold, aes(y=BNorm, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)

ggplot(a_excl_IQR, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
ggplot(a_excl_IQR, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) + facet_wrap(~Subject)
ggplot(a_excl_IQR, aes(y=BNorm, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
# 
# a_6fold = subset(a, a$xFold == 6)
# ggplot(a_6fold, aes(y=Beta, x=Condition, color=Condition)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=3)
# ggplot(a_6fold, aes(y=BNorm, x=Condition, color=Condition)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)
# 
a_6fold = subset(a_excl, a_excl$xFold == 6)
ggplot(a_6fold, aes(y=Beta, x=Condition, color=Condition)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=3)
ggplot(a_6fold, aes(y=Beta, x=Condition, color=Condition)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=3) + facet_wrap(~Subject)

ggplot(a_6fold, aes(y=BNorm, x=Condition, color=Condition)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=3)


a_6fold = subset(a, a$xFold == 7 & a$Subject != 12 & a$Subject != 20)
a_6fold = subset(a_excl, a_excl$xFold == 6)
wilcox.test(a_6fold$Beta, mu = 0, alternative = 'greater')

# m1 = lmer(Beta ~ 1  + (1|Subject),a_6fold)
#m2 = lmer(TimeSpent ~ Condition  + (Condition|Subject),a)
# 
#anova(m1,m2)

# summary(m1)
# anova(m1)
#summary(m2)
#anova(m2)


indivdata = aggregate(cbind(Beta,BNorm) ~ Subject + xFold, a, mean)
indivdata_wCond = aggregate(cbind(Beta,BNorm) ~ Subject + xFold + Condition, a, mean)

indivdata_excl = aggregate(cbind(Beta,BNorm) ~ Subject + xFold, a_excl, mean)
indivdata_excl_xFold = aggregate(cbind(Beta,BNorm) ~ Subject + xFold, a_excl_xFold, mean)

indivdata_excl2 = aggregate(cbind(Beta,BNorm) ~ Subject + xFold, a_excl2, mean)
indivdata_excl_xFold2 = aggregate(cbind(Beta,BNorm) ~ Subject + xFold, a_excl_xFold2, mean)

indivdata_excl_IQR = aggregate(cbind(Beta,BNorm) ~ Subject + xFold, a_excl_IQR, mean)

ggplot(indivdata, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=3) + facet_wrap(~Subject)

ggplot(indivdata, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3)# + facet_wrap(~Subject)
# ggplot(indivdata, aes(y=BNorm, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3)# + facet_wrap(~Subject)

ggplot(indivdata_excl2, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3)# + facet_wrap(~Subject)
# ggplot(indivdata_excl, aes(y=BNorm, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3)# + facet_wrap(~Subject)
ggplot(indivdata_excl_xFold2, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3)# + facet_wrap(~Subject)
ggplot(indivdata_excl_IQR, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3)# + facet_wrap(~Subject)


indiv_6fold = subset(indivdata_excl_xFold2, indivdata_excl_xFold2$xFold == 6 )# & indivdata$Subject != 12)
wilcox.test(indiv_6fold$Beta, mu = 0, alternative = 'greater')
wilcox.test(indiv_6fold$BNorm, mu = 0, alternative = 'greater')


ggplot(indivdata_excl, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3)# + facet_wrap(~Subject)
ggplot(indivdata_excl, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=3) + facet_wrap(~Subject)

indiv_6fold = subset(indivdata_excl, indivdata_excl$xFold == 6 )# & indivdata$Subject != 12)
indiv_6fold = subset(indivdata_excl, indivdata_excl$xFold == 6 & indivdata_excl$Subject != 12 & indivdata_excl$Subject != 20 )
wilcox.test(indiv_6fold$Beta, mu = 0, alternative = 'greater')
wilcox.test(indiv_6fold$BNorm, mu = 0, alternative = 'greater')


indiv_mean_sd <- indivdata %>% group_by(xFold) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
p<- ggplot(indiv_mean_sd, aes(y=Beta_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=Beta_mean-Beta_se,ymax=Beta_mean+Beta_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)


p + theme(
  #plot.title = element_text(color="red", size=14, face="bold.italic"),
  axis.title.x = element_text(size=18, face="bold"),
  axis.title.y = element_text(size=22, face="bold"),
  axis.text = element_text(size=22),
  legend.position = "none",
  legend.text = element_text(size=22),
  legend.title = element_text(size=18)
)

indivdata_excl_cond = aggregate(cbind(Beta,BNorm) ~ Subject + xFold + Condition, a_excl, mean)
indiv_6fold_excl_cond = subset(indivdata_excl_cond, indivdata_excl_cond$xFold == 6 )# & indivdata$Subject != 12)

indiv_mean_sd <- indiv_6fold_excl_cond %>% group_by(Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
ggplot(indiv_mean_sd, aes(y=Beta_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=Beta_mean-Beta_se,ymax=Beta_mean+Beta_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#F8766D", "#AAAAAA"))#+ ylim(0,20)


# a = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_0410.csv")
# a$xFold = factor(a$xFold)
# a$Subject = factor(a$Subject)
# ggplot(a, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3)# + facet_wrap(~Subject)
# ggplot(a, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=3) + facet_wrap(~Subject)
# 
# a_6fold = subset(a, a$xFold == 6  & a$Subject != 12)
# wilcox.test(a_6fold$Beta, mu = 0, alternative = 'greater')
