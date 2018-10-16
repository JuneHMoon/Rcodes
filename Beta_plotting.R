rm(list=ls(all=TRUE)) # clean
# 
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("readr")
# install.packages("ggplot2")
# install.packages("lme4")
# install.packages("lmerTest")
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2) # plot the data
library(lme4); library(lmerTest) # used for mixed models
library(sjPlot)
# 
# a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_0626_pmod_resol66.csv")
# a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_0626_pmod_motionCorrection_resol66_v2.csv")

# a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_0604_pmod_motionCorrection.csv")
a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_0604_pmod.csv")
# a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_0604_withSameCond.csv")
# a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_0604_pmod_thr0.5.csv")

# a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_0508_pmod.csv")
# a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_0508_func.csv")
# a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_0508_withSameCond.csv")
# a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_0508.csv")
# a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_0508_1stHalf.csv")

a <- a_raw
#exclude Subj 12/ round 4
# a <- subset(a, a$Subject != 16 & a$Subject != 20 & a$Subject != 4 & a$Subject != 22 & a$Subject != 27)
a <- subset(a, a$Subject !=22) # & a$Subject != 13) # 13
a <- subset(a, a$Subject !=13)
a <- subset(a, a$Subject !=26)
#a <- subset(a, a$Subject !=4)


a <- subset(a,!(a$Subject == 26 & a$Round ==5))
#a <- subset(a,!(a$Subject == 4 & a$Round ==2)) #& !(a$Subject == 3 & a$Round ==5)


a <- subset(a, a$Subject !=2 & a$Subject !=4 & a$Subject !=5 & a$Subject !=6 & a$Subject !=12 & a$Subject !=13 & a$Subject !=26) 

a <- subset(a,!(a$Subject == 12 & a$Round ==4)) #  & a$Subject != 13 )

a <- subset(a,!(a$Subject == 6 & a$Round ==2)) #& !(a$Subject == 3 & a$Round ==5)
a <- subset(a,!(a$Subject == 26 & a$Round ==5))
a <- subset(a,!(a$Subject == 27 & a$Round ==2))
# a <- subset(a,!(a$Subject == 11 & a$Round ==5))
# a <- subset(a,!(a$Subject == 26 & a$Round ==5)) #  & a$Subject != 13 )

a$Condition = ifelse(a$Condition == 1, 'No-body', 'Body')

aNorm <-data.frame() #Subject-wise normalization
aRNorm <-data.frame() #Round-wise normalization

for (n in 1:3)
{
  a_temp <- subset(a,a$GLM1_ROI == n)
  for (i in 1:27)
  {
    tempSubj <- subset(a_temp, Subject == i)

    for( j in 1:6)
    {
      tempRound <- subset(tempSubj,Round == j)
      temp2 <- data.frame(Subject = tempRound$Subject,GLM1_ROI = tempRound$GLM1_ROI,xFold = tempRound$xFold, Round = tempRound$Round,BRNorm_R = c(scale(tempRound$Beta_R)),BRNorm_L = c(scale(tempRound$Beta_L)), BRNorm_B = c(scale(tempRound$Beta_B)),Condition = tempRound$Condition)
      # colnames(temp2) <- c("Subject","GLM1_ROI","xFold","Round","BRNorm_R","BRNorm_L","BRNorm_B","Condition")
      aRNorm <- rbind(aRNorm,temp2)
    }
    temp <- data.frame(Subject = tempSubj$Subject,GLM1_ROI = tempSubj$GLM1_ROI,xFold = tempSubj$xFold, Round = tempSubj$Round,BNorm_R = c(scale(tempSubj$Beta_R)),BNorm_L = c(scale(tempSubj$Beta_L)), BNorm_B = c(scale(tempSubj$Beta_B)),Condition = tempSubj$Condition)
    
    # colnames(temp) <- c("Subject","GLM1_ROI","xFold","Round","BNorm_R","BNorm_L","BNorm_B","Condition")
    aNorm <- rbind(aNorm,temp)
  }
}

a <- merge(a,aNorm)
a <- merge(a,aRNorm)

a_outlier <- subset(a, (a$BNorm_B > 3 | a$BNorm_B < -3) | (a$BNorm_R > 3 | a$BNorm_R < -3) | (a$BNorm_L > 3 | a$BNorm_L < -3) )

# xFold-wise Normalization
# Norm_xFold <-data.frame()
# for (j in 4:7)
# {
#   temp <- subset(a, xFold == j)
#   temp2 <- cbind(Subject = temp$Subject,xFold= temp$xFold,Beta = temp$Beta, Norm_x = c(scale(temp$Beta)))
#   Norm_xFold <- rbind(Norm_xFold, temp2)
# }
# a <- merge(a,Norm_xFold)

a$Subject = factor(a$Subject)
a$xFold = factor(a$xFold)

behav_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Memory_data_fMRI_0502.csv")
behav_raw$TimeSpent = behav_raw$EndTime - behav_raw$StartTime # Timespent during retrieval

behav <- subset(behav_raw,  Index != 1 & Index != 14) # first trial - start from the center/ last trial - with virtual threat

NormalizedData <-data.frame() ## Normalization within subject
for (i in 1:27)
{
  temp <- subset(behav, Subject == i)
  temp2 <- cbind(scale(temp$dError), scale(temp$dMoved), scale(temp$estimation_dError))
  NormalizedData <- rbind(NormalizedData,temp2)
}

behav$distNorm <- NormalizedData$V1
behav$dMovedNorm <- NormalizedData$V2
behav$estimationNorm <- NormalizedData$V3

behav_data <- behav

RoundQ = aggregate(cbind(dError,dMoved,TimeSpent,distNorm, dMovedNorm,estimationNorm, Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z) ~  Subject + Condition +Round, behav_data, mean)
RoundQ$Condition = ifelse(RoundQ$Condition == 1, 'No-body', 'Body')


# a_outlier_xFold = subset(a, a$Norm_x > 3 | a$Norm_x < -3)
# a_excl_xFold = subset(a, a$Norm_x < 3 & a$Norm_x > -3)

aR <- subset(a,a$GLM1_ROI == 1) # GLM1 with the right Entorhinal Cortex
aL <- subset(a,a$GLM1_ROI == 2) # GLM1 with the left Entorhinal Cortex
aB <- subset(a,a$GLM1_ROI == 3) # GLM1 with the bilateral Entorhinal Cortex

### GLM1 : Right EC / Left / Bilateral
b<-aB
# b<-subset(b,b$Condition == 'Body')
# b<-subset(b,b$Condition == 'No-body')
#Raw Beta
ggplot(b, aes(y=Beta_R, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
ggplot(b, aes(y=Beta_L, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
ggplot(b, aes(y=Beta_B, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
#Normalized - Subject-wise
ggplot(b, aes(y=BNorm_R, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) # + facet_wrap(~Subject)
ggplot(b, aes(y=BNorm_L, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
ggplot(b, aes(y=BNorm_B, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
#Normalized - Round-wise
ggplot(b, aes(y=BRNorm_R, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
ggplot(b, aes(y=BRNorm_L, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
ggplot(b, aes(y=BRNorm_B, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)

ggplot(b, aes(y=Beta_R, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) + facet_wrap(~Subject,scales='free')
ggplot(b, aes(y=BNorm_R, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) + facet_wrap(~Subject)
ggplot(b, aes(y=BRNorm_R, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) + facet_wrap(~Subject)
ggplot(b, aes(y=Beta_L, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) + facet_wrap(~Subject)
ggplot(b, aes(y=BNorm_L, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) + facet_wrap(~Subject)
ggplot(b, aes(y=BRNorm_L, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) + facet_wrap(~Subject)
ggplot(b, aes(y=Beta_B, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) + facet_wrap(~Subject)
ggplot(b, aes(y=BNorm_B, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) + facet_wrap(~Subject)
ggplot(b, aes(y=BRNorm_B, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) + facet_wrap(~Subject)

# ggplot(a_excl2, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
# ggplot(a_excl, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) + facet_wrap(~Subject)
# ggplot(a_excl, aes(y=BNorm, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
# 
# ggplot(a_excl_xFold2, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
# ggplot(a_excl_xFold, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) + facet_wrap(~Subject)
# ggplot(a_excl_xFold, aes(y=BNorm, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
# 
# ggplot(a_excl_IQR, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)
# ggplot(a_excl_IQR, aes(y=Beta, x=xFold, color=xFold)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=1.5) + facet_wrap(~Subject)
# ggplot(a_excl_IQR, aes(y=BNorm, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1.5)# + facet_wrap(~Subject)

# a_6fold = subset(a, a$xFold == 6)
# ggplot(a_6fold, aes(y=Beta, x=Condition, color=Condition)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=3)
# ggplot(a_6fold, aes(y=BNorm, x=Condition, color=Condition)) + geom_boxplot() + geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)
# 
b<-aB
b_6fold = subset(b, b$xFold == 6)
b_6fold_body = subset(b_6fold, b_6fold$Condition == "Body")
b_6fold_nbody = subset(b_6fold, b_6fold$Condition == "No-body")

# ggplot(b_6fold, aes(y=Beta_R, x=Condition, group=Round, color=Condition)) + geom_line(aes(group=interaction(Subject,Round)),size = 0.8)+ geom_point(size = 2.5) #+ scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))
ggplot(b_6fold, aes(y=Beta_R, x=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) 
ggplot(b_6fold, aes(y=Beta_L, x=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) 
ggplot(b_6fold, aes(y=Beta_B, x=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) 

ggplot(b_6fold, aes(y=BNorm_R, x=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) 
ggplot(b_6fold, aes(y=BNorm_L, x=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) 
ggplot(b_6fold, aes(y=BNorm_B, x=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) 

ggplot(b_6fold, aes(y=BRNorm_R, x=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) 
ggplot(b_6fold, aes(y=BRNorm_L, x=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) 
ggplot(b_6fold, aes(y=BRNorm_B, x=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) 

# b_6fold <- b_6fold_body
# b_6fold <- b_6fold_nbody

wilcox.test(b_6fold$Beta_R, mu = 0, alternative = 'greater')
wilcox.test(b_6fold$Beta_L, mu = 0, alternative = 'greater')
wilcox.test(b_6fold$Beta_B, mu = 0, alternative = 'greater')
wilcox.test(b_6fold$BNorm_R, mu = 0, alternative = 'greater')
wilcox.test(b_6fold$BNorm_L, mu = 0, alternative = 'greater')
wilcox.test(b_6fold$BNorm_B, mu = 0, alternative = 'greater')
wilcox.test(b_6fold$BRNorm_R, mu = 0, alternative = 'greater')
wilcox.test(b_6fold$BRNorm_L, mu = 0, alternative = 'greater')
wilcox.test(b_6fold$BRNorm_B, mu = 0, alternative = 'greater')

wilcox.test(b$Beta_B[b$xFold==6],b$Beta_B[b$xFold==5],paired=T)

t.test(b_6fold$Beta_R, mu = 0, alternative = 'greater')
t.test(b_6fold$Beta_L, mu = 0, alternative = 'greater')
t.test(b_6fold$Beta_B, mu = 0, alternative = 'greater')
t.test(b_6fold$BNorm_R, mu = 0, alternative = 'greater')
t.test(b_6fold$BNorm_L, mu = 0, alternative = 'greater')
t.test(b_6fold$BNorm_B, mu = 0, alternative = 'greater')
t.test(b_6fold$BRNorm_R, mu = 0, alternative = 'greater')
t.test(b_6fold$BRNorm_L, mu = 0, alternative = 'greater')
t.test(b_6fold$BRNorm_B, mu = 0, alternative = 'greater')

t.test(b$Beta_R[b$xFold==6],b$Beta_R[b$xFold==4],paired=T)

temp_correl <- merge(b_6fold,RoundQ)
temp_correl_nobody <- subset(temp_correl, Condition == 'No-body')
temp_correl_body <- subset(temp_correl, Condition == 'Body')

ggplot(temp_correl, aes(y=Beta_R, x=Q1, color=Condition)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")
cor.test(~Beta_R + Q1, data = temp_correl_nobody, method ="spearman", continuity = TRUE, conf.level = 0.95)

m1 = lmer(Beta_B ~ Condition +(1|Subject),b_6fold)
m2 = lmer(Beta_B ~ Condition +(Condition|Subject),b_6fold)

m2 = lmer(Beta_R ~ Q1*Condition + (Q1*Condition|Subject),temp_correl)

m1 = lmer(Beta_R ~ Q1*Condition +(1|Subject),temp_correl)
m2 = lmer(Beta_R ~ Q1*Condition + (Q1*Condition|Subject),temp_correl)


m1 = lmer(BetaNorm ~ Q1z + Condition + (1|Subject),temp_correl)
m2 = lmer(BetaNorm ~ Q1z + Condition + (Condition|Subject),temp_correl)
anova(m1,m2)
summary(m1)
anova(m1)
summary(m2)
anova(m2)

indivdata = aggregate(cbind(Beta_R,Beta_L,Beta_B,BNorm_R,BNorm_L,BNorm_B,BRNorm_R,BRNorm_L,BRNorm_B) ~ Subject + GLM1_ROI + xFold, a, mean)
indivdata_wCond = aggregate(cbind(Beta_R,Beta_L,Beta_B,BNorm_R,BNorm_L,BNorm_B,BRNorm_R,BRNorm_L,BRNorm_B) ~ Subject + GLM1_ROI + xFold + Condition, a, mean)

# indivdata = aggregate(cbind(Beta_B,BNorm_B,BRNorm_B) ~ Subject + GLM1_ROI + xFold, a, mean)
# indivdata_wCond = aggregate(cbind(Beta_B,BNorm_B,BRNorm_B) ~ Subject + GLM1_ROI + xFold + Condition, a, mean)

indivwC_R <- subset(indivdata_wCond, indivdata_wCond$GLM1_ROI == 1)
indivwC_L <- subset(indivdata_wCond, indivdata_wCond$GLM1_ROI == 2)
indivwC_B <- subset(indivdata_wCond, indivdata_wCond$GLM1_ROI == 3)
# indivdata_excl = aggregate(cbind(Beta,BNorm) ~ Subject + xFold, a_excl, mean)
# indivdata_excl_xFold = aggregate(cbind(Beta,BNorm) ~ Subject + xFold, a_excl_xFold, mean)
# 
# indivdata_excl2 = aggregate(cbind(Beta,BNorm) ~ Subject + xFold, a_excl2, mean)
# indivdata_excl_xFold2 = aggregate(cbind(Beta,BNorm) ~ Subject + xFold, a_excl_xFold2, mean)
# 
# indivdata_excl_IQR = aggregate(cbind(Beta,BNorm) ~ Subject + xFold, a_excl_IQR, mean)

indiv_R <- subset(indivdata, indivdata$GLM1_ROI == 1)
indiv_L <- subset(indivdata, indivdata$GLM1_ROI == 2)
indiv_B <- subset(indivdata, indivdata$GLM1_ROI == 3)
indiv <- indiv_B
ggplot(indiv, aes(y=Beta_R, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) + facet_wrap(~Subject)

ggplot(indiv, aes(y=Beta_R, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)
ggplot(indiv, aes(y=Beta_L, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)
ggplot(indiv, aes(y=Beta_B, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)
ggplot(indiv, aes(y=BNorm_R, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)
ggplot(indiv, aes(y=BNorm_L, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)
ggplot(indiv, aes(y=BNorm_B, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)
ggplot(indiv, aes(y=BRNorm_R, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)
ggplot(indiv, aes(y=BRNorm_L, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)
ggplot(indiv, aes(y=BRNorm_B, x=xFold, color=xFold)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)

indiv_6fold = subset(indiv, indiv$xFold == 6 )# & indivdata$Subject != 12)
indiv_6fold = subset(indivwC_B, xFold == 6 & Condition == 'No-body')# & indivdata$Subject != 12)

wilcox.test(indiv_6fold$Beta_R, mu = 0, alternative = 'greater')
wilcox.test(indiv_6fold$Beta_L, mu = 0, alternative = 'greater')
wilcox.test(indiv_6fold$Beta_B, mu = 0, alternative = 'greater')
wilcox.test(indiv_6fold$BNorm_R, mu = 0, alternative = 'greater')
wilcox.test(indiv_6fold$BNorm_L, mu = 0, alternative = 'greater')
wilcox.test(indiv_6fold$BNorm_B, mu = 0, alternative = 'greater')
wilcox.test(indiv_6fold$BRNorm_R, mu = 0, alternative = 'greater')
wilcox.test(indiv_6fold$BRNorm_L, mu = 0, alternative = 'greater')
wilcox.test(indiv_6fold$BRNorm_B, mu = 0, alternative = 'greater')
wilcox.test(indiv$BNorm_R[indiv$xFold==6],indiv$BNorm_R[indiv$xFold==7],paired=T)

indiv <- indiv_B
indiv_mean_sd <- indiv %>% group_by(xFold) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

p<- ggplot(indiv_mean_sd, aes(y=Beta_R_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=Beta_R_mean-Beta_R_se,ymax=Beta_R_mean+Beta_R_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA")) + ylab('Grid-like representation')#+ ylim(0,20)
p<- ggplot(indiv_mean_sd, aes(y=BNorm_R_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=BNorm_R_mean-BNorm_R_se,ymax=BNorm_R_mean+BNorm_R_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA")) + ylab('Grid-like representation')#+ ylim(0,20)
p<- ggplot(indiv_mean_sd, aes(y=BRNorm_R_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=BRNorm_R_mean-BRNorm_R_se,ymax=BRNorm_R_mean+BRNorm_R_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA")) + ylab('Grid-like representation')#+ ylim(0,20)

p<- ggplot(indiv_mean_sd, aes(y=Beta_L_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=Beta_L_mean-Beta_L_se,ymax=Beta_L_mean+Beta_L_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)
p<- ggplot(indiv_mean_sd, aes(y=BNorm_L_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=BNorm_L_mean-BNorm_L_se,ymax=BNorm_L_mean+BNorm_L_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)
p<- ggplot(indiv_mean_sd, aes(y=BRNorm_L_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=BRNorm_L_mean-BRNorm_L_se,ymax=BRNorm_L_mean+BRNorm_L_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)

p<- ggplot(indiv_mean_sd, aes(y=Beta_B_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=Beta_B_mean-Beta_B_se,ymax=Beta_B_mean+Beta_B_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))+ ylab('Grid-like representation')#+ ylim(0,20)
p<- ggplot(indiv_mean_sd, aes(y=BNorm_B_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=BNorm_B_mean-BNorm_B_se,ymax=BNorm_B_mean+BNorm_B_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)
p<- ggplot(indiv_mean_sd, aes(y=BRNorm_B_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=BRNorm_B_mean-BRNorm_B_se,ymax=BRNorm_B_mean+BRNorm_B_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)

p + theme(
  #plot.title = element_text(color="red", size=14, face="bold.italic"),
  axis.title.x = element_text(size=18, face="bold"),
  axis.title.y = element_text(size=22, face="bold"),
  axis.text = element_text(size=22),
  legend.position = "none",
  legend.text = element_text(size=22),
  legend.title = element_text(size=18)
)

indivwC <- indivwC_B
indivwC_6fold <- subset(indivwC,indivwC$xFold ==6 )
# indivwC_6fold <- indivwC_6fold %>% group_by(Subject) %>% mutate(Body_Effect = (BNorm_B[Condition=='Body'] - BNorm_B[Condition=='No-body']))
indivwC_6fold <- indivwC_6fold %>% group_by(Subject) %>% mutate(Body_Effect = (Beta_B[Condition=='Body'] - Beta_B[Condition=='No-body']))
# indivwC_6fold <- indivwC_6fold %>% group_by(Subject) %>% mutate(Body_Effect = (Beta_L[Condition=='Body'] - Beta_L[Condition=='No-body']))
# indivwC_6fold <- indivwC_6fold %>% group_by(Subject) %>% mutate(Body_Effect = (Beta_B[Condition=='Body'] - Beta_B[Condition=='No-body']))

indivwC_6fold <- indivwC_6fold[with(indivwC_6fold, order(Subject)),]
# test <- subset(indivwC_6fold, Beta_R > 0)
# ggplot(test, aes(y=Beta_R, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))
# 
# test <- subset(indivwC_6fold, Beta_L > -0.02)
# ggplot(test, aes(y=Beta_L, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))


ggplot(indivwC_6fold, aes(y=Beta_B, x=Condition, group=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=3)

ggplot(indivwC_6fold, aes(y=Beta_R, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))
ggplot(indivwC_6fold, aes(y=Beta_L, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))
ggplot(indivwC_6fold, aes(y=Beta_B, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))

ggplot(indivwC_6fold, aes(y=BNorm_R, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))
ggplot(indivwC_6fold, aes(y=BNorm_L, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))
ggplot(indivwC_6fold, aes(y=BNorm_B, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))

indiv_body <- subset(indivwC,indivwC$Condition == "Body")
indiv_nbody <- subset(indivwC,indivwC$Condition == "No-body")

# indivwC_6fold <- subset(indivwC_6fold, indivwC_6fold$Subject != 16 & indivwC_6fold$Subject != 20 & indivwC_6fold$Subject != 27)


indiv_6fold_cond = subset(indiv_body, indiv$xFold == 6 )# & indivdata$Subject != 12)
indiv_6fold_cond = subset(indiv_nbody, indiv$xFold == 6 )# & indivdata$Subject != 12)
# 
wilcox.test(indiv_6fold_cond$Beta_R, mu = 0, alternative = 'greater')
wilcox.test(indiv_6fold_cond$Beta_L, mu = 0, alternative = 'greater')
wilcox.test(indiv_6fold_cond$Beta_B, mu = 0, alternative = 'greater')
# wilcox.test(indiv_6fold_cond$BNorm_R, mu = 0, alternative = 'greater')
# wilcox.test(indiv_6fold_cond$BNorm_L, mu = 0, alternative = 'greater')
# wilcox.test(indiv_6fold_cond$BNorm_B, mu = 0, alternative = 'greater')
# wilcox.test(indiv_6fold_cond$BRNorm_R, mu = 0, alternative = 'greater')
# wilcox.test(indiv_6fold_cond$BRNorm_L, mu = 0, alternative = 'greater')
# wilcox.test(indiv_6fold_cond$BRNorm_B, mu = 0, alternative = 'greater')
indivwC_mean_sd <- indivwC %>% group_by(xFold,Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
indiv_body_mean_sd <- indiv_body %>% group_by(xFold) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
indiv_nbody_mean_sd <- indiv_nbody %>% group_by(xFold) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
 
# p<- ggplot(indiv_body_mean_sd, aes(y=BNorm_R_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=BNorm_R_mean-BNorm_R_se,ymax=BNorm_R_mean+BNorm_R_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)
# p<- ggplot(indiv_nbody_mean_sd, aes(y=BNorm_R_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=BNorm_R_mean-BNorm_R_se,ymax=BNorm_R_mean+BNorm_R_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)
# 
# p<- ggplot(indiv_body_mean_sd, aes(y=BNorm_L_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=BNorm_L_mean-BNorm_L_se,ymax=BNorm_L_mean+BNorm_L_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)
# p<- ggplot(indiv_nbody_mean_sd, aes(y=BNorm_L_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=BNorm_L_mean-BNorm_L_se,ymax=BNorm_L_mean+BNorm_L_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)
# 
# p<- ggplot(indiv_body_mean_sd, aes(y=BNorm_B_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=BNorm_B_mean-BNorm_B_se,ymax=BNorm_B_mean+BNorm_B_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)
# p<- ggplot(indiv_nbody_mean_sd, aes(y=BNorm_B_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=BNorm_B_mean-BNorm_B_se,ymax=BNorm_B_mean+BNorm_B_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)
# 
Condition.color <- c("Body-control", "Nobody-control", "Body-control", "Nobody-control","Body", "Nobody", "Body-control", "Nobody-control")
p<- ggplot(indivwC_mean_sd, aes(y=Beta_B_mean, x=xFold, fill = Condition.color)) + geom_bar(stat="identity",width=.6,position=position_dodge(.7)) + geom_errorbar(aes(ymin=Beta_R_mean-Beta_R_se,ymax=Beta_R_mean+Beta_R_se), size = .3, width=.2,position=position_dodge(.7)) + scale_fill_manual(values=c("#F8766D", "#AAAAAA", "#00BFC4", "#AAAAAA")) + ylab("Grid-like Representation")#+ ylim(0,20)

ggplot(indivwC_mean_sd, aes(y=Beta_R_mean, x=xFold, fill = Condition)) + geom_bar(stat="identity",width=.6,position=position_dodge(.7)) + geom_errorbar(aes(ymin=Beta_R_mean-Beta_R_se,ymax=Beta_R_mean+Beta_R_se), size = .3, width=.2,position=position_dodge(.7)) #+ scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA", "#F8766D", "#AAAAAA", "#F8766D", "#AAAAAA")) #+ ylim(0,20)

p<- ggplot(indiv_body_mean_sd, aes(y=Beta_R_mean, x=xFold, fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=Beta_R_mean-Beta_R_se,ymax=Beta_R_mean+Beta_R_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)
p<- ggplot(indiv_nbody_mean_sd, aes(y=Beta_R_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=Beta_R_mean-Beta_R_se,ymax=Beta_R_mean+Beta_R_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)
# 
# p<- ggplot(indiv_body_mean_sd, aes(y=Beta_L_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=Beta_L_mean-Beta_L_se,ymax=Beta_L_mean+Beta_L_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)
# p<- ggplot(indiv_nbody_mean_sd, aes(y=Beta_L_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=Beta_L_mean-Beta_L_se,ymax=Beta_L_mean+Beta_L_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)
# 
p<- ggplot(indiv_body_mean_sd, aes(y=Beta_B_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=Beta_B_mean-Beta_B_se,ymax=Beta_B_mean+Beta_B_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)
p<- ggplot(indiv_nbody_mean_sd, aes(y=Beta_B_mean, x=xFold,fill=xFold)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=Beta_B_mean-Beta_B_se,ymax=Beta_B_mean+Beta_B_se), size = .3, width=.2,position=position_dodge(.9)) + scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)


p + theme(
  #plot.title = element_text(color="red", size=14, face="bold.italic"),
  axis.title.x = element_text(size=18, face="bold"),
  axis.title.y = element_text(size=22, face="bold"),
  axis.text = element_text(size=22),
  legend.position = "none",
  legend.text = element_text(size=22),
  legend.title = element_text(size=18)
)


indivwC <- indivwC_B
indivwC_6fold <- subset(indivwC,indivwC$xFold ==6 )
# indivwC_6fold <- indivwC_6fold %>% group_by(Subject) %>% mutate(Body_Effect = (BNorm_B[Condition=='Body'] - BNorm_B[Condition=='No-body']))
indivwC_6fold <- indivwC_6fold %>% group_by(Subject) %>% mutate(Body_Effect = (Beta_R[Condition=='Body'] - Beta_R[Condition=='No-body']))
# indivwC_6fold <- indivwC_6fold %>% group_by(Subject) %>% mutate(Body_Effect = (Beta_L[Condition=='Body'] - Beta_L[Condition=='No-body']))
# indivwC_6fold <- indivwC_6fold %>% group_by(Subject) %>% mutate(Body_Effect = (Beta_B[Condition=='Body'] - Beta_B[Condition=='No-body']))

indivwC_6fold <- indivwC_6fold[with(indivwC_6fold, order(Subject)),]
indiv_mean_sd <- indivwC_6fold %>% group_by(Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

p<- ggplot(indiv_mean_sd, aes(y=Beta_B_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=Beta_B_mean-Beta_B_se,ymax=Beta_B_mean+Beta_B_se), size = .3, width=.2,position=position_dodge(.9)) +  #+ scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)
p<- ggplot(indiv_mean_sd, aes(y=Beta_R_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=Beta_R_mean-Beta_R_se,ymax=Beta_R_mean+Beta_R_se), size = .3, width=.2,position=position_dodge(.9)) #+ scale_fill_manual(values=c("#AAAAAA", "#AAAAAA", "#F8766D", "#AAAAAA"))#+ ylim(0,20)

p + theme(
  #plot.title = element_text(color="red", size=14, face="bold.italic"),
  axis.title.x = element_text(size=18, face="bold"),
  axis.title.y = element_text(size=22, face="bold"),
  axis.text = element_text(size=22),
  legend.position = "none",
  legend.text = element_text(size=22),
  legend.title = element_text(size=18)
)
# ggplot(indiv, aes(y=BNorm_R, x=Condition, group=Round, color=Condition)) + geom_line(aes(group=interaction(Subject,Round)),size = 0.8)+ geom_point(size = 2.5) #+ scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))

wilcox.test(indivwC_6fold$Beta_R[indivwC_6fold$Condition == 'Body'], indivwC_6fold$Beta_R[indivwC_6fold$Condition == 'No-body'], paired=T)
wilcox.test(indivwC_6fold$Beta_L[indivwC_6fold$Condition == 'Body'], indivwC_6fold$Beta_L[indivwC_6fold$Condition == 'No-body'], paired=T)
wilcox.test(indivwC_6fold$Beta_B[indivwC_6fold$Condition == 'Body'], indivwC_6fold$Beta_B[indivwC_6fold$Condition == 'No-body'], paired=T)
wilcox.test(indivwC_6fold$BNorm_R[indivwC_6fold$Condition == 'Body'], indivwC_6fold$BNorm_R[indivwC_6fold$Condition == 'No-body'], paired=T)
wilcox.test(indivwC_6fold$BNorm_L[indivwC_6fold$Condition == 'Body'], indivwC_6fold$BNorm_L[indivwC_6fold$Condition == 'No-body'], paired=T)
wilcox.test(indivwC_6fold$BNorm_B[indivwC_6fold$Condition == 'Body'], indivwC_6fold$BNorm_B[indivwC_6fold$Condition == 'No-body'], paired=T)
wilcox.test(indivwC_6fold$BRNorm_R[indivwC_6fold$Condition == 'Body'], indivwC_6fold$BRNorm_R[indivwC_6fold$Condition == 'No-body'], paired=T)
wilcox.test(indivwC_6fold$BRNorm_L[indivwC_6fold$Condition == 'Body'], indivwC_6fold$BRNorm_L[indivwC_6fold$Condition == 'No-body'], paired=T)
wilcox.test(indivwC_6fold$BRNorm_B[indivwC_6fold$Condition == 'Body'], indivwC_6fold$BRNorm_B[indivwC_6fold$Condition == 'No-body'], paired=T)

Q5_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Memory_data_fMRI_Q5_0502.csv")
Q5_raw$Condition = ifelse(Q5_raw$Condition == 1, 'No-body', 'Body')

indivQ = aggregate(cbind(dError,dMoved,TimeSpent,distNorm, dMovedNorm,estimationNorm,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z) ~ Subject + Condition, RoundQ, mean)

indivwC <- indivwC_B
indivwC_6fold <- subset(indivwC,indivwC$xFold ==6 )
indiv_correl <- merge(indivwC_6fold,indivQ)
indiv_correl <- merge(indiv_correl,Q5_raw)
indiv_correl$Q_sum <- indiv_correl$Q1 + indiv_correl$Q2 + indiv_correl$Q5
indiv_correl_body <- subset(indiv_correl, Condition == 'Body')
indiv_correl_nobody <- subset(indiv_correl, Condition == 'No-body')


ggplot(indiv_correl, aes(y=Beta_B, x=Q2, color=Condition)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indiv_correl, aes(y=Beta_B, x=Q2z, color=Condition)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")

cor.test(~Beta_B + Q2 , data = indiv_correl_body, method ="spearman", continuity = TRUE, conf.level = 0.95)
