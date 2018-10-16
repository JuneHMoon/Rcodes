library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2) # plot the data
library(lme4); library(lmerTest) # used for mixed models
# 

# a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_GLM_0919.csv")
# a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_GLM_0926.csv")
# a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_GLM_Refine_1001.csv")
# a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_GLM_rRSC_1001.csv")
a_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_GLM_micro_resol_1002.csv")

a <- a_raw

aNorm <-data.frame() #Subject-wise normalization
for (i in 1:27)
{
  for (j in 1:16) # no. of ROIs
  {
    tempSubj <- subset(a, Subject == i & ROI == j)
    temp <- data.frame(Subject = tempSubj$Subject,Condition = tempSubj$Condition, ROI = tempSubj$ROI, Round = tempSubj$Round, TaskNorm = c(scale(tempSubj$Task)), RetrievalNorm = c(scale(tempSubj$Retrieval)))
    aNorm <- rbind(aNorm,temp)
  }
}
a <- merge(a,aNorm)

a_outlier <- subset(a, (a$TaskNorm > 2 | a$TaskNorm < -2 | a$RetrievalNorm > 2 | a$RetrievalNorm < -2) )


# a <- subset(a,(a$ROI != 2 & a$ROI != 3 & a$ROI != 5 & a$ROI != 6))
# a <- subset(a,(a$ROI == 1 | a$ROI == 5 | a$ROI == 8 | a$ROI == 9 | a$ROI == 11 | a$ROI == 12 | a$ROI == 14 | a$ROI == 15 | a$ROI == 16))
a <- subset(a,(a$ROI == 13 | a$ROI == 8 | a$ROI == 9 | a$ROI == 11 | a$ROI == 14 | a$ROI == 15 | a$ROI == 16))
a <- subset(a,(a$ROI == 1 | a$ROI == 5))
a <- subset(a, a$Subject != 26)
a <- subset(a,!(a$Subject == 26 & a$Round ==5))

# a <- subset(a,!(a$Subject == 13 & a$Round ==6))
# a <- subset(a,!(a$Subject == 18 & a$Round ==3))
a <- subset(a,!(a$Subject == 4 & a$Round ==2))

a <- subset(a,!(a$Subject == 26 & a$Round ==5))
a <- subset(a,!(a$Subject == 23 & (a$Round ==4 | a$Round == 5)))
a <- subset(a,!(a$Subject == 5 ))
a <- subset(a,!(a$Subject == 4 & (a$Round ==2 | a$Round == 1)))
a <- subset(a,!(a$Subject == 6 & (a$Round ==2 | a$Round == 1)))


a$Condition = ifelse(a$Condition == 0, 'No-body', 'Body') # 0 or 1

# a$Subject = factor(a$Subject).

# for 0919 data [ 1: bHCs, 2: rHC, 3: lHC, 4: bECs, 5: rEC, 6: lEC, 7: IPS, 8: RSC]
# a$ROI = ifelse(a$ROI == 1, 'bHCs', ifelse(a$ROI == 2, 'rHC', ifelse(a$ROI == 3, 'lHC', ifelse(a$ROI == 4, 'bECs',  ifelse(a$ROI == 5,  'rEC', ifelse(a$ROI == 6,  'lEC', ifelse(a$ROI == 7,  'IPS','RSC')))))))

# for 0926 data [ 1: bHCs, 2: rHC, 3: lHC, 4:bHCs_WFU, 5:bECs, 6: rEC, 7: lEC, 8:RSC, 9: paraHC, 10:paraHC_WFU, 11:Caudate, 12: AG, 13: AG_WFU, 14:Precuneus_WFU, 15:IPS, 16:rLingual Gyrus]
a$ROI = ifelse(a$ROI == 1, 'bHCs', ifelse(a$ROI == 2, 'rHC', ifelse(a$ROI == 3, 'lHC', ifelse(a$ROI == 4, 'bHCs_WFU', ifelse(a$ROI == 5, 'bECs',  ifelse(a$ROI == 6,  'rEC', ifelse(a$ROI == 7, 'lEC',
            ifelse(a$ROI == 8,  'RSC', ifelse(a$ROI == 9,  'paraHC', ifelse(a$ROI == 10,  'paraHC_WFU',  ifelse(a$ROI == 11,  'Caudate_WFU',  ifelse(a$ROI == 12,  'AG',  ifelse(a$ROI == 13,  'AG_WFU',  ifelse(a$ROI == 14, 
            'Precuneus_WFU', ifelse(a$ROI == 15,'IPS_AnaT', 'rLing')))))))))))))))

# sub_a <- subset(a,a$ROI == 'AG')
# 
# m1 = lmer(Task ~ Condition + (1|Subject),sub_a)
# m2 = lmer(Task ~ Condition + (Condition|Subject),sub_a)
# anova(m1,m2)
# summary(m1)
# anova(m1)
# summary(m2)
# anova(m2)

indivdata = aggregate(cbind(Cue, Retrieval, pmod_dError, MetaCog, Feedback, Threat, Resting, Task, TaskNorm, RetrievalNorm) ~ Subject + Condition + ROI, a, mean)


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

#Raw Beta (Cue / Retrieval / pmod_dError / MetaCog/ Feedback / Threat / Resting / Task)
# ggplot(a, aes(y=Task, x=ROI, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(jitter.width =.1 ))# + facet_wrap(~Subject)
# ggplot(a, aes(y=TaskNorm, x=ROI, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(jitter.width =.1 ))# + facet_wrap(~Subject)
# 
# ggplot(a, aes(y=Retrieval, x=ROI, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(jitter.width =.1 ))# + facet_wrap(~Subject)
# ggplot(a, aes(y=Threat, x=ROI, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(jitter.width =.1 ))# + facet_wrap(~Subject)
# ggplot(a, aes(y=Cue, x=ROI, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(jitter.width =.1 ))# + facet_wrap(~Subject)

indiv_behav = aggregate(cbind(dError,dMoved,TimeSpent,distNorm, dMovedNorm,estimationNorm,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z) ~ Subject + Condition, behav_data, mean)
indiv_behav = aggregate(cbind(dError,dMoved,TimeSpent,distNorm, dMovedNorm,estimationNorm) ~ Subject + Condition, behav_data, mean)
indiv_behav$Condition = ifelse(indiv_behav$Condition == 1, 'No-body', 'Body')

Q5_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Memory_data_fMRI_Q5_0502.csv")
Q5_raw$Condition = ifelse(Q5_raw$Condition == 1, 'No-body', 'Body')

indiv_behav <- merge(indiv_behav, Q5_raw)
# indivdata = aggregate(cbind(Cue, Retrieval, pmod_dError, MetaCog, Feedback,Navigation, Threat, Resting, Task) ~ Subject + Condition + ROI, a, mean)

indiv_behav <- merge(indivdata,indiv_behav)


######################################

b_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Betas_fMRI_comparison_pmod_0716.csv")

b <- b_raw[,-c(8:13)] 
b <- subset(b, b$Subject !=22)
#exclude Subj 12/ round 4

# b <- subset(b,!(b$Subject == 26 & b$Round ==5))
b <- subset(b, b$Subject != 26 )
b <- subset(b, b$Subject != 13 )
# b <- subset(b,!(b$Subject == 4 & b$Round == 2))
b$Condition = ifelse(b$Condition == 1, 'No-body', 'Body')

bNorm <-data.frame() #Subject-wise normalization

for (i in 1:27)
{
  for (j in 1:4) # Type ([withCond indiv/all, w/oCond indiv/all])
  {
    for( k in 1:3)
    {
      for( l in 1:3)
      {
        tempSubj <- subset(b, Subject == i & Type == j & GLM1_ROI == k & GLM2_ROI == l)
        temp <- data.frame(Subject = tempSubj$Subject,Condition = tempSubj$Condition, Type = tempSubj$Type, GLM1_ROI = tempSubj$GLM1_ROI, GLM2_ROI = tempSubj$GLM2_ROI, Round = tempSubj$Round, BetaNorm = c(scale(tempSubj$Beta)))
        
        bNorm <- rbind(bNorm,temp)
      }
    }
  }
}
b <- merge(b,bNorm)

b_outlier <- subset(b, (b$BetaNorm > 2 | b$BetaNorm < -2))

b <- subset(b, !(b$Subject == 12 & b$Round ==4))

b$Subject = factor(b$Subject)
b$Type = factor(b$Type)
b$GLM1_ROI = factor(b$GLM1_ROI)
b$GLM2_ROI = factor(b$GLM2_ROI) #ifelse(b$GLM2_ROI == 1, 'rEC',ifelse(a$GLM2_ROI == 2, 'lEC','both_ECs'))

sub_b <- subset(b,b$Type == 1 & b$GLM1_ROI == 3 & b$GLM2_ROI == 3)

# ggplot(sub_b, aes(y=Beta, x=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA)+ geom_point(alpha =.7, size=2.5,position=position_jitterdodge(jitter.width =.1 )) #+ geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)
# ggplot(sub_b, aes(y=BetaNorm, x=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(alpha =.7, size=2.5,position=position_jitterdodge(jitter.width =.1 )) #+ geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)

indiv_GridCode <- aggregate(cbind(Beta, BetaNorm) ~ Subject + Condition + Type + GLM1_ROI + GLM2_ROI, sub_b, mean)

ggplot(indiv_GridCode, aes(y=Beta, x=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA)+ geom_point(alpha =.7, size=2.5,position=position_jitterdodge(jitter.width =.1 )) #+ geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)
ggplot(indiv_GridCode, aes(y=BetaNorm, x=Condition, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(alpha =.7, size=2.5,position=position_jitterdodge(jitter.width =.1 )) #+ geom_jitter(width=.1,height=0,alpha=.5,size=3) #+ facet_wrap(~Subject)

ggplot(indivdata, aes(y=Task, x=ROI, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(jitter.width =.1 )) + ylab("Mean Betas during Task") # + facet_wrap(~Subject)
ggplot(indivdata, aes(y=TaskNorm, x=ROI, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(jitter.width =.1 ))# + facet_wrap(~Subject)
ggplot(indivdata, aes(y=Retrieval, x=ROI, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(jitter.width =.1 )) + ylab("Mean Betas during Retrieval")# + facet_wrap(~Subject)
ggplot(indivdata, aes(y=RetrievalNorm, x=ROI, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(jitter.width =.1 ))# + facet_wrap(~Subject)

ggplot(indivdata, aes(y=Cue, x=ROI, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(jitter.width =.1 ))# + facet_wrap(~Subject)
ggplot(indivdata, aes(y=Feedback, x=ROI, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(jitter.width =.1 ))# + facet_wrap(~Subject)
# ggplot(indivdata, aes(y=Navigation, x=ROI, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(jitter.width =.1 ))# + facet_wrap(~Subject)

ggplot(indivdata, aes(y=Threat, x=ROI, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(jitter.width =.1 ))# + facet_wrap(~Subject)
ggplot(indivdata, aes(y=pmod_dError, x=ROI, color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_point(position=position_jitterdodge(jitter.width =.1 ))# + facet_wrap(~Subject)

indivdata <- merge(indivdata,indiv_GridCode)

sub_indiv <- subset(indivdata,indivdata$ROI == 'bHCs') 
sub_indiv <- sub_indiv %>% group_by(Subject) %>% mutate(Body_Effect = (Task[Condition=='Body'] - Task[Condition=='No-body']))
sub_indiv <- sub_indiv %>% group_by(Subject) %>% mutate(Body_Effect2 = (Retrieval[Condition=='Body'] - Retrieval[Condition=='No-body']))

wilcox.test(sub_indiv$Task[sub_indiv$Condition == 'Body'], sub_indiv$Task[sub_indiv$Condition == 'No-body'], paired=T)
wilcox.test(sub_indiv$Task[sub_indiv$Condition == 'Body'], sub_indiv$Task[sub_indiv$Condition == 'No-body'], alternative = "greater", paired=T) #greater
wilcox.test(sub_indiv$TaskNorm[sub_indiv$Condition == 'Body'], sub_indiv$TaskNorm[sub_indiv$Condition == 'No-body'], paired=T)

wilcox.test(sub_indiv$Retrieval[sub_indiv$Condition == 'Body'], sub_indiv$Retrieval[sub_indiv$Condition == 'No-body'], paired=T)
wilcox.test(sub_indiv$Retrieval[sub_indiv$Condition == 'Body'], sub_indiv$Retrieval[sub_indiv$Condition == 'No-body'], alternative = "greater", paired=T) #greater
wilcox.test(sub_indiv$RetrievalNorm[sub_indiv$Condition == 'Body'], sub_indiv$RetrievalNorm[sub_indiv$Condition == 'No-body'], paired=T)

wilcox.test(sub_indiv$Cue[sub_indiv$Condition == 'Body'], sub_indiv$Cue[sub_indiv$Condition == 'No-body'], paired=T)
wilcox.test(sub_indiv$Feedback[sub_indiv$Condition == 'Body'], sub_indiv$Feedback[sub_indiv$Condition == 'No-body'], paired=T)
# wilcox.test(sub_indiv$Navigation[sub_indiv$Condition == 'Body'], sub_indiv$Navigation[sub_indiv$Condition == 'No-body'], paired=T)

# ggplot(indivdata, aes(y=Task[indivdata$ROI == 'RSC'], x=Task[indivdata$ROI == 'Caudate_WFU'], color=Condition)) + geom_point(alpha=.7,size=3) + geom_smooth(method="lm", se = TRUE, fill = "NA")



ROI_correl1 = gather(indivdata, key = Task, value = ROI)
# indivQ = aggregate(cbind(Q1z,Q2z,Q3z,Q4z) ~ Condition + Subject,a,mean)
# indivQ =  gather(indivQ,Quest,rating,Q1z:Q4z)

roi1 <- indivdata$Task[indivdata$ROI == 'bHCs']
roi2 <- indivdata$Task[indivdata$ROI == 'IPS_AnaT']
roi1 <- indivdata$Retrieval[indivdata$ROI == 'Caudate_WFU']
roi2 <- indivdata$Retrieval[indivdata$ROI == 'IPS_AnaT']
ROI_correl <- data.frame("ROI1" = roi1, "ROI2" = roi2)
ggplot(ROI_correl ,aes(y=roi1,x=roi2))  + geom_point(alpha=.7,size=3) + geom_smooth(method="lm", se = TRUE, fill = "NA")
cor.test(~roi1 + roi2 , data = ROI_correl, method ="spearman", continuity = TRUE, conf.level = 0.95)


sub_indiv <- subset(indiv_behav,indiv_behav$ROI == 'bHCs') 

sub_indivB <- subset(sub_indiv, sub_indiv$Condition == 'Body')
sub_indivN <- subset(sub_indiv, sub_indiv$Condition == 'No-body')
# TaskNorm
# Task
# RetrievalNorm
# Retrieval
ggplot(sub_indiv, aes(y=distNorm, x=TaskNorm, color=Condition)) + geom_point(alpha=.7,size=3) + geom_smooth(method="lm", se = TRUE, fill = "NA")
ggplot(sub_indiv, aes(y=dError, x=Task, color=Condition)) + geom_point(alpha=.7,size=3) + geom_smooth(method="lm", se = TRUE, fill = "NA")
ggplot(sub_indiv, aes(y=distNorm, x=TaskNorm)) + geom_point(alpha=.7,size=3) + geom_smooth(method="lm", se = TRUE, fill = "NA")
ggplot(sub_indiv, aes(y=dError, x=Task)) + geom_point(alpha=.7,size=3) + geom_smooth(method="lm", se = TRUE, fill = "NA")
ggplot(sub_indiv, aes(y=distNorm, x=RetrievalNorm, color=Condition)) + geom_point(alpha=.7,size=3) + geom_smooth(method="lm", se = TRUE, fill = "NA")
ggplot(sub_indiv, aes(y=dError, x=Retrieval, color=Condition)) + geom_point(alpha=.7,size=3) + geom_smooth(method="lm", se = TRUE, fill = "NA")
ggplot(sub_indiv, aes(y=distNorm, x=RetrievalNorm)) + geom_point(alpha=.7,size=3) + geom_smooth(method="lm", se = TRUE, fill = "NA")
ggplot(sub_indiv, aes(y=dError, x=Retrieval)) + geom_point(alpha=.7,size=3) + geom_smooth(method="lm", se = TRUE, fill = "NA")

sub_indiv <- subset(indivdata,indivdata$ROI == 'IPS_AnaT') 

ggplot(sub_indiv, aes(y=Beta, x=Retrieval, color=Condition)) + geom_point(alpha=.7,size=3) + geom_smooth(method="lm", se = TRUE, fill = "NA")
ggplot(sub_indiv, aes(y=Beta, x=Retrieval)) + geom_point(alpha=.7,size=3) + geom_smooth(method="lm", se = TRUE, fill = "NA")

cor.test(~Beta + Retrieval , data = sub_indiv, method ="spearman", continuity = TRUE, conf.level = 0.95)


cor.test(~distNorm + TaskNorm , data = sub_indiv, method ="spearman", continuity = TRUE, conf.level = 0.95)
cor.test(~dError + Task, data = sub_indiv, method ="spearman", continuity = TRUE, conf.level = 0.95)
cor.test(~distNorm + RetrievalNorm , data = sub_indiv, method ="spearman", continuity = TRUE, conf.level = 0.95)
cor.test(~dError + Retrieval, data = sub_indiv, method ="spearman", continuity = TRUE, conf.level = 0.95)

cor.test(~BetaNorm + dError , data = sub_indiv, method ="spearman", continuity = TRUE, conf.level = 0.95)




ggplot(sub_indiv, aes(y=Task, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))
ggplot(sub_indiv, aes(y=TaskNorm, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))

ggplot(sub_indiv, aes(y=Retrieval, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect2>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))
ggplot(sub_indiv, aes(y=RetrievalNorm, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))

ggplot(sub_indiv, aes(y=Cue, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))
ggplot(sub_indiv, aes(y=Feedback, x=Condition, group=Subject, color=Condition)) + geom_line(aes(color=Body_Effect>0),size = 0.8) + geom_point(size = 2.5) + scale_color_manual(values=c("#F8766D", "#00BFC4", "#00BFC4", "#F8766D"))

# t.test(sub_indiv$Cue[sub_indiv$Condition == 'Body'], sub_indiv$Cue[sub_indiv$Condition == 'No-body'], paired=T)

