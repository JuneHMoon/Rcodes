rm(list=ls(all=TRUE)) # clean

library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2) # plot the data
library(lme4); library(lmerTest) # used for mixed models

##Study1 : Encoding with 2 Conditions ################gvg

study_raw = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Memory_data_fMRI_0410.csv")
# study_raw = read_csv("E:/June/Experiment Data/Memory and Self/Behavioral Study/Main_EnR/Memory_data_EnR_1001.csv")
# study_raw = read_csv("E:/June/Experiment Data/Memory and Self/Behavioral Study/Main_E/Memory_data_0804.csv")

#study1_raw$Subject = as.numeric(study_raw$Subject)
study_raw$Condition = factor(study_raw$Condition)
# study_raw$Group = ifelse((study_raw$Subject)%%3 == 1, 1,ifelse((study_raw$Subject)%%3 == 2, 2,3))
study_raw$Group = ifelse((study_raw$Subject)%%2 == 1, 1, 2)
study_raw$Subject  = factor(study_raw$Subject)
study_raw$Round_fac = factor(study_raw$Round)
study_raw$Index_fac = factor(study_raw$Index)
study_raw$TimeSpent = study_raw$EndTime - study_raw$StartTime
# study_raw$FBTimeSpent = study_raw$FB_EndTime - study_raw$EndTime
study_raw$ErrorMetaCog = study_raw$dError - study_raw$estimation_dError
study_raw$Position = factor(study_raw$Position)

study_encoding = read_csv("D:/Experiment Data/Navigation and Self/GridCode_Analysis/Memory_data_fMRI_Encoding_0410.csv")
# study_encoding = read_csv("E:/June/Experiment Data/Memory and Self/Behavioral Study/Main_EnR/Memory_data_EnR_Encoding_1001.csv")
# study_encoding = read_csv("E:/June/Experiment Data/Memory and Self/Behavioral Study/Main_E/Memory_data_Encoding_1004.csv")

study_encoding$Subject  = factor(study_encoding$Subject)
study_encoding$Round_fac = factor(study_encoding$Round)

######################################################

# Exclusion1 - bad participant (dError > 55)
#study1 <- subset(study1_raw, Subject != 14)
study1 <- study_raw

# Exclusion2 - manipulation error ( moved distance < 5 || manipulation < 1 )
study1_miss <- subset(study1,((dMoved < 5 | nManipulation < 1 | TimeSpent < 3.5) & (estimation_dError > 50 | dError > 25))| estimation_dError > 70)
study1_excl <- subset(study1,!(((dMoved < 5 | nManipulation < 1 | TimeSpent < 3.5) & (estimation_dError > 50 | dError > 25))| estimation_dError > 70))
#study1_excl <- subset(study1_excl,Index!= 14)

## Exclusion3 - Round based (dError > 55)

threshold = 55;
## extract round info for Study1

a <- study1_excl

distNorm <-data.frame()
for (i in 1:23)
{
  if (i != 22)
  {
    temp <- subset(a, Subject == i)
    distNorm <- rbind(distNorm,scale(temp$dError))
  }
}

a$distNorm <- distNorm$V1
# a <- subset(a, Subject != 14)
study1_excl <- a

rounddata_median1 = aggregate(cbind(dError,distNorm,Condition,Group,estimation_dError,ErrorMetaCog) ~ Subject + Round_fac,a,median)
rounddata_median_withQ = aggregate(cbind(dError,distNorm,Condition,Group,estimation_dError,ErrorMetaCog,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z) ~ Subject + Round_fac,a,median)
rounddata_mean_withQ = aggregate(cbind(dError,distNorm,Condition,Group,estimation_dError,ErrorMetaCog,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z) ~ Subject + Round_fac,a,mean)

rounddata_median_withQ$Condition = ifelse(rounddata_median_withQ$Condition == 1, 'No-body', 'Body')
rounddata_mean_withQ$Condition = ifelse(rounddata_mean_withQ$Condition == 1, 'No-body', 'Body')

rounddata_mean1 = aggregate(cbind(dMoved,TimeSpent,directionChange,nManipulation) ~ Subject + Round_fac,a,mean) 
rounddata1 <- merge(rounddata_median1,rounddata_mean1)
rounddata_withQ <- merge (rounddata_median_withQ,rounddata_mean1)
round_excl1 <- subset(rounddata1,dError >= threshold )
threshold_IQR1 = IQR(rounddata1$dError)*2
round_IQR1 <- subset(rounddata1,dError >= threshold_IQR1)

round_info1 <- merge(study_encoding,rounddata1)
temp <- subset(study_encoding, !is.na(Subject) & Subject != 1)
round_info_withQ <- merge(temp,rounddata_withQ)
# round_info1 <- rounddata1
round_info_excl1 <- subset(round_info1, round_info1$dError < 55)
round_info_raw1 <- round_info1
round_info1 <- round_info_excl1
# because there is no outlier round. Skip!

# round_info_excl1 <- subset(round_info1, dError < threshold)
# round_info_IQR1 <- subset(round_info1, dError < threshold_IQR1)
# 
# round_info1_b1 <- subset(round_info1,Block != 1)

#####################
round_mean_sd1 <- round_info1 %>% group_by(Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
round_mean_sd1$Condition = ifelse(round_mean_sd1$Condition == 1, 'No-body','Body')

## Exclusion4 - Round 1
# confirm Round effect
round_info_withQ$Condition = ifelse(round_info_withQ$Condition == 1, 'No-body','Body')
round_info1$Condition = ifelse(round_info1$Condition == 1, 'No-body','Body')

round_info1$Performance = round_info1$dError / (round_info1$TimeSpent + 10)
  
ggplot(round_info1, aes(y=dError, x=Round_fac, color=Round_fac)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1)
ggplot(round_info1, aes(y=distNorm, x=Round_fac, color=Round_fac)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1)

ggplot(rounddata_withQ, aes(y=Q1z, x=Condition, color=Round_fac)) + geom_boxplot() #+ geom_jitter(width=.1,height=0,alpha=.5,size=1)

# 
# 
# study1_z = subset(study1,distNorm < 2)
# study2_z = subset(study2,distNorm < 2 )
# study1_excl_z = subset(study1_excl,distNorm < 2 )
# study2_excl_z = subset(study2_excl,distNorm < 2 )
# study1_excl_r1_z = subset(study1_excl_r1,distNorm < 2 )
# study2_excl_r1_z = subset(study2_excl_r1,distNorm < 2 )
###############################


##############
# exclusion of subjects without subjective body effect
# 
# a <- study1_excl
# a$Condition = ifelse(a$Condition == 1, 'No-body',ifelse(a$Condition == 2, 'Body','Object'))
# a$Condition2 <- ifelse(a$Condition=='No-body','aNo-body',a$Condition)
# 
# a_effect <- a
# 
# for (i in 7:32)
# {
#   if (i == 14)
#   {
#     next
#   }
#   
#   temp <- subset(a, Subject == i)
#   temp2 = aggregate(cbind(Q1,Q2,Q3,Q4,Q5) ~ Condition, temp, median)
#   temp2$Q1[temp2$Condition=='Body']
#   temp2$Q1[temp2$Condition=='No-body']
#   # print(i)
#   # print(temp2$Q1[temp2$Condition=='Body'])
#   if(temp2$Q1[temp2$Condition=='Body'] < 0 | (temp2$Q1[temp2$Condition=='Body'] < temp2$Q1[temp2$Condition=='No-body']))
#   {
#     print(i)
#     a_effect <- subset(a_effect, Subject != i)
#   } 
# }
# 
# study1_excl_effect <- a_effect

############## assess difference
a <- study1_excl
# a2 <- subset(a,  Index != 14) #Index != 1 &
# a<- a2
# a <- subset(a, Subject != 24 & Subject !=27)
a$Condition = ifelse(a$Condition == 1, 'No-body','Body')
a$Group =  ifelse(a$Group == 1, 'No-body First', 'Body First')

# indivdata1 = aggregate(cbind(dError,dMoved,TimeSpent,directionChange,nManipulation,estimation_dError,ErrorMetaCog,Q1,Q2,Q3,Q4,Q5) ~ Condition + Subject,a,median)
# indivdata_mean1 = aggregate(cbind(dError,dMoved,TimeSpent,directionChange,nManipulation, estimation_dError,ErrorMetaCog,Q1,Q2,Q3,Q4,Q5) ~ Condition + Subject,a,mean)
indivdata = aggregate(cbind(dError,distNorm,dMoved,TimeSpent,directionChange,nManipulation,totalAngle,estimation_dError,ErrorMetaCog) ~ Condition + Subject + Group,a,median)
indivdata1 = aggregate(cbind(dError,distNorm,dMoved,TimeSpent,directionChange,nManipulation,totalAngle,estimation_dError,ErrorMetaCog,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z) ~ Condition + Subject + Group,a,median)
indivdata_mean = aggregate(cbind(dError,distNorm,dMoved,TimeSpent,directionChange,nManipulation,totalAngle, estimation_dError,ErrorMetaCog) ~ Condition + Subject+Group,a,mean)
indivdata_mean1 = aggregate(cbind(dError,distNorm,dMoved,TimeSpent,directionChange,nManipulation,totalAngle, estimation_dError,ErrorMetaCog,Q1,Q2,Q3,Q4,Q1z,Q2z,Q3z,Q4z) ~ Condition + Subject+Group,a,mean)

a2 <- subset(a, Index != 14)
indivdata_wo_last = aggregate(cbind(dError,distNorm,dMoved,TimeSpent,directionChange,nManipulation,estimation_dError,ErrorMetaCog) ~ Condition + Subject + Group,a2,median)
indivdata_mean_wo_last = aggregate(cbind(dError,distNorm,dMoved,TimeSpent,directionChange,nManipulation,estimation_dError,ErrorMetaCog) ~ Condition + Subject + Group,a2,mean)


a$Performance = a$dError / (a$TimeSpent + 10)
indivdata_test = aggregate(cbind(dError,distNorm,dMoved,TimeSpent,directionChange,nManipulation,estimation_dError,ErrorMetaCog,Performance) ~ Condition + Index,a,median)
indivdata_mean_test = aggregate(cbind(dError,distNorm,dMoved,TimeSpent,directionChange,nManipulation,estimation_dError,ErrorMetaCog,Performance) ~ Condition + Index,a,mean)

ggplot(indivdata_test, aes(y=dError, x=Index, color=Condition)) + geom_line()+ geom_point()
ggplot(indivdata_test, aes(y=distNorm, x=Index, color=Condition)) + geom_line()+ geom_point()
ggplot(indivdata_mean_test, aes(y=distNorm, x=Index, color=Condition)) + geom_line()+ geom_point()

ggplot(indivdata_test, aes(y=distNorm, x=Index, color=Condition)) + geom_line()+ geom_point()
ggplot(indivdata_test, aes(y=Performance, x=Index, color=Condition)) + geom_line()+ geom_point()

# ggplot(indivdata_mean_test, aes(y=dError, x=Index, color=Condition)) + geom_line()+ geom_point()

ggplot(a, aes(y=dError, x=Condition,color=Condition)) + geom_boxplot() + facet_wrap(~Subject) #+ ylim(0,50)
ggplot(a, aes(y=distNorm, x=Condition,color=Condition)) + geom_boxplot() + facet_wrap(~Subject) # + ylim(-1,1)
ggplot(a, aes(y=dError, x=Condition,color=Condition)) + geom_boxplot() #+ facet_wrap(~Subject) #+ ylim(0,50)
ggplot(a, aes(y=distNorm, x=Condition,color=Condition)) + geom_boxplot() #+ facet_wrap(~Subject) # + ylim(-1,1)

ggplot(a, aes(y=dError, x=Group,color=Group)) + geom_boxplot(outlier.shape = NA) 
ggplot(a, aes(y=dMoved, x=Condition,color=Condition)) + geom_boxplot() + facet_wrap(~Subject)
ggplot(a, aes(y=dMoved, x=Condition,color=Condition)) + geom_boxplot() 

ggplot(indivdata, aes(y=dError, x=Condition,color=Condition)) + geom_boxplot() #+ facet_wrap(~Subject)
ggplot(indivdata, aes(y=distNorm, x=Condition,color=Condition)) + geom_boxplot() #+ facet_wrap(~Subject)

ggplot(indivdata_mean, aes(y=dError, x=Condition,color=Condition)) + geom_boxplot() #+ facet_wrap(~Subject)

ggplot(indivdata, aes(y=dError, x=Condition,color=Condition, group =Subject)) +  geom_line(color="black") + geom_point() 
ggplot(indivdata, aes(y=distNorm, x=Condition,color=Condition, group =Subject)) +  geom_line(color="black") + geom_point() 

ggplot(indivdata, aes(y=distNorm, x=Condition,color=Condition)) + geom_boxplot() + facet_wrap(~Subject)
ggplot(indivdata_mean, aes(y=distNorm, x=Condition,color=Condition)) + geom_boxplot() + facet_wrap(~Subject)

ggplot(indivdata, aes(y=dMoved, x=Condition,color=Condition)) + geom_boxplot() #+ facet_wrap(~Subject)
ggplot(indivdata_mean, aes(y=dMoved, x=Condition,color=Condition)) + geom_boxplot() #+ facet_wrap(~Subject)

ggplot(indivdata, aes(y=dMoved, x=Condition,color=Condition)) + geom_boxplot() #+ facet_wrap(~Subject)
ggplot(indivdata_mean, aes(y=dMoved, x=Condition,color=Condition)) + geom_boxplot() #+ facet_wrap(~Subject)

# ggplot(indivdata1, aes(y=dError, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) 
# ggplot(indivdata_mean1, aes(y=dError, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) 

# ggplot(indivdata, aes(y=dError, x=Group,color=Group)) + geom_boxplot(outlier.shape = NA) 
# ggplot(indivdata_mean, aes(y=dError, x=Group,color=Group)) + geom_boxplot(outlier.shape = NA) 

ggplot(round_info1, aes(y=dError, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + facet_wrap(~Subject) 
ggplot(round_info1, aes(y=distNorm, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + facet_wrap(~Subject)
ggplot(round_info1, aes(y=TimeSpent, x=Round_fac,color=Round_fac)) + geom_boxplot(outlier.shape = NA) + facet_wrap(~Subject)
ggplot(round_info1, aes(y=Performance, x=Round_fac,color=Round_fac)) + geom_boxplot(outlier.shape = NA) + facet_wrap(~Subject)

ggplot(round_info1, aes(y=Performance, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + facet_wrap(~Subject)

# indiv_mean_sd1 <- indivdata_mean1 %>% group_by(Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
# indiv_median_mean_sd1 <- indivdata1 %>% group_by(Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
# 
# indiv_group_mean_sd1 <- indivdata_mean1 %>% group_by(Group) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
# indiv_group_median_mean_sd1 <- indivdata1 %>% group_by(Group) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

indiv_mean_sd <- indivdata_mean %>% group_by(Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
indiv_median_mean_sd <- indivdata %>% group_by(Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

indiv_wo_1stT_mean_sd <- indivdata_mean_wo_1stT %>% group_by(Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
indiv_wo_1stT_median_mean_sd <- indivdata_wo_1stT %>% group_by(Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

indiv_group_mean_sd <- indivdata_mean %>% group_by(Group) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
indiv_group_median_mean_sd <- indivdata %>% group_by(Group) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

ggplot(indiv_mean_sd, aes(y=dError_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=dError_mean-dError_se,ymax=dError_mean+dError_se), size = .3, width=.2,position=position_dodge(.9)) #+ ylim(0,20)
ggplot(indiv_mean_sd, aes(y=distNorm_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=distNorm_mean-distNorm_se,ymax=distNorm_mean+distNorm_se), size = .3, width=.2,position=position_dodge(.9)) # ylim(-0.17,0.17)

ggplot(indiv_median_mean_sd, aes(y=dError_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=dError_mean-dError_se,ymax=dError_mean+dError_se), size = .3, width=.2,position=position_dodge(.9)) 
ggplot(indiv_median_mean_sd, aes(y=distNorm_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=distNorm_mean-distNorm_se,ymax=distNorm_mean+distNorm_se), size = .3, width=.2,position=position_dodge(.9)) # ylim(-0.17,0.17)

ggplot(indiv_group_median_mean_sd, aes(y=dError_mean, x=Group,fill=Group)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=dError_mean-dError_se,ymax=dError_mean+dError_se), size = .3, width=.2,position=position_dodge(.9)) #+ ylim(0,20)
ggplot(indiv_group_mean_sd, aes(y=distNorm_mean, x=Group,fill=Group)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=distNorm_mean-distNorm_se,ymax=distNorm_mean+distNorm_se), size = .3, width=.2,position=position_dodge(.9)) # ylim(-0.17,0.17)

ggplot(indiv_median_mean_sd, aes(y=dMoved_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=dMoved_mean-dMoved_se,ymax=dMoved_mean+dMoved_se), size = .3, width=.2,position=position_dodge(.9))  + coord_cartesian(ylim=c(75,90))
ggplot(indiv_mean_sd, aes(y=dMoved_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=dMoved_mean-dMoved_se,ymax=dMoved_mean+dMoved_se), size = .3, width=.2,position=position_dodge(.9))  + coord_cartesian(ylim=c(75,90))

ggplot(indiv_median_mean_sd, aes(y=TimeSpent_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=TimeSpent_mean-TimeSpent_se,ymax=TimeSpent_mean+TimeSpent_se), size = .3, width=.2,position=position_dodge(.9))  #+ coord_cartesian(ylim=c(75,90))
ggplot(indiv_mean_sd, aes(y=TimeSpent_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=TimeSpent_mean-TimeSpent_se,ymax=TimeSpent_mean+TimeSpent_se), size = .3, width=.2,position=position_dodge(.9))  #+ coord_cartesian(ylim=c(75,90))

# ggplot(indiv_wo_1stT_median_mean_sd, aes(y=dMoved_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=dMoved_mean-dMoved_se,ymax=dMoved_mean+dMoved_se), size = .3, width=.2,position=position_dodge(.9))  + coord_cartesian(ylim=c(75,95))
# ggplot(indiv_wo_1stT_mean_sd, aes(y=dMoved_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=dMoved_mean-dMoved_se,ymax=dMoved_mean+dMoved_se), size = .3, width=.2,position=position_dodge(.9))  + coord_cartesian(ylim=c(75,95))

 
# a_merge$Condition = ifelse(a_merge$Condition == 1, 'No-body',ifelse(a_merge$Condition == 2, 'Body','Object'))
# a_merge$Condition2 <- ifelse(a_merge$Condition=='No-body','aNo-body',a_merge$Condition)
a_test <- subset(a, a$Index != 1)
ggplot(a, aes(x=dError,y=..density..)) + geom_density() + facet_wrap(~Subject,scales='free')
ggplot(a, aes(x=dError,y=..density..)) + geom_density() + facet_wrap(~Subject)

shapiro.test(a$dError)

ggplot(a, aes(x=dError,y=..density..)) + geom_histogram()  + geom_density() #+ xlim(0,20)
ggplot(a, aes(x=dError,y=..density.., group=Condition,  color=Condition)) + geom_density() + facet_wrap(~Subject,scales='free')
ggplot(a, aes(x=distNorm,y=..density.., group=Condition,  color=Condition)) + geom_density() + facet_wrap(~Subject,scales='free')
ggplot(a, aes(x=distNorm,y=..density.., group=Condition,  color=Condition)) + geom_density()
ggplot(a, aes(x=dError,y=..density.., group=Condition,  color=Condition)) + geom_density()

ggplot(a, aes(x=log(dError),y=..density.., group=Condition,  color=Condition)) + geom_density()

ggplot(round_info_excl1, aes(x=distNorm,y=..density.., group=Condition,  color=Condition)) + geom_density()
ggplot(indivdata_mean1, aes(x=distNorm,y=..density.., group=Condition,  color=Condition)) + geom_density()


ggplot(a_test, aes(x=TimeSpent,y=..density.., group=Condition,  color=Condition)) + geom_density()
ggplot(round_info1, aes(x=EN_TimeSpent,y=..density.., group=Condition,  color=Condition)) + geom_density()


ggplot(a_test, aes(x=dMoved,y=..density.., group=Condition,  color=Condition)) + geom_density()
ggplot(a_test, aes(x=TimeSpent,y=..density.., group=Condition,  color=Condition)) + geom_density()
ggplot(a, aes(x=totalAngle,y=..density.., group=Condition,  color=Condition)) + geom_density()
ggplot(a, aes(x=nManipulation,y=..density.., group=Condition,  color=Condition)) + geom_density()
ggplot(a, aes(x=directionChange,y=..density.., group=Condition,  color=Condition)) + geom_density()
ggplot(round_info1, aes(x=EN_nManipulation,y=..density.., group=Condition,  color=Condition)) + geom_density()

ggplot(a_test, aes(x=dMoved,y=..density.., group=Condition,  color=Condition)) + geom_density() + facet_wrap(~Subject,scales='free')



ggplot(a, aes(x=distNorm,y=..density.., group=Condition,  color=Condition)) + geom_density() + facet_wrap(~Group,scales='free')
ggplot(a_test, aes(x=distNorm,y=..density.., group=Condition,  color=Condition)) + geom_density() + facet_wrap(~Group,scales='free')
ggplot(a, aes(x=distNorm,y=..density.., group=Group,  color=Group)) + geom_density() 

ggplot(round_info1, aes(x=dError,y=..density.., group=Condition,  color=Condition)) + geom_density() #+ facet_wrap(~Subject,scales='free')

ggplot(a, aes(x=dError,y=..density.., group=Round_fac, color=Round_fac)) + geom_density()
ggplot(a2, aes(y=dError, x=Position, color=Position)) + geom_boxplot()

ggplot(a, aes(x=estimation_dError,y=..density.., group=Condition,  color=Condition)) + geom_density() + facet_wrap(~Subject,scales='free')
ggplot(a, aes(x=ErrorMetaCog,y=..density.., group=Condition, color=Condition)) + geom_density()

ggplot(a, aes(y=dError, x=Condition, color=Subject)) + geom_jitter(width=.05,height=0,alpha=.7,size=0.3)

ggplot(a, aes(y=dError, x=Condition, color=Subject)) + geom_jitter(width=.05,height=0,alpha=.7,size=0.5) + geom_smooth(aes(group = Subject), method="lm", se = FALSE, fill = "NA")
# ggplot(subset(a,Condition != 'No-body'), aes(y=dError, x=Condition, color=Subject)) + geom_jitter(width=.05,height=0,alpha=.7,size=0.3) + geom_smooth(aes(group = Subject), method="lm", se = FALSE, fill = "NA")

# ggplot(subset(a,Condition != 'Object'), aes(y=dError, x=Condition, color=Subject)) + geom_point(alpha=.3,size=0.3) + geom_smooth(method='lm', fill = 'NA') #+ facet_wrap(~Subject) 


# indivdata = aggregate(cbind(dError,dMoved,TimeSpent,directionChange,nManipulation,estimation_dError,ErrorMetaCog,Q1,Q2,Q3,Q4,Q5) ~ Condition + Subject,a,median)
# indivdata_mean = aggregate(cbind(dError,dMoved,TimeSpent,directionChange,nManipulation, estimation_dError,ErrorMetaCog,Q1,Q2,Q3,Q4,Q5) ~ Condition + Subject,a,mean)
# 
# 
# indivdata_excl_mean = aggregate(cbind(dError,dMoved,TimeSpent,directionChange,nManipulation, estimation_dError,ErrorMetaCog,Q1,Q2,Q3,Q4,Q5) ~ Condition + Subject,a_excl,mean)
# indivdata_excl = aggregate(cbind(dError,dMoved,TimeSpent,directionChange,nManipulation, estimation_dError,ErrorMetaCog,Q1,Q2,Q3,Q4,Q5) ~ Condition + Subject,a_excl,median)
# 

# test <- merge(indivdata_mean,indivdata_TA_mean)
# 
# indivdata_raw = aggregate(cbind(dError,dMoved,TimeSpent,directionChange,nManipulation,estimation_dError,ErrorMetaCog) ~ Condition + Subject,a_raw,median)
# indivdata_raw_mean = aggregate(cbind(dError,dMoved,TimeSpent,directionChange,nManipulation, estimation_dError,ErrorMetaCog) ~ Condition + Subject,a_raw,mean)
# indivdata = aggregate(cbind(dError,estimation_dError,ErrorMetaCog) ~ Condition + Subject,a,median)
# indivdata = aggregate(cbind(dMoved,TimeSpent,directionChange,nManipulation) ~ Condition + Subject,a,mean)

# ggplot(round_info_excl2, aes(y=dError, x=Round_fac, color=Round_fac)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=1)

#indivdata$medError = aggregate(dError ~ Condition + Subject,a,median)$dError

ggplot(indivdata, aes(y=dError, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) #+ facet_wrap(~Subject)
ggplot(indivdata_mean, aes(y=dError, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) #+ facet_wrap(~Subject)

ggplot(round_info1, aes(y=distNorm, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.05,height=0,alpha=.7,size=3)# + facet_wrap(~Subject)

p <- ggplot(indivdata, aes(y=distNorm, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.05,height=0,alpha=.7,size=3) + labs(y = "Median dError (Z-scored)")
ggplot(indivdata, aes(y=distNorm, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA)  + facet_wrap(~Subject)

p <- ggplot(indiv_median_mean_sd, aes(y=distNorm_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=distNorm_mean-distNorm_se,ymax=distNorm_mean+distNorm_se), size = .3, width=.2,position=position_dodge(.9))  + labs(y = "Median dError (Z-scored)")

p + theme(
  #plot.title = element_text(color="red", size=14, face="bold.italic"),
  axis.title.x = element_text(size=22, face="bold"),
  axis.title.y = element_text(size=22, face="bold"),
  #axis.text = element_text(size=14),
  legend.text = element_text(size=22),
  legend.title = element_text(size=18)
)


ggplot(indivdata_mean, aes(y=distNorm, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.05,height=0,alpha=.7,size=1.5) #+ facet_wrap(~Subject)
ggplot(indivdata_mean, aes(y=distNorm, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + facet_wrap(~Subject)

ggplot(indivdata, aes(y=estimation_dError, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + facet_wrap(~Subject)
ggplot(indivdata, aes(y=estimation_dError, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) #+ facet_wrap(~Subject)
ggplot(indivdata_mean, aes(y=estimation_dError, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) #+ facet_wrap(~Subject)

ggplot(indivdata, aes(y=ErrorMetaCog, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + facet_wrap(~Subject)
ggplot(indivdata, aes(y=ErrorMetaCog, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) #+ facet_wrap(~Subject)
ggplot(indivdata_mean, aes(y=ErrorMetaCog, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) #+ facet_wrap(~Subject)

ggplot(indivdata, aes(y=totalAngle, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + facet_wrap(~Subject)
ggplot(indivdata, aes(y=totalAngle, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) #+ facet_wrap(~Subject)
ggplot(indivdata_mean, aes(y=totalAngle, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) #+ facet_wrap(~Subject)


###################### mixed model
sub_a = subset(a, Index != 1 )
sub_a = a
round_info_excl1$Condition = ifelse(round_info_excl1$Condition == 1, 'No-body','Body')

# m1 = glmer(distNorm ~ Condition  + (1|Subject),round_info_excl1, family=Gamma(link=log))
# m2 = glmer(distNorm ~ Condition  + (Condition|Subject),round_info_excl1,family=Gamma(link=log))
# # 
# # 
# m1 = lmer(distNorm ~ Condition  + (1|Subject),round_info_excl1)
# m2 = lmer(distNorm ~ Condition  + (Condition|Subject),round_info_excl1)
# 
# m1 = glmer(distNorm ~ Condition  + (1|Subject),round_info_excl1, family=Gamma(link=log))
# m2 = glmer(distNorm ~ Condition  + (Condition|Subject),round_info_excl1,family=Gamma(link=log))
# # 
# # 
m1 = lmer(dError ~ Condition  + (1|Subject),indivdata_mean)
m2 = lmer(dError ~ Condition  + (Condition|Subject),indivdata_mean)



# m1 = lmer(distNorm ~ Condition  + (1|Subject),a)
# m2 = lmer(distNorm ~ Condition  + (Condition|Subject),a)

m1 = glmer(distNorm ~ Condition  + (1|Subject),sub_a, family=Gamma(link=log))
m2 = glmer(distNorm ~ Condition  + (Condition|Subject),sub_a,family=Gamma(link=log))
# # 
# # 
anova(m1,m2)

summary(m1)
anova(m1)
summary(m2)
anova(m2)

m1 = lmer(TimeSpent ~ Condition  + (1|Subject),a)
m2 = lmer(TimeSpent ~ Condition  + (Condition|Subject),a)
# 
anova(m1,m2)

summary(m1)
anova(m1)
summary(m2)
anova(m2)

m1 = lmer(EN_TimeSpent ~ Condition  + (1|Subject),round_info1)
m2 = lmer(EN_TimeSpent ~ Condition  + (Condition|Subject),round_info1)
# 
anova(m1,m2)

summary(m1)
anova(m1)
summary(m2)
anova(m2)

m1 = glmer(distNorm ~ Condition+  (1|Subject),a, family=Gamma())
m2 = glmer(distNorm ~ Condition+ (Condition|Subject),a, family=Gamma())

anova(m1,m2)

summary(m1)
anova(m1)
summary(m2)
anova(m2)

m1 = lmer(EN_dMoved ~ Condition+  (1|Subject),round_info1)
m2 = lmer(EN_dMoved ~ Condition+ (Condition|Subject),round_info1)

anova(m1,m2)

summary(m1)
anova(m1)
summary(m2)
anova(m2)

m1 = lmer(Q1 ~ Condition  + (1|Subject),round_info_withQ)
m2 = lmer(Q1 ~ Condition  + (Condition|Subject),round_info_withQ)
# 
anova(m1,m2)

summary(m1)
anova(m1)
summary(m2)
anova(m2)



###################### Wilcox 

sub_indivdata = subset(indivdata, Subject != 20 )
sub_indivdata = indivdata

wilcox.test(sub_indivdata$dError[sub_indivdata$Condition=='Body'],sub_indivdata$dError[sub_indivdata$Condition=='No-body'],paired=T)
wilcox.test(sub_indivdata$distNorm[sub_indivdata$Condition=='Body'],sub_indivdata$distNorm[sub_indivdata$Condition=='No-body'],paired=T)

temp_body = subset(sub_indivdata, Condition=='Body')
temp_Nobody = subset(sub_indivdata, Condition=='No-body')

wilcox.test(temp_body$distNorm,temp_Nobody$distNorm,paired=T)

# indivdata$dError[indivdata$Condition=='Body']
# indivdata$dError[indivdata$Condition=='No-body']
wilcox.test(1/indivdata$dError[indivdata$Condition=='Body'],1/indivdata$dError[indivdata$Condition=='No-body'],paired=T)

###################### repeated measures ANOVA

am1 <- aov(distNorm ~ Condition + Subject, data=indivdata)
summary(am1)

am2 <- aov(distNorm ~ Condition + Error(Subject/Condition), data=indivdata)
summary(am2)

######################repeated measures ANOVA

# am1 <- aov(dError ~ Condition + Subject, data=indivdata_mean)
# summary(am1)
# 
# am2 <- aov(dError ~ Condition + Error(Subject/Condition), data=indivdata_mean)
# summary(am2)
# 
# am3 <- aov(dError ~ Condition + Error(Subject/Condition), data=indivdata_excl_mean)
# summary(am3)


##################

ggplot(indiv_mean_sd, aes(y=dError_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=dError_mean-dError_se,ymax=dError_mean+dError_se), size = .3, width=.2,position=position_dodge(.9)) + ylim(0,20)

ggplot(indiv_mean_sd, aes(y=distNorm_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=distNorm_mean-distNorm_se,ymax=distNorm_mean+distNorm_se), size = .3, width=.2,position=position_dodge(.9)) #+ ylim(-0.25,0.25)

ggplot(indiv_median_mean_sd, aes(y=distNorm_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=distNorm_mean-distNorm_se,ymax=distNorm_mean+distNorm_se), size = .3, width=.2,position=position_dodge(.9)) #+ ylim(-0.25,0.25)

# time
ggplot(indiv_mean_sd, aes(y=TimeSpent_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=TimeSpent_mean-TimeSpent_se,ymax=TimeSpent_mean+TimeSpent_se), size = .3, width=.2,position=position_dodge(.9)) + ylim(0,20)
ggplot(round_mean_sd1, aes(y=EN_TimeSpent_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=EN_TimeSpent_mean-EN_TimeSpent_se,ymax=EN_TimeSpent_mean+EN_TimeSpent_se), size = .3, width=.2,position=position_dodge(.9)) + ylim(0,250)


#distanceMoved
ggplot(indiv_mean_sd, aes(y=dMoved_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=dMoved_mean-dMoved_se,ymax=dMoved_mean+dMoved_se), size = .3, width=.2,position=position_dodge(.9)) + coord_cartesian(ylim=c(75,100))
ggplot(round_mean_sd1, aes(y=EN_dMoved_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=EN_dMoved_mean-EN_dMoved_se,ymax=EN_dMoved_mean+EN_dMoved_se), size = .3, width=.2,position=position_dodge(.9)) + coord_cartesian(ylim=c(700,1100))

#No. of directionChange
ggplot(indiv_mean_sd, aes(y=directionChange_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=directionChange_mean-directionChange_se,ymax=directionChange_mean+directionChange_se), size = .3, width=.2,position=position_dodge(.9)) + coord_cartesian(ylim=c(0,4))
ggplot(round_mean_sd1, aes(y=EN_nDirectionChange_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=EN_nDirectionChange_mean-EN_nDirectionChange_se,ymax=EN_nDirectionChange_mean+EN_nDirectionChange_se), size = .3, width=.2,position=position_dodge(.9)) + coord_cartesian(ylim=c(0,45))

#No. of manipulation
ggplot(indiv_mean_sd1, aes(y=nManipulation_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=nManipulation_mean-nManipulation_se,ymax=nManipulation_mean+nManipulation_se), size = .3, width=.2,position=position_dodge(.9)) + coord_cartesian(ylim=c(0,6.5))
ggplot(indiv_mean_sd2, aes(y=nManipulation_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=nManipulation_mean-nManipulation_se,ymax=nManipulation_mean+nManipulation_se), size = .3, width=.2,position=position_dodge(.9)) + coord_cartesian(ylim=c(0,6.5))
ggplot(round_mean_sd1, aes(y=EN_nManipulation_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=EN_nManipulation_mean-EN_nManipulation_se,ymax=EN_nManipulation_mean+EN_nManipulation_se), size = .3, width=.2,position=position_dodge(.9)) + coord_cartesian(ylim=c(0,80))
ggplot(round_mean_sd2, aes(y=EN_nManipulation_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=EN_nManipulation_mean-EN_nManipulation_se,ymax=EN_nManipulation_mean+EN_nManipulation_se), size = .3, width=.2,position=position_dodge(.9)) + coord_cartesian(ylim=c(0,80))


ggplot(indivdata1, aes(y=dError, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=2)+ coord_cartesian(ylim=c(0,40))#+ facet_wrap(~Subject) 
ggplot(indivdata2, aes(y=dError, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=2)+ coord_cartesian(ylim=c(0,40))#+ facet_wrap(~Subject) 

################ correlations #########

ggplot(a, aes(y=dError, x=TimeSpent, color=Condition)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(a, aes(y=dError, x=dMoved, color=Condition)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")

cor.test(~dError + dMoved, data = a, method ="spearman", continuity = TRUE, conf.level = 0.95)

m1 = lmer(dError ~ dMoved + (1|Subject),round_info1)
m2 = lmer(dError ~ dMoved + (EN_TimeSpent|Subject),round_info1)
anova(m1,m2)
summary(m1)
anova(m1)
summary(m2)
anova(m2)

ggplot(round_info1, aes(y=dError, x=EN_TimeSpent, color=Condition)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(round_info1, aes(y=dError, x=EN_nDirectionChange, color=Condition)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(round_info1, aes(y=dError, x=EN_nManipulation, color=Condition)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
cor.test(~EN_TimeSpent + dError, data = round_info1, method ="spearman", continuity = FALSE, conf.level = 0.95)
ggplot(round_mean_sd, aes(y=EN_TimeSpent_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=EN_TimeSpent_mean-EN_TimeSpent_se,ymax=EN_TimeSpent_mean+EN_TimeSpent_se), size = .3, width=.2,position=position_dodge(.9)) 
ggplot(round_mean_sd, aes(y=EN_dMoved_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=EN_dMoved_mean-EN_dMoved_se,ymax=EN_dMoved_mean+EN_dMoved_se), size = .3, width=.2,position=position_dodge(.9)) 
ggplot(round_mean_sd, aes(y=EN_nDirectionChange_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=EN_nDirectionChange_mean-EN_nDirectionChange_se,ymax=EN_nDirectionChange_mean+EN_nDirectionChange_se), size = .3, width=.2,position=position_dodge(.9)) 
ggplot(round_mean_sd, aes(y=EN_nManipulation_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=EN_nManipulation_mean-EN_nManipulation_se,ymax=EN_nManipulation_mean+EN_nManipulation_se), size = .3, width=.2,position=position_dodge(.9)) 

# metacognition a$ErrorMetaCog = a$dError - a$estimation_dError

ggplot(indivdata1, aes(y=estimation_dError, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=2)#+ facet_wrap(~Subject) 
ggplot(indivdata2, aes(y=estimation_dError, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=2)#+ facet_wrap(~Subject) 

# temp <- merge(indiv_mean_sd[,c(1,3,8)],indiv_mean_sd[,c(1,11,16)])
meta_mean_sd <-  read_csv("E:/June/Memory/MetaData_mean_excl.csv")
meta_mean_sd1 <- subset(meta_mean_sd,Study==1)
meta_mean_sd2 <- subset(meta_mean_sd,Study==2)

ggplot(meta_mean_sd1, aes(y=dError_mean, x=Condition, fill=Actual_vs_Meta)) + geom_bar(stat="identity",width=.6,position=position_dodge(.7)) + geom_errorbar(aes(ymin=dError_mean-dError_se,ymax=dError_mean+dError_se), size = .3, width=.2,position=position_dodge(.7))  + coord_cartesian(ylim=c(0,20))
ggplot(meta_mean_sd2, aes(y=dError_mean, x=Condition, fill=Actual_vs_Meta)) + geom_bar(stat="identity",width=.6,position=position_dodge(.7)) + geom_errorbar(aes(ymin=dError_mean-dError_se,ymax=dError_mean+dError_se), size = .3, width=.2,position=position_dodge(.7))  + coord_cartesian(ylim=c(0,20))

# ggplot(indiv_mean_sd, aes(y=dError_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6,position=position_dodge(.9)) + geom_errorbar(aes(ymin=dError_mean-dError_se,ymax=dError_mean+dError_se), size = .3, width=.2,position=position_dodge(.9)) 
ggplot(indiv_mean_sd1, aes(y=estimation_dError_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6,position=position_dodge(.9)) + geom_errorbar(aes(ymin=estimation_dError_mean-estimation_dError_se,ymax=estimation_dError_mean+estimation_dError_se), size = .3, width=.2,position=position_dodge(.9)) + coord_cartesian(ylim=c(0,8))
ggplot(indiv_mean_sd2, aes(y=estimation_dError_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6,position=position_dodge(.9)) + geom_errorbar(aes(ymin=estimation_dError_mean-estimation_dError_se,ymax=estimation_dError_mean+estimation_dError_se), size = .3, width=.2,position=position_dodge(.9)) + coord_cartesian(ylim=c(0,8))

ggplot(indiv_mean_sd1, aes(y=ErrorMetaCog_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6,position=position_dodge(.9)) + geom_errorbar(aes(ymin=ErrorMetaCog_mean-ErrorMetaCog_se,ymax=ErrorMetaCog_mean+ErrorMetaCog_se), size = .3, width=.2,position=position_dodge(.9))
ggplot(indiv_mean_sd2, aes(y=ErrorMetaCog_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6,position=position_dodge(.9)) + geom_errorbar(aes(ymin=ErrorMetaCog_mean-ErrorMetaCog_se,ymax=ErrorMetaCog_mean+ErrorMetaCog_se), size = .3, width=.2,position=position_dodge(.9))
# ggplot(indiv_mean_sd, aes(y=ErrorMetaCog_ratio_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6,position=position_dodge(.9)) + geom_errorbar(aes(ymin=ErrorMetaCog_ratio_mean-ErrorMetaCog_ratio_se,ymax=ErrorMetaCog_ratio_mean+ErrorMetaCog_ratio_se), size = .3, width=.2,position=position_dodge(.9))


ggplot(a, aes(y=dError, x=estimation_dError, color=Condition)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA") + geom_abline(slope=1,linetype='dashed') + xlim(0,100) + ylim(0,100) + ylab("Actual error (m)") + xlab('Estimated error (m)') #+ facet_wrap(~Subject)

cor.test(~estimation_dError + dError, data = a, method ="spearman", continuity = FALSE, conf.level = 0.95)


###
m1 = lmer(ErrorMetaCog ~ Condition2 + (1|Subject),a1)
m2 = lmer(ErrorMetaCog ~ Condition2 + (1|Subject),a2)

# anova(m1,m2)

summary(m1)
anova(m1)
summary(m2)
anova(m2)


########### do the same thing, with mixed models, taking individual trials into account
# a2$rank = rank(a2$dError)
# #a$perform_rank=rank(-1/a$err)
# 
# m1 = lmer(dError ~ Condition +(1|Subject),a2)
# m2 = lmer(dError ~ Condition +(Condition|Subject),a2)
# anova(m1,m2)
# 
# summary(m1)
# anova(m1)
# 
# plot(fitted(m1),residuals(m1))
# 
# hist(residuals(m1))
# qqnorm(residuals(m1))
# 
# summary(m2)
# anova(m2)
# coef(m2)

## Questionnaire

indivQ = aggregate(cbind(Q1z,Q2z,Q3z,Q4z) ~ Condition + Subject,a,mean)
indivQ =  gather(indivQ,Quest,rating,Q1z:Q4z)
indivQ$Quest2 = ifelse(indivQ$Quest == 'Q1z', 'Q1',ifelse(indivQ$Quest == 'Q2z', 'Q2',ifelse(indivQ$Quest == 'Q3z', 'Q3','Q4')))

p <- ggplot(indivQ, aes(y=rating, x=Quest2, color = Condition)) + geom_boxplot() + labs(x = "", y = "Ratings (Z-scored)") #+ geom_jitter(width=.2,height=0,alpha=.5,size=3) #facet_wrap(~Subject)
# ggplot(indivQ, aes(y=rating, x=Quest, color = Condition)) + geom_boxplot() + facet_wrap(~Subject)

p + theme(
  #plot.title = element_text(color="red", size=14, face="bold.italic"),
  axis.title.x = element_text(size=0, face="bold"),
  axis.title.y = element_text(size=22, face="bold"),
  axis.text = element_text(size=20),
  legend.text = element_text(size=22),
  legend.title = element_text(size=18)
)

## look at correlations between err and questions

# medquest1 = aggregate(cbind(dError,distNorm,Q1,Q2,Q3,Q4) ~ Condition + Subject, a, median)
# meanquest1 = aggregate(cbind(dError,distNorm,Q1,Q2,Q3,Q4) ~ Condition + Subject, a, mean)
# 
# temp1_med <- subset(medquest1, select=cbind(Condition,dError,distNorm,Q1,Q2,Q3,Q4))
# temp1_mean <- subset(meanquest1, select=cbind(Condition,dError,distNorm,Q1,Q2,Q3,Q4))
# 
# Q1_med_mean_sd <- temp1_med %>% group_by(Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
# Q1_mean_mean_sd <- temp1_mean %>% group_by(Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

# indivdata1$Q1r = rank(indivdata1$Q1)
# indivdata1$Q2r = rank(indivdata1$Q2)
# indivdata1$Q3r = rank(indivdata1$Q3)
# indivdata1$Q4r = rank(indivdata1$Q4)
# 
# indivdata_mean1$Q1r = rank(indivdata_mean1$Q1)
# indivdata_mean1$Q2r = rank(indivdata_mean1$Q2)
# indivdata_mean1$Q3r = rank(indivdata_mean1$Q3)
# indivdata_mean1$Q4r = rank(indivdata_mean1$Q4)
# 
# rounddata_mean_withQ$Q1r = rank(rounddata_mean_withQ$Q1)
# rounddata_mean_withQ$Q2r = rank(rounddata_mean_withQ$Q2)
# rounddata_mean_withQ$Q3r = rank(rounddata_mean_withQ$Q3)
# rounddata_mean_withQ$Q4r = rank(rounddata_mean_withQ$Q4)
# 
# rounddata_median_withQ$Q1r = rank(rounddata_median_withQ$Q1)
# rounddata_median_withQ$Q2r = rank(rounddata_median_withQ$Q2)
# rounddata_median_withQ$Q3r = rank(rounddata_median_withQ$Q3)
# rounddata_median_withQ$Q4r = rank(rounddata_median_withQ$Q4)
####

Q_med = aggregate(rating ~ Condition + Quest, indivQ, median)
Q_mean = aggregate(rating ~ Condition + Quest, indivQ, mean)

Questionnaire_mean <- indivQ %>% group_by(Condition,Quest) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

# ggplot(Questionnaire_mean1,aes(x=Quest,y=rating_mean,color=Condition,fill=Condition)) +
#   geom_bar(color='black',width=.5,stat = 
#              "identity",position=position_dodge(width=.5)) +
#   scale_fill_brewer(palette='Set2')
# scale_fill_manual(values=c('red','blue'))
# + geom_boxplot() 
# +  scale_fill_brewer(palette='Set2')
# scale_fill_manual(values=c('red','blue'))

ggplot(Questionnaire_mean, aes(y=rating_mean, x=Quest,fill=Condition)) + geom_bar(stat="identity",width=.6,position=position_dodge(.7)) + geom_errorbar(aes(ymin=rating_mean-rating_se,ymax=rating_mean+rating_se), size = .3, width=.2,position=position_dodge(.7)) + ylim(-1.3,1.3) +
  ylab("Rating (Z-scored)") + xlab('Questionnaire')


m1 = lmer(Q2z ~ Condition + (1|Subject),rounddata_withQ)
summary(m1)
anova(m1)

m1 = lmer(Q4z ~ Condition + (1|Subject),indivdata1)
summary(m1)
anova(m1)

# wilcox.test(medquest$Q1[medquest$Condition=='Body'],medquest$Q1[medquest$Condition=='No-body'],paired=T)
# wilcox.test(medquest$Q2[medquest$Condition=='Body'],medquest$Q2[medquest$Condition=='Object'],paired=T)

##################
##### Correlations
rounddata_median1

ggplot(a, aes(y=dError, x=Q2z, color=Condition)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata1, aes(y=dError, x=Q2z, color=Condition)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata1, aes(y=dError, x=Q2z, color=Condition)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")

ggplot(rounddata_median_withQ, aes(y=dError, x=Q2, color=Condition)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(rounddata_mean_withQ, aes(y=distNorm, x=Q2, color=Condition)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")

ggplot(a, aes(y=dError, x=Q2, color=Condition)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata1, aes(y=dError, x=Q2, color=Condition)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(rounddata_median_withQ, aes(y=dError, x=Q2, color=Condition)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(rounddata_mean_withQ, aes(y=dError, x=Q2, color=Condition)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")

ggplot(a1, aes(y=dError, x=Q4)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata1, aes(y=dError, x=Q4)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")

ggplot(a2, aes(y=dError, x=Q4)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata2, aes(y=dError, x=Q4)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")

ggplot(a_merge, aes(y=dError, x=Q4)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata_merge, aes(y=dError, x=Q4)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(medquest_merge, aes(y=dError, x=Q4r)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")

ggplot(a1, aes(y=distNorm, x=Q4)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
# ggplot(indivdata1, aes(y=distNorm, x=Q1)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(medquest1, aes(y=distNorm, x=Q4r)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")

ggplot(a2, aes(y=distNorm, x=Q4)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
# ggplot(indivdata2, aes(y=distNorm, x=Q1)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(medquest2, aes(y=distNorm, x=Q4r)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")

ggplot(a_merge, aes(y=distNorm, x=Q4)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
# ggplot(indivdata_merge, aes(y=distNorm, x=Q1)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(medquest_merge, aes(y=distNorm, x=Q4r)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")

# temp <- merge(indivdata_mean,scr_indv)
# ggplot(temp, aes(y=Q2, x=Q3, color=Condition)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")
# 
# 
cor.test(~Q3 + dError, data = medquest_merge, method ="spearman", continuity = FALSE, conf.level = 0.95)

ggplot(a1, aes(y=dError, x=dMoved)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata1, aes(y=dError, x=dMoved)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(a1, aes(y=distNorm, x=dMoved)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata1, aes(y=distNorm, x=dMoved)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(round_info1, aes(y=dError, x=EN_dMoved)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")

ggplot(a2, aes(y=dError, x=dMoved)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata2, aes(y=dError, x=dMoved)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(a2, aes(y=distNorm, x=dMoved)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata2, aes(y=distNorm, x=dMoved)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(round_info2, aes(y=dError, x=EN_dMoved)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")

ggplot(a1, aes(y=dError, x=directionChange)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata1, aes(y=dError, x=dMoved)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(a1, aes(y=distNorm, x=dMoved)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata1, aes(y=distNorm, x=dMoved)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(round_info1, aes(y=dError, x=EN_nDirectionChange)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")

ggplot(a2, aes(y=dError, x=dMoved)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata2, aes(y=dError, x=dMoved)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(a2, aes(y=distNorm, x=dMoved)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(indivdata2, aes(y=distNorm, x=dMoved)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")
ggplot(round_info2, aes(y=dError, x=EN_nDirectionChange)) + geom_point(alpha=.7,size=2) + geom_smooth(method="lm", se = FALSE, fill = "NA")

###

m1 = lmer(dError ~ Q2z + (1|Subject),rounddata_median_withQ)
anova(m1)
summary(m1)

m1 = lmer(dMoved ~ Q3 +(1|Subject),indivdata1)
anova(m1)
summary(m1)

m1 = lmer(dError ~ Q2z + (1|Subject),indivdata1)
anova(m1)
summary(m1)


m1 = lmer(dError ~ TimeSpent + (1|Subject),indivdata2)
anova(m1)
summary(m1)

m2 = lmer(dError ~ EN_TimeSpent+ (1|Subject),round_info1)
anova(m2)
summary(m2)

m1 = lmer(Q4 ~ Q2 + (1|Subject),medquest_merge)
anova(m1)
summary(m1)

m2 = lmer(dError ~ Q2z + Condition + (1|Subject),indivdata1)
anova(m2)
summary(m2)

m2 = lmer(distNorm ~ Q2z + Condition + (1|Subject),indivdata1)
anova(m2)
summary(m2)


# 
# ggplot(medquest, aes(y=Q1, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=2)#+ facet_wrap(~Subject) 
# 
# ggplot(medquest, aes(y=Q2, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=2)#+ facet_wrap(~Subject) 
# 
# ggplot(medquest, aes(y=Q3, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=2)#+ facet_wrap(~Subject) 
# 
# ggplot(medquest, aes(y=Q4, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=2)#+ facet_wrap(~Subject) 
# 
# ggplot(medquest, aes(y=Q5, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=2)#+ facet_wrap(~Subject) 
# 
# round_info$Condition <- factor(round_info$Condition)
# ggplot(round_info,aes(x=EN_dMoved, y=dError)) + geom_point() + geom_smooth(method='lm') #+ facet_wrap(~Subject)

ggplot(medquest,aes(x=1/dError,y=Q1,color=Condition,fill=Condition)) + geom_point() + geom_smooth(method='lm')
ggplot(medquest,aes(x=1/dError,y=Q1)) + geom_point() + geom_smooth(method='lm')
ggplot(medquest,aes(x=Q2,y=Q1,color=Condition,fill=Condition)) + geom_jitter(width=.05,height=.05,alpha=.7,size=1) + geom_smooth(method='lm')


summary(lm(medquest$dError[medquest$Condition=='Body'] ~ medquest$Q3[medquest$Condition=='Body']))
summary(lm(round_info$dError ~ round_info$EN_dMoved))

m1 = lmer(dError ~ EN_dMoved*Condition+ (1|Subject),round_info)
anova(m1)
summary(m1)




## compute linear regression between error and metacognition
ggplot(a,aes(x=estimation_dError,y=dError,color=Condition,fill=Condition)) + geom_point() + geom_smooth(method='lm') +
  geom_abline(slope=1,linetype='dashed') + xlim(0,100) + ylim(0,100) +
  ylab("Actual error (m)") + xlab('Estimated error (m)') #+ facet_wrap(~Subject)

m1=lmer(dError~estimation_dError * Condition + (1|Subject),data = a)
m2=lmer(dError~estimation_dError * Condition + (estimation_dError * Condition|Subject),data = a)
anova(m2)
summary(m1)
## permutation test
reps=1000
a$var=a$TimeSpent
bodyError= a$var[a$Condition=='Body']
objectError = a$var[a$Condition=='Object']

realdiff=mean(bodyError) - mean(objectError)
permdiff=c()
for (r in 1:reps) {
  randobject=sort(sample(length(bodyError), 
                         round(length(bodyError)/2),replace=F))
  randbody=which(!is.element(seq(1,length(bodyError),1),randobject))
  permdiff[r]=mean(a$var[randobject])-mean(a$var[randbody])
}
diffs=data.frame(permdiff,realdiff)
ggplot(diffs,aes(x=permdiff,y=..density..)) + geom_density() + 
  geom_vline(xintercept = realdiff,linetype='dashed') +
  ggtitle(paste("p =",round(length(which(realdiff>permdiff))/reps,2)))

## Skin conductance
scr = read_csv("D:/Experiment Data/Memory and Self/Behavioral Study/Main/SCR_0818.csv")

scr$Subject = factor(scr$Subject)
scr$Condition = ifelse(scr$Condition == 1, 'No-body',ifelse(scr$Condition == 2, 'Body','Object'))
scr$Order = factor(scr$Order)

scr_excl <- subset(scr,SCR > 0)

scr_indv = aggregate(cbind(SCR) ~ Condition + Order+ Subject,scr,mean)
scr_indv_excl = aggregate(cbind(SCR) ~ Condition + Order+ Subject,scr_excl,mean)

scr_mean_sd <- scr_indv %>% group_by(Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
scr_mean_sd_excl<- scr_indv_excl %>% group_by(Condition) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

scr_mean_sd_order <- scr_indv %>% group_by(Order) %>% summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))

ggplot(scr_indv, aes(y=SCR, x=Condition,color=Condition))  + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=2)#+ facet_wrap(~Subject) 
ggplot(scr_indv, aes(y=SCR, x=Order,color=Order))  + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=2)#+ 


ggplot(scr_mean_sd, aes(y=SCR_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6,position=position_dodge(.9)) + geom_errorbar(aes(ymin=SCR_mean-SCR_se,ymax=SCR_mean+SCR_se), size = .3, width=.2,position=position_dodge(.9))

ggplot(scr_mean_sd_order, aes(y=SCR_mean, x=Order,fill=Order)) + geom_bar(stat="identity",width=.6,position=position_dodge(.9)) + geom_errorbar(aes(ymin=SCR_mean-SCR_se,ymax=SCR_mean+SCR_se), size = .3, width=.2,position=position_dodge(.9))


m1 = lmer(SCR ~ Order + (1|Subject),scr)
m2 = lmer(SCR ~ Order + (Order|Subject),scr)

anova(m1,m2)

summary(m2)
anova(m2)

summary(m1)
anova(m1)

wilcox.test(scr_indv$SCR[scr_indv$Condition=='Body'],scr_indv$SCR[scr_indv$Condition=='Object'],paired=T)
wilcox.test(scr_indv$SCR[scr_indv$Condition=='Body'],scr_indv$SCR[scr_indv$Condition=='No-body'],paired=T)

ggplot(subset(scr,Condition != 'Object'), aes(y=SCR, x=Condition, color=Subject)) + geom_jitter(width=.05,height=0,alpha=.7,size=0.3) + geom_smooth(aes(group = Subject), method="lm", se = FALSE, fill = "NA")

ggplot(subset(scr,Condition != 'No-body'), aes(y=SCR, x=Condition, color=Subject)) + geom_jitter(width=.05,height=0,alpha=.7,size=0.3) + geom_smooth(aes(group = Subject), method="lm", se = FALSE, fill = "NA")

ggplot(scr, aes(y=SCR, x=Condition, color=Subject)) + geom_jitter(width=.05,height=0,alpha=.7,size=0.3) + geom_smooth(aes(group = Subject), method="lm", se = FALSE, fill = "NA")

temp <- merge(indivdata,scr_indv)
ggplot(temp, aes(y=dError, x=SCR, color=Condition)) + geom_point(alpha=.7,size=1) + geom_smooth(method="lm", se = FALSE, fill = "NA")

cor.test(~SCR + dError, data = temp, method ="spearman", continuity = TRUE, conf.level = 0.95)


########### code bakup

# 
# ggplot(indiv_mean_sd, aes(y=nManipulation_mean, x=Condition,fill=Condition)) + geom_bar(stat="identity",width=.6) + geom_errorbar(aes(ymin=nManipulation_mean-nManipulation_se,ymax=nManipulation_mean+nManipulation_se), size = .3, width=.2,position=position_dodge(.9)) 
# 
# temp <- merge(indivdata_TA_mean,indivdata)
# 
# ggplot(a, aes(y=dError, x=nManipulation, color=Condition)) + geom_point(alpha=.7,size=0.3) + geom_smooth(method="lm", se = FALSE, fill = "NA")
# 
# cor.test(~nManipulation + dError, data = a, method ="spearman", continuity = FALSE, conf.level = 0.95)
# 
# 
# ggplot(indivdata, aes(y=dMoved, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=2)#+ facet_wrap(~Subject) 
# ggplot(indivdata, aes(y=TimeSpent, x=Condition,color=Condition))  + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=2)#+ facet_wrap(~Subject) 
# ggplot(indivdata, aes(y=directionChange, x=Condition,color=Condition))  + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=2)#+ facet_wrap(~Subject) 
# ggplot(indivdata, aes(y=nManipulation, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=2)#+ facet_wrap(~Subject) 
# ggplot(indivdata, aes(y=estimation_dError, x=Condition,color=Condition)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=2)#+ facet_wrap(~Subject) 
# ggplot(indivdata, aes(y=ErrorMetaCog, x=Condition,color=Condition))  + geom_boxplot(outlier.shape = NA) + geom_jitter(width=.1,height=0,alpha=.5,size=2)#+ facet_wrap(~Subject) 
# 
# indivdata$Condition2 <- ifelse(indivdata$Condition=='No-body','aNo-body',indivdata$Condition)
# ggplot(indivdata, aes(y=1/dError, x=Condition,color=Condition, group =Subject)) +  geom_line(color="black") + geom_point() 
# 
# ### Counting
# effect_count = 0
# for (i in 7:32)
# {
#   if(i != 14)
#   {
#     tmp_body <- subset(indivdata, (Subject == i & Condition == 'Body'))
#     tmp_object <- subset(indivdata, (Subject == i & Condition == 'No-body'))
#     if(tmp_body$dError < tmp_object$dError)
#     {
#       effect_count = effect_count + 1
#     }
#   }
# }
# 
# 
# ########### run a ttest on the err median between body and object
# t.test(indivdata$dError[indivdata$Condition=='Body'],indivdata$dError[indivdata$Condition=='Object'],paired=T)
# wilcox.test(indivdata$dError[indivdata$Condition=='Body'],indivdata$dError[indivdata$Condition=='Object'],paired=T)
# 
# t.test(1/indivdata$dError[indivdata$Condition=='Body'],1/indivdata$dError[indivdata$Condition=='Object'],paired=T)
# wilcox.test(1/indivdata$dError[indivdata$Condition=='Body'],1/indivdata$dError[indivdata$Condition=='Object'],paired=T)
